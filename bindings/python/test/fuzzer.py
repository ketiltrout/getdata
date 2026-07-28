# Copyright (C) 2026 G. Smecher
#
##########################################################################
#
# This file is part of the GetData project.
#
# GetData is free software; you can redistribute it and/or modify it under
# the terms of the GNU Lesser General Public License as published by the
# Free Software Foundation; either version 2.1 of the License, or (at your
# option) any later version.
#
# GetData is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
# License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with GetData; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
#

"""
Fuzzing tests for the GetData Python bindings.

Requires hypothesis; the module skips itself if hypothesis is unavailable so
that it can be wired into CTest unconditionally.
"""

import numpy
import os
import shutil
import sys
import tempfile

try:
    import pygetdata as gd
    import pytest
    from hypothesis import assume, given, note, settings, strategies as st
    from hypothesis import HealthCheck
    from hypothesis.stateful import (
        RuleBasedStateMachine, initialize, invariant, precondition, rule,
    )
except ImportError as ex:  # pragma: no cover
    sys.stderr.write("%s; skipping fuzzer\n" % ex)
    sys.exit(77)  # autotools/CTest convention for "skipped"

fuzzer = settings(
    max_examples=int(os.environ.get("GD_FUZZER_EXAMPLES", "50")),
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)

# ---------------------------------------------------------------------------
# Type model
#
# spf is deliberately kept small: num_frames reads multiply by it, and large
# products make examples slow without exercising new code paths.
# ---------------------------------------------------------------------------

INT_TYPES = {
    gd.INT8: (-(2**7), 2**7 - 1),
    gd.UINT8: (0, 2**8 - 1),
    gd.INT16: (-(2**15), 2**15 - 1),
    gd.UINT16: (0, 2**16 - 1),
    gd.INT32: (-(2**31), 2**31 - 1),
    gd.UINT32: (0, 2**32 - 1),
    gd.INT64: (-(2**63), 2**63 - 1),
    gd.UINT64: (0, 2**64 - 1),
}

# Types whose values survive a round trip exactly.  float32 is excluded from
# the exact-equality tests because generating a float64 payload and storing it
# as float32 loses precision; it is covered by test_float32_roundtrip instead.
EXACT_TYPES = sorted(INT_TYPES) + [gd.FLOAT64, gd.COMPLEX128]

NUMPY_TYPE = {
    gd.INT8: numpy.int8, gd.UINT8: numpy.uint8,
    gd.INT16: numpy.int16, gd.UINT16: numpy.uint16,
    gd.INT32: numpy.int32, gd.UINT32: numpy.uint32,
    gd.INT64: numpy.int64, gd.UINT64: numpy.uint64,
    gd.FLOAT32: numpy.float32, gd.FLOAT64: numpy.float64,
    gd.COMPLEX64: numpy.complex64, gd.COMPLEX128: numpy.complex128,
}

# Encodings that support writing.  SLIM_ENCODED and ZZIP_ENCODED are read-only
# (alter_encoding raises UnsupportedError), so they cannot appear in a
# write-then-read property.  The compressed encodings need in-process library
# support (libbz2, liblzma, ...) compiled in: an external bzip2/gzip binary is
# not enough for gd_add().  A build missing one is a broken build environment,
# not something for this test to paper over.
WRITABLE_ENCODINGS = [
    gd.UNENCODED, gd.GZIP_ENCODED, gd.BZIP2_ENCODED, gd.LZMA_ENCODED,
    gd.TEXT_ENCODED, gd.SIE_ENCODED,
]

# Field codes.  The Dirfile standard forbids several characters in field names
# (/, &, ;, <, >, |, and whitespace among them) and reserves the name INDEX;
# rather than encode all of that here, generate conservative identifiers.  Name
# validity is already covered exhaustively by test/add_*.c.
FIELD_CODES = st.from_regex(r"\A[A-Za-z][A-Za-z0-9_]{0,7}\Z").filter(
    lambda s: s != "INDEX"
)


@st.composite
def raw_field(draw, types=None):
    """A RAW field description: (code, type, spf)."""
    return (
        draw(FIELD_CODES),
        draw(st.sampled_from(EXACT_TYPES if types is None else types)),
        draw(st.integers(min_value=1, max_value=8)),
    )


def payload(dtype, n):
    """A strategy for n exactly-representable samples of getdata type dtype."""
    if dtype in INT_TYPES:
        lo, hi = INT_TYPES[dtype]
        elements = st.integers(min_value=lo, max_value=hi)
    elif dtype in (gd.COMPLEX64, gd.COMPLEX128):
        # Keep magnitudes modest so that float32 storage (COMPLEX64) and the
        # text encoding both reproduce the value exactly.
        comp = st.integers(min_value=-(2**20), max_value=2**20)
        elements = st.builds(complex, comp, comp)
    else:
        elements = st.integers(min_value=-(2**20), max_value=2**20).map(float)
    return st.lists(elements, min_size=n, max_size=n).map(
        lambda v: numpy.array(v, dtype=NUMPY_TYPE[dtype])
    )


def decode(s):
    """Field codes and strings come back from the bindings as bytes."""
    return s.decode() if isinstance(s, bytes) else s


class Dirfile:
    """A dirfile in a scratch directory, cleaned up on exit."""

    def __init__(self, encoding=None):
        self.root = tempfile.mkdtemp(prefix="gd_torture_")
        self.path = os.path.join(self.root, "dirfile")
        self.D = gd.dirfile(self.path, gd.CREAT | gd.EXCL | gd.RDWR)
        if encoding is not None and encoding != gd.UNENCODED:
            self.D.fragment(0).alter_encoding(encoding, recode=1)

    def reopen(self, mode=gd.RDONLY):
        """Close the current handle and return a freshly parsed one."""
        self.D.close()
        self.D = gd.dirfile(self.path, mode)
        return self.D

    def __enter__(self):
        return self

    def __exit__(self, *exc):
        try:
            self.D.close()
        except Exception:
            pass
        shutil.rmtree(self.root, ignore_errors=True)


# ---------------------------------------------------------------------------
# Round-trip properties
# ---------------------------------------------------------------------------

@fuzzer
@given(field=raw_field(), data=st.data())
def test_roundtrip_same_handle(field, data):
    """putdata() then getdata() on the *writing* handle returns the input.
    """
    code, dtype, spf = field
    values = data.draw(payload(dtype, spf * data.draw(
        st.integers(min_value=1, max_value=6))))

    with Dirfile() as df:
        df.D.add(gd.entry(gd.RAW_ENTRY, code, 0, dict(type=dtype, spf=spf)))
        assert df.D.putdata(code, values) == len(values)
        df.D.flush()

        out = df.D.getdata(code, dtype, first_frame=0, num_samples=len(values))
        assert len(out) == len(values), (
            "short read on writing handle: got %d of %d samples (error=%r)"
            % (len(out), len(values), df.D.error)
        )
        assert numpy.array_equal(out, values)


@fuzzer
@given(field=raw_field(), data=st.data())
def test_roundtrip_reopened(field, data):
    """Data written, then read back through a freshly parsed handle."""
    code, dtype, spf = field
    values = data.draw(payload(dtype, spf * data.draw(
        st.integers(min_value=1, max_value=6))))

    with Dirfile() as df:
        df.D.add(gd.entry(gd.RAW_ENTRY, code, 0, dict(type=dtype, spf=spf)))
        df.D.putdata(code, values)
        D = df.reopen()

        out = D.getdata(code, dtype, first_frame=0, num_samples=len(values))
        assert numpy.array_equal(out, values)
        assert D.nframes == len(values) // spf
        assert D.spf(code) == spf


@fuzzer
@given(field=raw_field(), data=st.data())
def test_same_handle_agrees_with_reopened(field, data):
    """A read on the writing handle agrees with one on a reopened handle."""
    code, dtype, spf = field
    values = data.draw(payload(dtype, spf * data.draw(
        st.integers(min_value=1, max_value=6))))

    with Dirfile() as df:
        df.D.add(gd.entry(gd.RAW_ENTRY, code, 0, dict(type=dtype, spf=spf)))
        df.D.putdata(code, values)
        df.D.flush()

        before = df.D.getdata(code, dtype, first_frame=0,
                              num_samples=len(values))
        after = df.reopen().getdata(code, dtype, first_frame=0,
                                    num_samples=len(values))
        assert numpy.array_equal(before, after)


@fuzzer
@given(
    field=raw_field(),
    encoding=st.sampled_from(WRITABLE_ENCODINGS),
    data=st.data(),
)
def test_encoding_roundtrip(field, encoding, data):
    """Every writable encoding preserves the data it stores."""
    code, dtype, spf = field
    values = data.draw(payload(dtype, spf * data.draw(
        st.integers(min_value=1, max_value=4))))

    with Dirfile(encoding=encoding) as df:
        df.D.add(gd.entry(gd.RAW_ENTRY, code, 0, dict(type=dtype, spf=spf)))
        df.D.putdata(code, values)
        D = df.reopen()

        assert D.fragment(0).encoding == encoding
        out = D.getdata(code, dtype, first_frame=0, num_samples=len(values))
        assert numpy.array_equal(out, values)


@fuzzer
@given(field=raw_field(), data=st.data())
def test_partial_reads_tile(field, data):
    """Reading a field in arbitrary chunks reconstructs the whole field.

    Exercises the seek/offset arithmetic in _GD_Seek and _GD_DoRaw at
    boundaries a fixed test would have to enumerate by hand.
    """
    code, dtype, spf = field
    nframes = data.draw(st.integers(min_value=1, max_value=6))
    values = data.draw(payload(dtype, spf * nframes))

    with Dirfile() as df:
        df.D.add(gd.entry(gd.RAW_ENTRY, code, 0, dict(type=dtype, spf=spf)))
        df.D.putdata(code, values)
        D = df.reopen()

        # Split the sample range at a generated set of cut points.
        cuts = sorted(set(data.draw(st.lists(
            st.integers(min_value=0, max_value=len(values)),
            max_size=5))) | {0, len(values)})
        pieces = []
        for lo, hi in zip(cuts, cuts[1:]):
            if hi == lo:
                continue
            pieces.append(D.getdata(code, dtype, first_sample=lo,
                                    num_samples=hi - lo))
        joined = numpy.concatenate(pieces) if pieces else values[:0]
        note("cuts=%r" % (cuts,))
        assert numpy.array_equal(joined, values)


@fuzzer
@given(field=raw_field(), data=st.data())
def test_unbounded_read_returns_whole_field(field, data):
    """getdata() with no bounds returns the entire field.

    The commonest call in practice, and the one with the least explicit
    arithmetic behind it: with no first_frame/num_samples the library has to
    derive the extent itself.
    """
    code, dtype, spf = field
    values = data.draw(payload(dtype, spf * data.draw(
        st.integers(min_value=1, max_value=6))))

    with Dirfile() as df:
        df.D.add(gd.entry(gd.RAW_ENTRY, code, 0, dict(type=dtype, spf=spf)))
        df.D.putdata(code, values)

        # ...both before and after the format file is reparsed.
        assert numpy.array_equal(df.D.getdata(code, dtype), values)
        assert numpy.array_equal(df.reopen().getdata(code, dtype), values)


@fuzzer
@given(field=raw_field(), data=st.data())
def test_frame_and_sample_counts_agree(field, data):
    """Reading n frames is the same as reading n * spf samples."""
    code, dtype, spf = field
    nframes = data.draw(st.integers(min_value=1, max_value=6))
    values = data.draw(payload(dtype, spf * nframes))

    with Dirfile() as df:
        df.D.add(gd.entry(gd.RAW_ENTRY, code, 0, dict(type=dtype, spf=spf)))
        df.D.putdata(code, values)
        D = df.reopen()

        n = data.draw(st.integers(min_value=1, max_value=nframes))
        by_frame = D.getdata(code, dtype, first_frame=0, num_frames=n)
        by_sample = D.getdata(code, dtype, first_sample=0, num_samples=n * spf)
        assert numpy.array_equal(by_frame, by_sample)
        assert numpy.array_equal(by_frame, values[:n * spf])

        # The same equivalence holds for a non-zero starting frame.
        f0 = data.draw(st.integers(min_value=0, max_value=nframes - 1))
        assert numpy.array_equal(
            D.getdata(code, dtype, first_frame=f0, num_frames=nframes - f0),
            D.getdata(code, dtype, first_sample=f0 * spf,
                      num_samples=(nframes - f0) * spf))


@fuzzer
@given(field=raw_field(), data=st.data())
def test_read_past_eof_is_short_not_error(field, data):
    """Reading beyond the end of a field yields a short read, not an error."""
    code, dtype, spf = field
    values = data.draw(payload(dtype, spf * data.draw(
        st.integers(min_value=1, max_value=4))))

    with Dirfile() as df:
        df.D.add(gd.entry(gd.RAW_ENTRY, code, 0, dict(type=dtype, spf=spf)))
        df.D.putdata(code, values)
        D = df.reopen()

        overshoot = data.draw(st.integers(min_value=1, max_value=32))
        out = D.getdata(code, dtype, first_sample=0,
                        num_samples=len(values) + overshoot)
        assert len(out) <= len(values) + overshoot
        assert numpy.array_equal(out[:len(values)], values)
        assert D.eof(code) == len(values)


@fuzzer
@given(field=raw_field(types=[gd.FLOAT32]), data=st.data())
def test_float32_roundtrip(field, data):
    """FLOAT32 fields round-trip within float32 precision."""
    code, dtype, spf = field
    values = data.draw(payload(gd.FLOAT32, spf * data.draw(
        st.integers(min_value=1, max_value=4))))

    with Dirfile() as df:
        df.D.add(gd.entry(gd.RAW_ENTRY, code, 0, dict(type=dtype, spf=spf)))
        df.D.putdata(code, values)
        out = df.reopen().getdata(code, dtype, first_frame=0,
                                  num_samples=len(values))
        assert numpy.allclose(out, values, rtol=0, atol=0)


# ---------------------------------------------------------------------------
# Metadata / parser properties
# ---------------------------------------------------------------------------

@fuzzer
@given(field=raw_field())
def test_entry_survives_format_roundtrip(field):
    """An added RAW entry is unchanged by a format-file write/parse cycle."""
    code, dtype, spf = field

    with Dirfile() as df:
        df.D.add(gd.entry(gd.RAW_ENTRY, code, 0, dict(type=dtype, spf=spf)))
        before = df.D.entry(code)
        after = df.reopen().entry(code)

        assert after.field_type == before.field_type == gd.RAW_ENTRY
        assert decode(after.name) == decode(before.name) == code
        assert after.data_type == before.data_type == dtype
        assert after.spf == before.spf == spf


@fuzzer
@given(fields=st.lists(raw_field(), min_size=1, max_size=6,
                       unique_by=lambda f: f[0].lower()))
def test_field_list_matches_added_fields(fields):
    """field_list() reports exactly the RAW fields added, plus INDEX."""
    with Dirfile() as df:
        for code, dtype, spf in fields:
            df.D.add(gd.entry(gd.RAW_ENTRY, code, 0,
                              dict(type=dtype, spf=spf)))
        D = df.reopen()

        listed = {decode(f) for f in D.field_list()}
        expected = {code for code, _, _ in fields} | {"INDEX"}
        assert listed == expected
        assert D.nfields() == len(expected)


@fuzzer
@given(field=raw_field(), data=st.data())
def test_nframes_tracks_written_data(field, data):
    """nframes reflects the number of complete frames written."""
    code, dtype, spf = field
    nframes = data.draw(st.integers(min_value=1, max_value=8))
    values = data.draw(payload(dtype, spf * nframes))

    with Dirfile() as df:
        df.D.add(gd.entry(gd.RAW_ENTRY, code, 0, dict(type=dtype, spf=spf)))
        df.D.putdata(code, values)
        assert df.reopen().nframes == nframes


# ---------------------------------------------------------------------------
# Derived-field properties
#
# The arithmetic each derived field performs is already covered thoroughly by
# the C suite (get_lincom*.c, get_window_*.c, get_sbit.c, ... -- including every
# WINDOW operator and SBIT's sign extension).  What those tests do not vary is
# the *extent* of the read: nearly all of them read a single frame at frame 5.
# So these properties fix simple parameters and generate the offsets and
# lengths instead, which is the arithmetic the C tests leave pinned.
# ---------------------------------------------------------------------------

@st.composite
def extent(draw, nframes):
    """A (first_sample, num_samples) window inside a field of nframes."""
    first = draw(st.integers(min_value=0, max_value=nframes - 1))
    return first, draw(st.integers(min_value=1, max_value=nframes - first))


@fuzzer
@given(code=FIELD_CODES, data=st.data())
def test_polynom_over_generated_extents(code, data):
    """A POLYNOM agrees with numpy over any sub-range of its input."""
    assume(code != "raw")
    nframes = data.draw(st.integers(min_value=1, max_value=24))
    values = data.draw(payload(gd.FLOAT64, nframes))
    coeffs = (1.0, 2.0, 3.0)
    first, count = data.draw(extent(nframes))

    with Dirfile() as df:
        df.D.add(gd.entry(gd.RAW_ENTRY, "raw", 0,
                          dict(type=gd.FLOAT64, spf=1)))
        df.D.putdata("raw", values)
        df.D.add(gd.entry(gd.POLYNOM_ENTRY, code, 0,
                          dict(in_field="raw", a=coeffs)))
        out = df.reopen().getdata(code, gd.FLOAT64, first_sample=first,
                                  num_samples=count)

        window = values[first:first + count]
        expect = numpy.polyval(list(reversed(coeffs)), window)
        assert numpy.allclose(out, expect, rtol=1e-9, atol=1e-6)


@fuzzer
@given(code=FIELD_CODES, data=st.data())
def test_bit_over_generated_extents(code, data):
    """A BIT field agrees with a mask/shift over any sub-range."""
    assume(code != "raw")
    nframes = data.draw(st.integers(min_value=1, max_value=24))
    values = data.draw(payload(gd.UINT16, nframes))
    bitnum, numbits = 1, 3
    first, count = data.draw(extent(nframes))

    with Dirfile() as df:
        df.D.add(gd.entry(gd.RAW_ENTRY, "raw", 0, dict(type=gd.UINT16, spf=1)))
        df.D.putdata("raw", values)
        df.D.add(gd.entry(gd.BIT_ENTRY, code, 0,
                          dict(in_field="raw", bitnum=bitnum,
                               numbits=numbits)))
        out = df.reopen().getdata(code, gd.UINT32, first_sample=first,
                                  num_samples=count)

        window = values[first:first + count].astype(numpy.uint64)
        assert numpy.array_equal(out, (window >> bitnum) & ((1 << numbits) - 1))


@fuzzer
@given(code=FIELD_CODES, data=st.data())
def test_multiply_and_divide_over_generated_extents(code, data):
    """MULTIPLY and DIVIDE agree with numpy over any sub-range.

    Two-input derived fields have to seek both inputs in step, so the offset
    arithmetic here is not the same code as the single-input case.
    """
    assume(code not in ("a", "b"))
    nframes = data.draw(st.integers(min_value=1, max_value=24))
    a = data.draw(payload(gd.FLOAT64, nframes))
    b = data.draw(payload(gd.FLOAT64, nframes))
    assume(numpy.all(b != 0))
    first, count = data.draw(extent(nframes))

    with Dirfile() as df:
        for name, v in (("a", a), ("b", b)):
            df.D.add(gd.entry(gd.RAW_ENTRY, name, 0,
                              dict(type=gd.FLOAT64, spf=1)))
            df.D.putdata(name, v)
        # Two-input entries take in_field1/in_field2, not in_fields.
        df.D.add(gd.entry(gd.MULTIPLY_ENTRY, code, 0,
                          dict(in_field1="a", in_field2="b")))
        df.D.add(gd.entry(gd.DIVIDE_ENTRY, code + "d", 0,
                          dict(in_field1="a", in_field2="b")))
        D = df.reopen()

        sl = slice(first, first + count)
        got_m = D.getdata(code, gd.FLOAT64, first_sample=first,
                          num_samples=count)
        got_d = D.getdata(code + "d", gd.FLOAT64, first_sample=first,
                          num_samples=count)
        assert numpy.allclose(got_m, a[sl] * b[sl], rtol=1e-12, atol=1e-9)
        assert numpy.allclose(got_d, a[sl] / b[sl], rtol=1e-12, atol=1e-9)


@fuzzer
@given(code=FIELD_CODES, data=st.data())
def test_window_over_generated_extents(code, data):
    """A WINDOW field gates its input consistently over any sub-range.

    Only GT is used: get_window_*.c already covers all eight operators.  What
    is generated here is the read window, and the check that the gating lines
    up with the right input samples once the read is offset.
    """
    assume(code not in ("a", "c"))
    nframes = data.draw(st.integers(min_value=1, max_value=24))
    a = data.draw(payload(gd.FLOAT64, nframes))
    check = data.draw(payload(gd.INT32, nframes))
    threshold = 4
    first, count = data.draw(extent(nframes))

    with Dirfile() as df:
        df.D.add(gd.entry(gd.RAW_ENTRY, "a", 0, dict(type=gd.FLOAT64, spf=1)))
        df.D.add(gd.entry(gd.RAW_ENTRY, "c", 0, dict(type=gd.INT32, spf=1)))
        df.D.putdata("a", a)
        df.D.putdata("c", check)
        df.D.add(gd.entry(gd.WINDOW_ENTRY, code, 0,
                          dict(in_field1="a", in_field2="c",
                               windop=gd.WINDOP_GT, threshold=threshold)))
        out = df.reopen().getdata(code, gd.FLOAT64, first_sample=first,
                                  num_samples=count)

        sl = slice(first, first + count)
        keep = check[sl].astype(numpy.int64) > threshold
        assert numpy.array_equal(numpy.isnan(out), ~keep)
        assert numpy.allclose(out[keep], a[sl][keep], rtol=1e-12, atol=1e-9)


# ---------------------------------------------------------------------------
# Complex-valued properties
#
# Complex scalar parameters, and real parameters applied to complex-valued
# inputs, are a distinct code path in the library (see the fix in 5f44d13d,
# "Fix complex-typed reads of LINCOMs with real scalar parameters").
# ---------------------------------------------------------------------------

@st.composite
def complex_scalar(draw):
    """A complex number which survives storage in a format file exactly."""
    part = st.integers(min_value=-1000, max_value=1000)
    return complex(draw(part), draw(part))


@fuzzer
@given(code=FIELD_CODES, m=complex_scalar(), b=complex_scalar(),
       data=st.data())
def test_complex_lincom(code, m, b, data):
    """A LINCOM over a complex field with complex parameters computes m*in+b."""
    assume(code != "raw")
    values = data.draw(payload(gd.COMPLEX128, data.draw(
        st.integers(min_value=1, max_value=12))))

    with Dirfile() as df:
        df.D.add(gd.entry(gd.RAW_ENTRY, "raw", 0,
                          dict(type=gd.COMPLEX128, spf=1)))
        df.D.putdata("raw", values)
        df.D.add(gd.entry(gd.LINCOM_ENTRY, code, 0,
                          dict(in_fields=("raw",), m=(m,), b=(b,))))
        out = df.reopen().getdata(code, gd.COMPLEX128, first_frame=0,
                                  num_samples=len(values))
        assert numpy.allclose(out, m * values + b, rtol=1e-12, atol=1e-9)


@fuzzer
@given(
    code=FIELD_CODES,
    m=st.integers(min_value=-1000, max_value=1000).map(float),
    b=st.integers(min_value=-1000, max_value=1000).map(float),
    data=st.data(),
)
def test_real_scalars_on_complex_lincom(code, m, b, data):
    """A LINCOM with *real* parameters over a complex field."""
    assume(code != "raw")
    values = data.draw(payload(gd.COMPLEX128, data.draw(
        st.integers(min_value=1, max_value=12))))

    with Dirfile() as df:
        df.D.add(gd.entry(gd.RAW_ENTRY, "raw", 0,
                          dict(type=gd.COMPLEX128, spf=1)))
        df.D.putdata("raw", values)
        df.D.add(gd.entry(gd.LINCOM_ENTRY, code, 0,
                          dict(in_fields=("raw",), m=(m,), b=(b,))))
        out = df.reopen().getdata(code, gd.COMPLEX128, first_frame=0,
                                  num_samples=len(values))
        assert numpy.allclose(out, m * values + b, rtol=1e-12, atol=1e-9)


@fuzzer
@given(code=FIELD_CODES, dividend=complex_scalar(), data=st.data())
def test_complex_recip(code, dividend, data):
    """A RECIP over a complex field computes dividend / in."""
    assume(code != "raw")
    values = data.draw(payload(gd.COMPLEX128, data.draw(
        st.integers(min_value=1, max_value=12))))
    assume(numpy.all(values != 0))

    with Dirfile() as df:
        df.D.add(gd.entry(gd.RAW_ENTRY, "raw", 0,
                          dict(type=gd.COMPLEX128, spf=1)))
        df.D.putdata("raw", values)
        df.D.add(gd.entry(gd.RECIP_ENTRY, code, 0,
                          dict(in_field="raw", dividend=dividend)))
        out = df.reopen().getdata(code, gd.COMPLEX128, first_frame=0,
                                  num_samples=len(values))
        assert numpy.allclose(out, dividend / values, rtol=1e-12, atol=1e-9)


@fuzzer
@given(field=raw_field(types=[gd.COMPLEX64, gd.COMPLEX128]), data=st.data())
def test_complex_read_as_real_gives_real_part(field, data):
    """Reading a complex field into a real type yields the real part."""
    code, dtype, spf = field
    values = data.draw(payload(dtype, spf * data.draw(
        st.integers(min_value=1, max_value=4))))

    with Dirfile() as df:
        df.D.add(gd.entry(gd.RAW_ENTRY, code, 0, dict(type=dtype, spf=spf)))
        df.D.putdata(code, values)
        out = df.reopen().getdata(code, gd.FLOAT64, first_frame=0,
                                  num_samples=len(values))
        assert numpy.allclose(out, values.real, rtol=1e-12, atol=1e-9)


# ---------------------------------------------------------------------------
# Scalar-field properties
# ---------------------------------------------------------------------------

@fuzzer
@given(code=FIELD_CODES,
       value=st.integers(min_value=-(2**31), max_value=2**31 - 1).map(float))
def test_const_roundtrip(code, value):
    """A CONST field reads back the value written to it."""
    with Dirfile() as df:
        df.D.add(gd.entry(gd.CONST_ENTRY, code, 0, dict(type=gd.FLOAT64)))
        df.D.put_constant(code, value)
        assert df.reopen().get_constant(code, gd.FLOAT64) == value


@fuzzer
@given(code=FIELD_CODES,
       values=st.lists(st.integers(min_value=-(2**31), max_value=2**31 - 1),
                       min_size=1, max_size=8))
def test_carray_roundtrip(code, values):
    """A CARRAY field reads back the array written to it."""
    with Dirfile() as df:
        df.D.add(gd.entry(gd.CARRAY_ENTRY, code, 0,
                          dict(type=gd.FLOAT64, array_len=len(values))))
        df.D.put_carray(code, numpy.array(values, dtype=numpy.float64))
        D = df.reopen()
        assert D.array_len(code) == len(values)
        assert numpy.array_equal(D.get_carray(code, gd.FLOAT64), values)


@fuzzer
@given(code=FIELD_CODES,
       value=st.text(alphabet=st.characters(
           min_codepoint=32, max_codepoint=126), min_size=0, max_size=32)
       .filter(lambda s: "\\" not in s))
def test_string_roundtrip(code, value):
    """A STRING field reads back the string written to it.

    Backslashes are excluded: the format-file writer escapes them, and
    escaping round-trips are already covered by test/parse_escaped_*.c.
    """
    with Dirfile() as df:
        df.D.add(gd.entry(gd.STRING_ENTRY, code, 0, {}))
        df.D.put_string(code, value)
        assert decode(df.reopen().get_string(code)) == value


# ---------------------------------------------------------------------------
# Frame-offset geometry
#
# A fragment's frame offset shifts where its fields begin: samples before
# frameoffset * spf are not stored, and read back as zero.  Every read which
# straddles that boundary has to splice synthetic zeros onto real data, and the
# splice point moves with spf, the offset, and the read window -- a geometry
# with no dedicated tests in test/ at all (there is no frameoffset_*.c).
# ---------------------------------------------------------------------------

@fuzzer
@given(
    frameoffset=st.integers(min_value=0, max_value=6),
    spf=st.integers(min_value=1, max_value=5),
    nframes=st.integers(min_value=1, max_value=6),
    data=st.data(),
)
def test_frameoffset_pads_with_zeros(frameoffset, spf, nframes, data):
    """Reads spanning a frame offset splice zeros onto the stored data.

    The model: sample i reads as zero for i < frameoffset * spf, as the stored
    value for frameoffset * spf <= i < eof, and is absent past eof (a short
    read, not an error).
    """
    values = data.draw(payload(gd.INT32, spf * nframes))

    with Dirfile() as df:
        df.D.add(gd.entry(gd.RAW_ENTRY, "r", 0, dict(type=gd.INT32, spf=spf)))
        df.D.putdata("r", values)
        if frameoffset:
            df.D.fragment(0).alter_frameoffset(frameoffset, recode=0)
        D = df.reopen()

        bof = frameoffset * spf
        eof = bof + len(values)
        assert D.bof("r") == bof
        assert D.eof("r") == eof
        assert D.nframes == frameoffset + nframes

        first = data.draw(st.integers(min_value=0, max_value=eof + 4))
        count = data.draw(st.integers(min_value=1, max_value=eof + 4))
        out = D.getdata("r", gd.INT32, first_sample=first, num_samples=count)

        idx = numpy.arange(first, first + count)
        expect = numpy.where(idx < bof, 0,
                             values[numpy.clip(idx - bof, 0,
                                               len(values) - 1)])
        expect = expect[idx < eof]
        note("bof=%d eof=%d first=%d count=%d" % (bof, eof, first, count))
        assert numpy.array_equal(out, expect)


@fuzzer
@given(
    frameoffset=st.integers(min_value=1, max_value=6),
    spf=st.integers(min_value=1, max_value=5),
    nframes=st.integers(min_value=1, max_value=6),
    data=st.data(),
)
def test_frameoffset_chunked_reads_tile(frameoffset, spf, nframes, data):
    """Chunked reads across a frame offset reconstruct the unchunked read.

    Reading in pieces must not change what comes back, however the pieces fall
    relative to the padding boundary.
    """
    values = data.draw(payload(gd.INT32, spf * nframes))

    with Dirfile() as df:
        df.D.add(gd.entry(gd.RAW_ENTRY, "r", 0, dict(type=gd.INT32, spf=spf)))
        df.D.putdata("r", values)
        df.D.fragment(0).alter_frameoffset(frameoffset, recode=0)
        D = df.reopen()

        eof = D.eof("r")
        whole = D.getdata("r", gd.INT32, first_sample=0, num_samples=eof)

        cuts = sorted(set(data.draw(st.lists(
            st.integers(min_value=0, max_value=eof), max_size=5))) | {0, eof})
        pieces = [D.getdata("r", gd.INT32, first_sample=lo, num_samples=hi - lo)
                  for lo, hi in zip(cuts, cuts[1:]) if hi > lo]
        joined = numpy.concatenate(pieces) if pieces else whole[:0]
        note("cuts=%r bof=%d" % (cuts, D.bof("r")))
        assert numpy.array_equal(joined, whole)


# ---------------------------------------------------------------------------
# Stateful testing
#
# Drives sequences of metadata operations against a plain-dict model.  This is
# where ordering bugs -- add/delete/rename interacting with the entry list and
# the format file -- surface.
# ---------------------------------------------------------------------------

class DirfileModel(RuleBasedStateMachine):
    """Compare a dirfile's metadata against an independent Python model."""

    def __init__(self):
        super().__init__()
        self.df = None
        self.model = {}  # code -> (type, spf)

    @initialize()
    def create(self):
        self.df = Dirfile()

    def teardown(self):
        if self.df is not None:
            self.df.__exit__(None, None, None)

    codes = st.from_regex(r"\A[A-Za-z][A-Za-z0-9_]{0,5}\Z").filter(
        lambda s: s != "INDEX")

    @rule(code=codes, dtype=st.sampled_from(EXACT_TYPES),
          spf=st.integers(min_value=1, max_value=4))
    def add_raw(self, code, dtype, spf):
        if code in self.model:
            return
        self.df.D.add(gd.entry(gd.RAW_ENTRY, code, 0,
                               dict(type=dtype, spf=spf)))
        self.model[code] = (dtype, spf)

    @precondition(lambda self: self.model)
    @rule(data=st.data())
    def delete_field(self, data):
        code = data.draw(st.sampled_from(sorted(self.model)))
        self.df.D.delete(code, flags=gd.DEL_DATA)
        del self.model[code]

    @precondition(lambda self: self.model)
    @rule(data=st.data(), new=codes)
    def rename_field(self, data, new):
        code = data.draw(st.sampled_from(sorted(self.model)))
        if new in self.model:
            return
        self.df.D.rename(code, new)
        self.model[new] = self.model.pop(code)

    # An empty dirfile flushes a zero-byte format file, which cannot be
    # reparsed ("Unable to determine encoding scheme"), so only reopen once
    # there is at least one field to write out.
    @precondition(lambda self: self.model)
    @rule()
    def reopen(self):
        """Flushing and reparsing must not change the metadata."""
        self.df.reopen(gd.RDWR)

    @invariant()
    def field_list_agrees(self):
        if self.df is None:
            return
        listed = {decode(f) for f in self.df.D.field_list()}
        assert listed == set(self.model) | {"INDEX"}

    @invariant()
    def entries_agree(self):
        if self.df is None:
            return
        for code, (dtype, spf) in self.model.items():
            e = self.df.D.entry(code)
            assert e.data_type == dtype, (code, e.data_type, dtype)
            assert e.spf == spf, (code, e.spf, spf)


TestDirfileModel = DirfileModel.TestCase
TestDirfileModel.settings = fuzzer


class SparseWriteModel(RuleBasedStateMachine):
    """Write a field at generated offsets, comparing against a numpy model.

    A dirfile field behaves like an infinite zero-filled array which grows to
    fit whatever has been written: writing past the end leaves a hole which
    reads back as zero, and eof tracks the furthest write rather than the last
    one.  So the model is just a numpy array, extended as needed.

    The states worth reaching are combinations -- write past the end to open a
    hole, overwrite backwards across the hole's edge, extend again -- which is
    what a sequence of generated writes explores and a fixed test does not.

    The machine runs over every writable encoding to ensure consistent
    semantics on top of different plugins.
    """

    # Small, so that writes collide often rather than scattering into disjoint
    # regions where nothing interesting overlaps.
    LIMIT = 24

    def __init__(self):
        super().__init__()
        self.df = None
        self.model = numpy.zeros(0, dtype=numpy.int64)

    @initialize(spf=st.integers(min_value=1, max_value=4),
                encoding=st.sampled_from(WRITABLE_ENCODINGS))
    def create(self, spf, encoding):
        self.df = Dirfile(encoding=encoding)
        self.spf = spf
        self.df.D.add(gd.entry(gd.RAW_ENTRY, "r", 0,
                               dict(type=gd.INT32, spf=spf)))

    def teardown(self):
        if self.df is not None:
            self.df.__exit__(None, None, None)

    @rule(
        first=st.integers(min_value=0, max_value=LIMIT),
        values=st.lists(st.integers(min_value=-(2**20), max_value=2**20),
                        min_size=1, max_size=8),
    )
    def write(self, first, values):
        """Write at an arbitrary offset, extending or overwriting."""
        v = numpy.array(values, dtype=numpy.int64)
        assert self.df.D.putdata(
            "r", v.astype(numpy.float64), first_sample=first) == len(v)

        end = first + len(v)
        if end > len(self.model):
            self.model = numpy.concatenate([
                self.model,
                numpy.zeros(end - len(self.model), dtype=numpy.int64)])
        self.model[first:end] = v

    @rule()
    def flush(self):
        self.df.D.flush()

    @precondition(lambda self: len(self.model))
    @rule()
    def reopen(self):
        self.df.reopen(gd.RDWR)

    @precondition(lambda self: len(self.model))
    @rule(data=st.data())
    def read_slice(self, data):
        """Any sub-range reads back what the model holds."""
        first = data.draw(st.integers(min_value=0,
                                      max_value=len(self.model) - 1))
        count = data.draw(st.integers(min_value=1,
                                      max_value=len(self.model) - first))
        self.df.D.flush()
        out = self.df.D.getdata("r", gd.INT32, first_sample=first,
                                num_samples=count)
        assert numpy.array_equal(out, self.model[first:first + count]), (
            first, count, out, self.model[first:first + count])

    @invariant()
    def whole_field_agrees(self):
        if self.df is None or not len(self.model):
            return
        self.df.D.flush()
        out = self.df.D.getdata("r", gd.INT32, first_sample=0,
                                num_samples=len(self.model))
        assert numpy.array_equal(out, self.model), (out, self.model)

    @invariant()
    def eof_tracks_furthest_write(self):
        """eof follows the furthest write; nframes counts *complete* frames.

        A partially-written trailing frame is not counted, so nframes floors
        rather than rounding up.
        """
        if self.df is None or not len(self.model):
            return
        self.df.D.flush()
        assert self.df.D.eof("r") == len(self.model)
        assert self.df.D.nframes == len(self.model) // self.spf


TestSparseWriteModel = SparseWriteModel.TestCase
TestSparseWriteModel.settings = fuzzer


if __name__ == "__main__":
    sys.exit(pytest.main([os.path.abspath(__file__), "-q"]))
