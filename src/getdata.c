/* (C) 2002-2005 C. Barth Netterfield
 * (C) 2005-2009 D. V. Wiebe
 *
 ***************************************************************************
 *
 * This file is part of the GetData project.
 *
 * GetData is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation; either version 2.1 of the License, or (at your
 * option) any later version.
 *
 * GetData is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with GetData; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
#include "internal.h"

#ifdef STDC_HEADERS
#include <inttypes.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#endif

#define EXTRACT_REPR(it,ot,f) \
  for (i = 0; i < n; ++i) ((ot*)rdata)[i] = (ot)f(((it*)cdata)[i])

#define EXTRACT_REPR2(it,ot) \
  switch (repr) { \
    case GD_REPR_REAL: EXTRACT_REPR(it,ot,creal); break; \
    case GD_REPR_IMAG: EXTRACT_REPR(it,ot,cimag); break; \
    case GD_REPR_MOD:  EXTRACT_REPR(it,ot,cabs); break; \
    case GD_REPR_ARG:  EXTRACT_REPR(it,ot,carg); break; \
  }

#define EXTRACT_REPRS(ot) \
  switch (in_type) { \
    case GD_UINT8:      EXTRACT_REPR2(       uint8_t, ot); break; \
    case GD_INT8:       EXTRACT_REPR2(        int8_t, ot); break; \
    case GD_UINT16:     EXTRACT_REPR2(      uint16_t, ot); break; \
    case GD_INT16:      EXTRACT_REPR2(       int16_t, ot); break; \
    case GD_UINT32:     EXTRACT_REPR2(      uint32_t, ot); break; \
    case GD_INT32:      EXTRACT_REPR2(       int32_t, ot); break; \
    case GD_UINT64:     EXTRACT_REPR2(      uint64_t, ot); break; \
    case GD_INT64:      EXTRACT_REPR2(       int64_t, ot); break; \
    case GD_FLOAT32:    EXTRACT_REPR2(         float, ot); break; \
    case GD_FLOAT64:    EXTRACT_REPR2(        double, ot); break; \
    case GD_COMPLEX64:  EXTRACT_REPR2( float complex, ot); break; \
    case GD_COMPLEX128: EXTRACT_REPR2(double complex, ot); break; \
    default: \
      _GD_SetError(D, GD_E_BAD_TYPE, in_type, NULL, 0, NULL); \
    case GD_NULL: \
      break; \
  }


static void _GD_ExtractRepr(DIRFILE* D, const void* cdata, gd_type_t in_type,
    void* rdata, gd_type_t type, size_t n, int repr)
{
  size_t i;

  dtrace("%p, %p, %x, %p, %x, %zi, %i", D, cdata, in_type, rdata, type, n,
      repr);

  switch (type) {
    case GD_UINT8:      EXTRACT_REPRS(       uint8_t); break;
    case GD_INT8:       EXTRACT_REPRS(        int8_t); break;
    case GD_UINT16:     EXTRACT_REPRS(      uint16_t); break;
    case GD_INT16:      EXTRACT_REPRS(       int16_t); break;
    case GD_UINT32:     EXTRACT_REPRS(      uint32_t); break;
    case GD_INT32:      EXTRACT_REPRS(       int32_t); break;
    case GD_UINT64:     EXTRACT_REPRS(      uint64_t); break;
    case GD_INT64:      EXTRACT_REPRS(       int64_t); break;
    case GD_FLOAT32:    EXTRACT_REPRS(         float); break;
    case GD_FLOAT64:    EXTRACT_REPRS(        double); break;
    case GD_COMPLEX64:  EXTRACT_REPRS( float complex); break;
    case GD_COMPLEX128: EXTRACT_REPRS(double complex); break;
    default:
      _GD_SetError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
    case GD_NULL:
      break;
  }
}

/* _GD_FillFileFrame: fill dataout with frame indices
*/
static void _GD_FillFileFrame(void *dataout, gd_type_t rtype, off64_t s0,
    size_t n)
{
  size_t i;

  dtrace("%p, 0x%x, %lli, %zi", dataout, rtype, s0, n);

  switch (rtype) {
    case GD_INT8:
      for (i = 0; i < n; i++)
        ((int8_t*)dataout)[i] = (int8_t)(i + s0);
      break;
    case GD_UINT8:
      for (i = 0; i < n; i++)
        ((uint8_t*)dataout)[i] = (uint8_t)(i + s0);
      break;
    case GD_INT16:
      for (i = 0; i < n; i++)
        ((int16_t*)dataout)[i] = (int16_t)(i + s0);
      break;
    case GD_UINT16:
      for (i = 0; i < n; i++)
        ((uint16_t*)dataout)[i] = (uint16_t)(i + s0);
      break;
    case GD_INT32:
      for (i = 0; i < n; i++)
        ((int32_t*)dataout)[i] = (int32_t)(i + s0);
      break;
    case GD_UINT32:
      for (i = 0; i < n; i++)
        ((uint32_t*)dataout)[i] = (uint32_t)(i + s0);
      break;
    case GD_INT64:
      for (i = 0; i < n; i++)
        ((int64_t*)dataout)[i] = (int64_t)(i + s0);
      break;
    case GD_UINT64:
      for (i = 0; i < n; i++)
        ((uint64_t*)dataout)[i] = (uint64_t)(i + s0);
      break;
    case GD_FLOAT32:
      for (i = 0; i < n; i++)
        ((float*)dataout)[i] = (float)(i + s0);
      break;
    case GD_FLOAT64:
      for (i = 0; i < n; i++)
        ((double*)dataout)[i] = (double)(i + s0);
      break;
    case GD_COMPLEX64:
      for (i = 0; i < n; i++)
        ((float complex*)dataout)[i] = (float complex)(i + s0);
      break;
    case GD_COMPLEX128:
      for (i = 0; i < n; i++)
        ((double complex*)dataout)[i] = (double complex)(i + s0);
      break;
    default:
      break;
  }

  dreturnvoid();
}

/* _GD_FillZero: fill data buffer with zero/NaN of the appropriate type.
 */
int _GD_FillZero(void *databuffer, gd_type_t type, size_t nz)
{
  size_t i;
  const double NaN = NAN;

  dtrace("%p, 0x%x, %zi", databuffer, type, nz);

  if (type & GD_IEEE754) {
    if (type == GD_FLOAT32)
      for (i = 0; i < nz; ++i)
        *((float*)databuffer + i) = (float)NaN;
    else
      for (i = 0; i < nz; ++i)
        *((double*)databuffer + i) = (double)NaN;
  } else if (type & GD_COMPLEX) {
    if (type == GD_COMPLEX64)
      for (i = 0; i < nz; ++i)
        *((float complex*)databuffer + i) = (float complex)(NaN +
            _Complex_I * NaN);
    else
      for (i = 0; i < nz; ++i)
        *((double complex*)databuffer + i) = (double complex)(NaN +
            _Complex_I * NaN);
  } else 
    memset(databuffer, 0, nz * GD_SIZE(type));

  dreturn("%i", nz);

  return (nz);
}

/* _GD_DoRaw:  Read from a raw.  Returns number of samples read.
*/
static size_t _GD_DoRaw(DIRFILE *D, gd_entry_t *E, off64_t s0, size_t ns,
    gd_type_t return_type, void *data_out)
{
  size_t n_read = 0;
  ssize_t samples_read;
  char *databuffer;
  size_t zero_pad = 0;

  dtrace("%p, %p, %lli, %zi, 0x%x, %p)", D, E, s0, ns, return_type, data_out);

  if (s0 < E->spf * D->fragment[E->fragment_index].frame_offset)
    zero_pad = E->spf * D->fragment[E->fragment_index].frame_offset - s0;
  else
    s0 -= E->spf * D->fragment[E->fragment_index].frame_offset;

  databuffer = malloc(ns * E->e->size);
  if (databuffer == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%zi", 0);
    return 0;
  }

  if (zero_pad > 0) {
    n_read = _GD_FillZero(databuffer, E->data_type, (zero_pad > ns) ? ns :
        zero_pad);
    ns -= n_read;
    s0 = 0;
  }

  if (ns > 0) {
    /** open the file (and cache the fp) if it hasn't been opened yet. */
    if (E->e->file[0].fp < 0) {
      if (!_GD_Supports(D, E, GD_EF_OPEN | GD_EF_SEEK | GD_EF_READ)) {
        dreturn("%i", 0);
        return 0;
      } else if (_GD_SetEncodedName(D, E->e->file, E->e->filebase, 0)) {
        dreturn("%i", 0);
        return 0;
      } else if ((*_gd_ef[E->e->file[0].encoding].open)(E->e->file,
            D->flags & GD_ACCMODE, 0))
      {
        _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[0].name, errno, NULL);
        dreturn("%zi", 0);
        return 0;
      }
    }

    if ((*_gd_ef[E->e->file[0].encoding].seek)(E->e->file, s0, E->data_type, 0)
        == -1)
    {
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[0].name, errno, NULL);
      dreturn("%zi", 0);
      return 0;
    }

    samples_read = (*_gd_ef[E->e->file[0].encoding].read)(E->e->file,
        databuffer + n_read * E->e->size, E->data_type, ns);

    if (samples_read == -1) {
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[0].name, errno, NULL);
      free(databuffer);
      dreturn("%zi", 0);
      return 0;
    }

    if (_gd_ef[E->e->file[0].encoding].ecor &&
        (D->fragment[E->fragment_index].byte_sex ==
#ifdef WORDS_BIGENDIAN
         GD_LITTLE_ENDIAN
#else
         GD_BIG_ENDIAN
#endif
        ))
    {
      if (E->data_type & GD_COMPLEX)
        _GD_FixEndianness(databuffer + n_read * E->e->size, E->e->size / 2,
            samples_read * 2);
      else
        _GD_FixEndianness(databuffer + n_read * E->e->size, E->e->size,
            samples_read);
    }

    n_read += samples_read;
  }
  _GD_ConvertType(D, databuffer, E->data_type, data_out, return_type, n_read);

  free(databuffer);

  dreturn("%zi", (D->error == GD_E_OK) ? n_read : 0);
  return (D->error == GD_E_OK) ? n_read : 0;
}

/* Macros to reduce tangly code */
#define POLYNOM5(t) \
  for (i = 0; i < npts; i++) ((t*)data)[i] = (t)( \
      ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] \
      * ((t*)data)[i] * ((t*)data)[i] * a[5] \
      + ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] * a[4] \
      + ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] * a[3] \
      + ((t*)data)[i] * ((t*)data)[i] * a[2] \
      + ((t*)data)[i] * a[1] + a[0] \
      )

#define POLYNOM4(t) \
  for (i = 0; i < npts; i++) ((t*)data)[i] = (t)( \
    ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] * a[4] \
    + ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] * a[3] \
    + ((t*)data)[i] * ((t*)data)[i] * a[2] \
    + ((t*)data)[i] * a[1] + a[0] \
    )

#define POLYNOM3(t) \
  for (i = 0; i < npts; i++) ((t*)data)[i] = (t)( \
    ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] * a[3] \
    + ((t*)data)[i] * ((t*)data)[i] * a[2] \
    + ((t*)data)[i] * a[1] + a[0] \
    )

#define POLYNOM2(t) \
  for (i = 0; i < npts; i++) ((t*)data)[i] = (t)( \
    ((t*)data)[i] * ((t*)data)[i] * a[2] \
    + ((t*)data)[i] * a[1] + a[0] \
    )

#define POLYNOM(t) \
  switch (n) { \
    case 2: POLYNOM2(t); break; \
    case 3: POLYNOM3(t); break; \
    case 4: POLYNOM4(t); break; \
    case 5: POLYNOM5(t); break; \
    default: _GD_InternalError(D); \
  }

/* _GD_PolynomData: Compute data = Sum(i=0..n; data**i * a[i]), for scalar a,
 * and integer 2 <= n < GD_MAX_POLYORD
 */
static void _GD_PolynomData(DIRFILE* D, void *data, gd_type_t type, size_t npts,
    int n, double* a)
{
  size_t i;

  dtrace("%p, %p, 0x%x, %zi, %i, %p", D, data, type, npts, n, a);

  if (n == 1) {
    /* no need to duplicate this case */
    _GD_LincomData(D, 1, data, type, NULL, NULL, a + 1, a, NULL, npts);
  } else {
    switch (type) {
      case GD_NULL:
        break;
      case GD_INT8:       POLYNOM(        int8_t); break;
      case GD_UINT8:      POLYNOM(       uint8_t); break;
      case GD_INT16:      POLYNOM(       int16_t); break;
      case GD_UINT16:     POLYNOM(      uint16_t); break;
      case GD_INT32:      POLYNOM(       int32_t); break;
      case GD_UINT32:     POLYNOM(      uint32_t); break;
      case GD_INT64:      POLYNOM(       int64_t); break;
      case GD_UINT64:     POLYNOM(      uint64_t); break;
      case GD_FLOAT32:    POLYNOM(         float); break;
      case GD_FLOAT64:    POLYNOM(        double); break;
      case GD_COMPLEX64:  POLYNOM( float complex); break;
      case GD_COMPLEX128: POLYNOM(double complex); break;
      default:
        _GD_SetError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
        break;
    }
  }

  dreturnvoid();
}

/* _GD_CPolynomData: Compute data = Sum(i=0..n; data**i * a[i]), for complex
 * scalar a, and integer 2 <= n < GD_MAX_POLYORD
 */
static void _GD_CPolynomData(DIRFILE* D, void *data, gd_type_t type,
    size_t npts, int n, double complex* a)
{
  size_t i;

  dtrace("%p, %p, 0x%x, %zi, %i, %p", D, data, type, npts, n, a);

  if (n == 1) {
    /* no need to duplicate this case */
    _GD_CLincomData(D, 1, data, type, NULL, NULL, a + 1, a, NULL, npts);
  } else {
    switch (type) {
      case GD_NULL:
        break;
      case GD_INT8:       POLYNOM(        int8_t); break;
      case GD_UINT8:      POLYNOM(       uint8_t); break;
      case GD_INT16:      POLYNOM(       int16_t); break;
      case GD_UINT16:     POLYNOM(      uint16_t); break;
      case GD_INT32:      POLYNOM(       int32_t); break;
      case GD_UINT32:     POLYNOM(      uint32_t); break;
      case GD_INT64:      POLYNOM(       int64_t); break;
      case GD_UINT64:     POLYNOM(      uint64_t); break;
      case GD_FLOAT32:    POLYNOM(         float); break;
      case GD_FLOAT64:    POLYNOM(        double); break;
      case GD_COMPLEX64:  POLYNOM( float complex); break;
      case GD_COMPLEX128: POLYNOM(double complex); break;
      default:
        _GD_SetError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
        break;
    }
  }

  dreturnvoid();
}

#define MULTIPLY(t) \
  for (i = 0; i < n; i++) ((t*)A)[i] = (t)((double)((t*)A)[i] * \
      B[i * spfB / spfA])
#define CMULTIPLY(t) \
  for (i = 0; i < n; i++) ((t*)A)[i] = (t)((double complex)((t*)A)[i] * \
      (t)B[i * spfB / spfA])

/* MultiplyData: Multiply A by B.  B is unchanged.
*/
static void _GD_MultiplyData(DIRFILE* D, void *A, gd_spf_t spfA, double *B,
    gd_spf_t spfB, gd_type_t type, size_t n)
{
  size_t i;

  dtrace("%p, %p, %u, %p, %u, 0x%x, %zi", D, A, spfA, B, spfB, type, n);

  switch (type) {
    case GD_NULL: /* null read */
      break;
    case GD_UINT8:      MULTIPLY(       uint8_t); break;
    case GD_INT8:       MULTIPLY(        int8_t); break;
    case GD_UINT16:     MULTIPLY(      uint16_t); break;
    case GD_INT16:      MULTIPLY(       int16_t); break;
    case GD_UINT32:     MULTIPLY(      uint32_t); break;
    case GD_INT32:      MULTIPLY(       int32_t); break;
    case GD_UINT64:     MULTIPLY(      uint64_t); break;
    case GD_INT64:      MULTIPLY(       int64_t); break;
    case GD_FLOAT32:    MULTIPLY(         float); break;
    case GD_FLOAT64:    MULTIPLY(        double); break;
    case GD_COMPLEX64:  MULTIPLY( float complex); break;
    case GD_COMPLEX128: MULTIPLY(double complex); break;
    default:
      _GD_SetError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
      break;
  }

  dreturnvoid();
}

/* MultiplyData: Multiply A by B.  B is complex.
*/
static void _GD_CMultiplyData(DIRFILE* D, void *A, gd_spf_t spfA,
    double complex *B, gd_spf_t spfB, gd_type_t type, size_t n)
{
  size_t i;

  dtrace("%p, %p, %u, %p, %u, 0x%x, %zi", D, A, spfA, B, spfB, type, n);

  switch (type) {
    case GD_NULL:
      break;
    case GD_UINT8:      CMULTIPLY(       uint8_t); break;
    case GD_INT8:       CMULTIPLY(        int8_t); break;
    case GD_UINT16:     CMULTIPLY(      uint16_t); break;
    case GD_INT16:      CMULTIPLY(       int16_t); break;
    case GD_UINT32:     CMULTIPLY(      uint32_t); break;
    case GD_INT32:      CMULTIPLY(       int32_t); break;
    case GD_UINT64:     CMULTIPLY(      uint64_t); break;
    case GD_INT64:      CMULTIPLY(       int64_t); break;
    case GD_FLOAT32:    CMULTIPLY(         float); break;
    case GD_FLOAT64:    CMULTIPLY(        double); break;
    case GD_COMPLEX64:  CMULTIPLY( float complex); break;
    case GD_COMPLEX128: CMULTIPLY(double complex); break;
    default:
      _GD_SetError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
      break;
  }

  dreturnvoid();
}

/* _GD_DoLincom:  Read from a lincom.  Returns number of samples read.
*/
static size_t _GD_DoLincom(DIRFILE *D, gd_entry_t *E, off64_t first_samp,
    size_t num_samp, gd_type_t return_type, void *data_out)
{
  gd_spf_t spf[GD_MAX_LINCOM];
  size_t n_read;
  int i;
  void *tmpbuf2 = NULL;
  void *tmpbuf3 = NULL;

  dtrace("%p, %p, %lli, %zi, 0x%x, %p", D, E, first_samp, num_samp, return_type,
      data_out);

  const gd_type_t ntype = (return_type & GD_COMPLEX) ? GD_COMPLEX128
    : GD_FLOAT64;

  /* input field checks */
  for (i = 0; i < E->n_fields; ++i) {
    if (_GD_BadInput(D, E, i)) {
      dreturn("%zi", 0);
      return 0;
    }

    spf[i] = _GD_GetSPF(D, E->e->entry[0]);
    if (D->error != GD_E_OK) {
      dreturn("%zi", 0);
      return 0;
    }
  }

  /* read the first field and record the number of samples returned -- we can
   * safely store this in the output buffer, with the correct return type as
   * it will not aversely affect our later math */
  n_read = _GD_DoField(D, E->e->entry[0], E->e->repr[0], first_samp, num_samp,
      return_type, data_out);

  if (D->error) {
    dreturn("%i", 0);
    return 0;
  }

  /* Nothing to lincomise */
  if (n_read == 0) {
    dreturn("%zi", 0);
    return 0;
  }

  /* Some dirfiles use "bar LINCOM foo 1 0" to rename <foo> to <bar>.  I
   * recommend using "bar PHASE foo 0" in this case, but we'll accomodate them
   * as much as we can.  Suggested by MDT. */
  if (E->n_fields == 1 && E->cm[0] == 1 && E->cb[0] == 0) {
    dreturn("%zi", n_read);
    return n_read;
  }

  /* Read the second field, if present */
  if (E->n_fields > 1) {
    /* calculate the first sample, type and number of samples to read of the
     * second field */
    size_t num_samp2 = (int)ceil((double)n_read * spf[1] / spf[0]);
    off64_t first_samp2 = first_samp * spf[1] / spf[0];

    /* Allocate a temporary buffer for the next field */
    tmpbuf2 = _GD_Alloc(D, ntype, num_samp2);
    if (D->error) {
      free(tmpbuf2);
      dreturn("%i", 0);
      return 0;
    }

    /* read the second field */
    size_t n_read2 = _GD_DoField(D, E->e->entry[1], E->e->repr[1], first_samp2,
        num_samp2, ntype, tmpbuf2);
    if (D->error || n_read2 == 0) {
      free(tmpbuf2);
      dreturn("%i", 0);
      return 0;
    }

    /* adjust n_read for a short read from field two */
    if (n_read2 * spf[0] != n_read * spf[1])
      n_read = n_read2 * spf[0] / spf[1];

    /* Do the same for the third field, if needed */
    if (E->n_fields > 2) {
      size_t num_samp3 = (int)ceil((double)n_read * spf[2] / spf[0]);
      off64_t first_samp3 = first_samp * spf[2] / spf[0];

      tmpbuf3 = _GD_Alloc(D, ntype, num_samp3);
      if (D->error) {
        free(tmpbuf2);
        free(tmpbuf3);
        dreturn("%i", 0);
        return 0;
      }

      size_t n_read3 = _GD_DoField(D, E->e->entry[2], E->e->repr[2],
          first_samp3, num_samp3, ntype, tmpbuf3);
      if (D->error || n_read3 == 0) {
        free(tmpbuf2);
        free(tmpbuf3);
        dreturn("%i", 0);
        return 0;
      }

      if (n_read3 * spf[0] != n_read * spf[2])
        n_read = n_read3 * spf[0] / spf[2];
    }
  }

  /* Compute everything at once */
  if (E->comp_scal)
    _GD_CLincomData(D, E->n_fields, data_out, return_type, tmpbuf2, tmpbuf3,
        E->cm, E->cb, spf, n_read);
  else
    _GD_LincomData(D, E->n_fields, data_out, return_type, tmpbuf2, tmpbuf3,
        E->m, E->b, spf, n_read);

  /* free temporary buffers */
  free(tmpbuf2);
  free(tmpbuf3);

  if (D->error)
    n_read = 0;

  dreturn("%zi", n_read);
  return n_read;
}

/* _GD_DoMultiply:  Read from a multiply.  Returns number of samples read.
*/
static size_t _GD_DoMultiply(DIRFILE *D, gd_entry_t* E, off64_t first_samp,
    size_t num_samp, gd_type_t return_type, void *data_out)
{
  void *tmpbuf = NULL;
  gd_spf_t spf1, spf2;
  size_t n_read, n_read2, num_samp2;
  off64_t first_samp2;

  dtrace("%p, %p, %lli, %zi, 0x%x, %p", D, E, first_samp, num_samp, return_type,
      data_out);

  /* Check input fields */
  if (_GD_BadInput(D, E, 0)) {
    dreturn("%zi", 0);
    return 0;
  }

  if (_GD_BadInput(D, E, 1)) {
    dreturn("%zi", 0);
    return 0;
  }

  /* find the samples per frame of the first field */
  spf1 = _GD_GetSPF(D, E->e->entry[0]);
  if (D->error != GD_E_OK) {
    dreturn("%zi", 0);
    return 0;
  }

  /* read the first field and record the number of samples returned */
  n_read = _GD_DoField(D, E->e->entry[0], E->e->repr[0], first_samp, num_samp,
      return_type, data_out);

  if (D->error != GD_E_OK) {
    dreturn("%zi", 0);
    return 0;
  }

  /* Nothing to multiply */
  if (n_read == 0) {
    dreturn("%zi", 0);
    return 0;
  }

  /* find the samples per frame of the second field */
  spf2 = _GD_GetSPF(D, E->e->entry[1]);
  if (D->error != GD_E_OK) {
    dreturn("%zi", 0);
    return 0;
  }

  /* calculate the first sample and number of samples to read of the
   * second field */
  num_samp2 = (int)ceil((double)n_read * spf2 / spf1);
  first_samp2 = first_samp * spf2 / spf1;

  /* find the native type of the second field */
  gd_type_t type2 = (_GD_NativeType(D, E->e->entry[1], E->e->repr[1])
      & GD_COMPLEX) ? GD_COMPLEX128 : GD_FLOAT64;

  /* Allocate a temporary buffer for the second field */
  tmpbuf = _GD_Alloc(D, type2, num_samp2);

  if (D->error != GD_E_OK) {
    free(tmpbuf);
    dreturn("%zi", 0);
    return 0;
  }

  /* read the second field */
  n_read2 = _GD_DoField(D, E->e->entry[1], E->e->repr[1], first_samp2,
      num_samp2, type2, tmpbuf);

  if (D->error != GD_E_OK) {
    free(tmpbuf);
    dreturn("%zi", 0);
    return 0;
  }

  if (n_read2 > 0 && n_read2 * spf1 < n_read * spf2)
    n_read = n_read2 * spf1 / spf2;

  if (type2 & GD_COMPLEX)
    _GD_CMultiplyData(D, data_out, spf1, tmpbuf, spf2, return_type, n_read);
  else
    _GD_MultiplyData(D, data_out, spf1, tmpbuf, spf2, return_type, n_read);

  free(tmpbuf);

  dreturn("%zi", n_read);
  return n_read;
}

/* _GD_DoBit:  Read from a bitfield.  Returns number of samples read.
 *             This is used by both BIT and SBIT (is_signed distinguishes)
 */
static size_t _GD_DoBit(DIRFILE *D, gd_entry_t *E, int is_signed,
    off64_t first_samp, size_t num_samp, gd_type_t return_type, void *data_out)
{
  void *tmpbuf;
  size_t i;
  size_t n_read;

  dtrace("%p, %p, %i, %lli, %zi, 0x%x, %p", D, E, is_signed, first_samp,
      num_samp, return_type, data_out);

  const uint64_t mask = (E->numbits == 64) ? 0xffffffffffffffffULL :
    ((uint64_t)1 << E->numbits) - 1;

  if (_GD_BadInput(D, E, 0)) {
    dreturn("%zi", 0);
    return 0;
  }

  if (is_signed)
    tmpbuf = (int64_t *)malloc(num_samp * sizeof(int64_t));
  else
    tmpbuf = (uint64_t *)malloc(num_samp * sizeof(uint64_t));
  if (tmpbuf == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%zi", 0);
    return 0;
  }

  n_read = _GD_DoField(D, E->e->entry[0], E->e->repr[0], first_samp, num_samp,
      (is_signed) ? GD_INT64 : GD_UINT64, tmpbuf);

  if (D->error != GD_E_OK) {
    free(tmpbuf);
    dreturn("%zi", 0);
    return 0;
  }

  /* extract bits */
  if (is_signed) {
    uint64_t sign = -1 << (E->numbits + - 1);
    for (i = 0; i < n_read; i++)
      ((int64_t*)tmpbuf)[i] = (((((uint64_t*)tmpbuf)[i] >> E->bitnum) & mask)
        + sign) ^ sign;
  } else
    for (i = 0; i < n_read; i++)
      ((uint64_t*)tmpbuf)[i] = (((uint64_t*)tmpbuf)[i] >> E->bitnum) & mask;

  _GD_ConvertType(D, tmpbuf, (is_signed) ? GD_INT64 : GD_UINT64, data_out,
      return_type, n_read);
  free(tmpbuf);

  dreturn("%zi", n_read);
  return n_read;
}

/* _GD_DoPhase:  Read from a phase.  Returns number of samples read.
*/
static size_t _GD_DoPhase(DIRFILE *D, gd_entry_t *E, off64_t first_samp,
    size_t num_samp, gd_type_t return_type, void *data_out)
{
  size_t n_read;

  dtrace("%p, %p, %lli, %zi, 0x%x, %p", D, E, first_samp, num_samp, return_type,
      data_out);

  if (_GD_BadInput(D, E, 0)) {
    dreturn("%zi", 0);
    return 0;
  }

  n_read = _GD_DoField(D, E->e->entry[0], E->e->repr[0], first_samp + E->shift,
      num_samp, return_type, data_out);

  dreturn("%zi", n_read);
  return n_read;
}

/* _GD_DoLinterp:  Read from a linterp.  Returns number of samples read.
*/
static size_t _GD_DoLinterp(DIRFILE *D, gd_entry_t* E, off64_t first_samp,
    size_t num_samp, gd_type_t return_type, void *data_out)
{
  size_t n_read = 0;
  double* data_in;

  dtrace("%p, %p, %lli, %zi, 0x%x, %p", D, E, first_samp, num_samp, return_type,
      data_out);

  if (E->e->table_len < 0) {
    _GD_ReadLinterpFile(D, E);
    if (D->error != GD_E_OK) {
      dreturn("%zi", 0);
      return 0;
    }
  }

  if (_GD_BadInput(D, E, 0)) {
    dreturn("%zi", 0);
    return 0;
  }

  /* allocate a temporary buffer */
  data_in = _GD_Alloc(D, GD_FLOAT64, num_samp);

  if (D->error) {
    free(data_in);
    dreturn("%zi", 0);
    return 0;
  }

  n_read = _GD_DoField(D, E->e->entry[0], E->e->repr[0], first_samp, num_samp,
      GD_FLOAT64, data_in);

  if (D->error != GD_E_OK) {
    free(data_in);
    dreturn("%zi", 0);
    return 0;
  }

  if (E->e->complex_table)
    _GD_CLinterpData(D, data_out, return_type, data_in, n_read, E->e->x,
        E->e->cy, E->e->table_len);
  else
    _GD_LinterpData(D, data_out, return_type, data_in, n_read, E->e->x, E->e->y,
        E->e->table_len);

  free(data_in);
  dreturn("%zi", n_read);
  return n_read;
}

/* _GD_DoPolynom:  Read from a polynom.  Returns number of samples read.
*/
static size_t _GD_DoPolynom(DIRFILE *D, gd_entry_t *E, off64_t first_samp,
    size_t num_samp, gd_type_t return_type, void *data_out)
{
  size_t n_read;

  dtrace("%p, %p, %lli, %zi, 0x%x, %p", D, E, first_samp, num_samp, return_type,
      data_out);

  if (_GD_BadInput(D, E, 0)) {
    dreturn("%zi", 0);
    return 0;
  }

  /* read the input field */
  n_read = _GD_DoField(D, E->e->entry[0], E->e->repr[0], first_samp, num_samp,
      return_type, data_out);

  if (D->error != GD_E_OK) {
    dreturn("%zi", 0);
    return 0;
  }

  /* Nothing to polynomise */
  if (n_read == 0) {
    dreturn("%zi", 0);
    return 0;
  }

  if (E->comp_scal)
    _GD_CPolynomData(D, data_out, return_type, n_read, E->poly_ord, E->ca);
  else
    _GD_PolynomData(D, data_out, return_type, n_read, E->poly_ord, E->a);

  dreturn("%zi", n_read);
  return n_read;
}

/* _GD_DoConst:  Read from a const.  Returns number of samples read (ie. 1).
*/
static size_t _GD_DoConst(DIRFILE *D, const gd_entry_t *E,
    gd_type_t return_type, void *data_out)
{
  dtrace("%p, %p, 0x%x, %p", D, E, return_type, data_out);

  if (E->const_type & GD_SIGNED)
    _GD_ConvertType(D, &E->e->iconst, GD_INT64, data_out, return_type, 1);
  else if (E->const_type & GD_IEEE754)
    _GD_ConvertType(D, &E->e->dconst, GD_FLOAT64, data_out, return_type, 1);
  else if (E->const_type & GD_COMPLEX)
    _GD_ConvertType(D, &E->e->cconst, GD_COMPLEX128, data_out, return_type, 1);
  else
    _GD_ConvertType(D, &E->e->uconst, GD_UINT64, data_out, return_type, 1);

  if (D->error) { /* bad input type */
    dreturn("%i", 0);
    return 0;
  }

  dreturn("%i", 1);
  return 1;
}

/* _GD_DoString:  Read from a string.  Returns number of samples read (ie. the
 * length of the string plus 1).
 */
static size_t _GD_DoString(gd_entry_t *E, size_t num_samp, void *data_out)
{
  dtrace("%p, %zi, %p", E, num_samp, data_out);

  if (num_samp > 0 && data_out != NULL)
    strncpy(data_out, E->e->string, num_samp); 

  dreturn("%i", strlen(E->e->string) + 1);
  return strlen(E->e->string) + 1;
}

/* _GD_DoField: Locate the field in the database and read it.
*/
size_t _GD_DoField(DIRFILE *D, gd_entry_t *E, int repr, off64_t first_samp,
    size_t num_samp, gd_type_t return_type, void *data_out)
{
  size_t n_read = 0;
  gd_type_t ntype;
  void *true_data_out = data_out;
  const gd_type_t true_return_type = return_type; 
  int out_of_place = 0;

  dtrace("%p, %p(%s), %i, %lli, %zi, 0x%x, %p", D, E, E->field, repr,
      first_samp, num_samp, return_type, data_out);

  if (++D->recurse_level >= GD_MAX_RECURSE_LEVEL) {
    _GD_SetError(D, GD_E_RECURSE_LEVEL, 0, NULL, 0, E->field);
    D->recurse_level--;
    dreturn("%zi", 0);
    return 0;
  }

  if (!E->e->calculated)
    _GD_CalculateEntry(D, E);

  if (D->error) {
    dreturn("%i", 0);
    return 0;
  }

  /* calculate the native type */
  ntype = _GD_NativeType(D, E, GD_REPR_NONE); 

  if (D->error) {
    dreturn("%i", 0);
    return 0;
  }

  /* short circuit for purely real native types */
  if (~ntype & GD_COMPLEX) {
    if (repr == GD_REPR_IMAG) {
      memset(data_out, 0, GD_SIZE(return_type) * num_samp);
      dreturn("%zi", num_samp);
      return num_samp;
    } else if (repr == GD_REPR_REAL)
      repr = GD_REPR_NONE;
  }

  /* if the native type is complex valued, but our return type is purely real,
   * we compute the field out-of-place, and then cast it to the return type
   * later, otherwise we just compute things in-place and don't worry too much
   * about accuracy */
  if (ntype & GD_COMPLEX && ~return_type & GD_COMPLEX) {
    out_of_place = 1;
    return_type = GD_COMPLEX128;
    data_out = _GD_Alloc(D, GD_COMPLEX128, num_samp);
    if (repr == GD_REPR_NONE)
      repr = GD_REPR_AUTO;
  }

  switch (E->field_type) {
    case GD_RAW_ENTRY:
      n_read = _GD_DoRaw(D, E, first_samp, num_samp, return_type, data_out);
      break;
    case GD_LINTERP_ENTRY:
      n_read = _GD_DoLinterp(D, E, first_samp, num_samp, return_type, data_out);
      break;
    case GD_LINCOM_ENTRY:
      n_read = _GD_DoLincom(D, E, first_samp, num_samp, return_type, data_out);
      break;
    case GD_BIT_ENTRY:
      n_read = _GD_DoBit(D, E, 0, first_samp, num_samp, return_type, data_out);
      break;
    case GD_MULTIPLY_ENTRY:
      n_read = _GD_DoMultiply(D, E, first_samp, num_samp, return_type,
          data_out);
      break;
    case GD_PHASE_ENTRY:
      n_read = _GD_DoPhase(D, E, first_samp, num_samp, return_type, data_out);
      break;
    case GD_INDEX_ENTRY:
      /* if Asking for "INDEX", just return it */
      _GD_FillFileFrame(data_out, return_type, first_samp, n_read = num_samp);
      break;
    case GD_POLYNOM_ENTRY:
      n_read = _GD_DoPolynom(D, E, first_samp, num_samp, return_type, data_out);
      break;
    case GD_SBIT_ENTRY:
      n_read = _GD_DoBit(D, E, 1, first_samp, num_samp, return_type, data_out);
      break;
    case GD_CONST_ENTRY:
      n_read = _GD_DoConst(D, E, return_type, data_out);
      break;
    case GD_STRING_ENTRY:
      n_read = _GD_DoString(E, num_samp, data_out);
      break;
    case GD_NO_ENTRY:
      /* Can't get here */
      _GD_InternalError(D);
      n_read = 0;
  }

  /* extract the requested representation */
  if (!D->error && repr != GD_REPR_NONE)
    _GD_ExtractRepr(D, data_out, return_type, true_data_out, true_return_type,
        n_read, repr);

  if (out_of_place)
    free(data_out);

  D->recurse_level--;
  dreturn("%zi", n_read);
  return n_read;
}

/* this function is little more than a public boilerplate for _GD_DoField */
size_t getdata64(DIRFILE* D, const char *field_code_in, off64_t first_frame,
    off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t return_type, void *data_out)
{
  size_t n_read = 0;
  gd_entry_t* entry;
  char* field_code;
  int repr;

  dtrace("%p, \"%s\", %lli, %lli, %zi, %zi, 0x%x, %p", D, field_code_in,
      first_frame, first_samp, num_frames, num_samp, return_type, data_out);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%zi", 0);
    return 0;
  }

  _GD_ClearError(D);

  repr = _GD_GetRepr(D, field_code_in, &field_code);

  if (D->error) {
    dreturn("%i", 0);
    return 0;
  }

  entry = _GD_FindField(D, field_code, NULL);

  if (entry == NULL)
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
  else if (entry->field_type & GD_SCALAR_ENTRY)
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);

  if (field_code != field_code_in)
    free(field_code);

  if (D->error) {
    dreturn("%i", 0);
    return 0;
  }

  /* get the samples per frame */
  gd_spf_t spf = _GD_GetSPF(D, entry);

  if (D->error) {
    dreturn("%i", 0);
    return 0;
  }

  first_samp += spf * first_frame;
  num_samp += spf * num_frames;

  n_read = _GD_DoField(D, entry, repr, first_samp, num_samp, return_type,
      data_out);

  dreturn("%zi", n_read);
  return n_read;
}

/* 32(ish)-bit wrapper for the 64-bit version, when needed */
size_t getdata(DIRFILE* D, const char *field_code, off_t first_frame,
    off_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t return_type, void *data_out)
{
  return getdata64(D, field_code, first_frame, first_samp, num_frames, num_samp,
      return_type, data_out);
}
/* vim: ts=2 sw=2 et tw=80
*/
