PHP BINDINGS FOR GETDATA
========================

The PHP bindings for GetData provide PHP5 bindings to the C GetData Library.
The bindings are provided in a PHP extension (called "getdata") which also
defines all the GetData constants (GD_RDONLY, GD_E_OK, &c.).  The bindings
are configured using php-config(1), which should provide all necessary
configuration.  The install path can be changed by passing --with-php-dir to
./configure.

In PHP, the C API's DIRFILE object is represented by a Dirfile resource,
allocated by gd_open().  The DIRFILE associated with a Dirfile resource is
automatically discarded and de-allocated (see gd_discard(3)) when the resource
goes out of scope and is reclaimed by GC.  Typically this means that a
Dirfile opened with these bindings stays open only while the calling
script runs.  However, Dirfile resources can also be made persistent by
calling gd_popen() instead.  (See Persistent Database Connections in the PHP
manual for details on persistent resources.)

A persistent Dirfile resource is not collected by GC when it goes out of scope.
Call gd_discard or gd_close to explicitly delete it.  See the discussion of
gd_popen() below for more details on retrieving previously-created persistent
Dirfile resources.

In general when strings are passed to the extension, only that part of the
string up to the first NUL character will be considered.  The exception to this
is gd_popen() (q.v.)

Data Types
----------

In the extension, gd_entry_t structs are represented as associative arrays whose
keys are the same as the gd_entry_t structure members in the C API (see
gd_entry(3)), with the following exceptions:

* There is no "scalar_ind" key.  The value associated with the key "scalar"
  is an array of two-element arrays.  The first element of the two-element
  array is the scalar field code, and the second is the scalar index.

* Because the extension can determine at run-time the type of a value, there
  is no need to distinguish purely real from complex data in the entry
  arrays.  As a result, complex data will be stored in the values associated
  with "m", "b", "a", "dividend" when appropriate.  For the same reason, there
  is no "comp_scal" key.

Data can be returned by the extension either packed or unpacked.  Packed data
are returned as a string and can be later unpacked using the standard unpack()
function.  Unpacked data are returned as an array of the data type requested.
See the INI setting "getdata.unpack" below for ways of indicating whether
packed or unpacked data should be returned.

Although PHP does not support unsigned integers, if requested, the extension
will return unpacked unsigned data coerced to signed.  Be careful with its
interpretation.  For convenience, in addition to the standard GD_INT32,
GD_INT64, &c. data type symbols, the extension defines the constants GD_INT
and GD_FLOAT to the native PHP integer and floating point types.

Similarly, data vectors passed to the extension can be packed or unpacked.
Packed data require a GetData type code indicating the type of the packed data.
If a GetData type code is specified with unpacked data, that type will be used
internally to pass the data to the C API; if no type code is given with unpacked
data, a suitable type will be automatically picked by the extension.  Because
packed data can be passed through the extension without intermediate conversion,
it should be preferred when a choice is available.

Complex data (when not packed) is represented as a two-element array.  The
element indexed zero is the real part of the datum; the element indexed one
is the imaginary part.  If the "getdata.degrade_complex" INI setting (see below)
is true, complex data whose imaginary part is zero will be simply reported as
a real number, rather than the two-element array.

INI Setttings
-------------

The GetData extension defines two optional, boolean INI settings:

* getdata.unpack: If true, GetData will by default return data as an unpacked
    array, rather than a packed string.  All functions which return data allow
    overriding this setting on a call-by-call basis via their "unpack" argument.
    The default is false.

* getdata.degrade_complex: If true, when returning (unpacked) complex value
    data, values which are purely real will be represented simply as a floating
    point number.  If this is false, these values whill be returned as a
    two-element array whose second element (the imarginary part) is zero.  The
    default is true.

Both of these settings can be changed on the fly using the standard ini_set()
function.

Functions
---------

Unless otherwise indicated, functions in the extension return false on error,
regardless of their stated return type.  Functions which are specified to return
a bool return true on success, except as noted.

Most functions behave equivalently to their counterparts in the C API.  See the
corresponding manual page in the Unix manual for complete details.  A complete
list of available functions is given below.  Differences from the C API are
pointed out.

In the prototypes below, "data" can be either a string containing packed data
or else an array containing unpacked data; "number" can be any numeric type
(including a two-element array representing a complex number).

Optional arguments are given in square brackets with their default values.
The default value for the always-optional $unpack parameter is not given; the
default is the current value of the getdata.unpack INI setting (see above).
Some functions have several ways that they can be called.

* bool gd_add(resource $dirfile, array $entry)

    For LINCOM and POLYNOM entries, the "n_fields" and "poly_ord" keys of $entry
    are optional: the extension will use the other supplied parameters to
    deterimine these if not given.  If named scalars are used for parameters,
    the corresponding literal parameter can be omitted.  In most cases, $entry
    members which are one-element arrays can be replaced with a scalar.

* bool gd_add_alias(resource $dirfile, string $field_code, string $target,
        [ int $fragment_index = 0 ])

* bool gd_add_bit(resource $dirfile, string $field_code, string $in_field,
        int $bitnum, [ int $numbits = 1, int $fragment_index = 0 ])

* bool gd_add_carray(resource $dirfile, string $field_code, int $const_type,
        array $unpacked_data, [ int $fragment_index = 0 ])
  bool gd_add_carray(resource $dirfile, string $field_code, int $const_type,
        int $data_type, data $data, [ int $fragment_index = 0 ])

* bool gd_add_const(resource $dirfile, string $field_code, int $const_type,
        number $value)

* bool gd_add_divide(resource $dirfile, string $field_code, string $in_field1,
        string $in_field2, [ int $fragment_index = 0 ])

* bool gd_add_lincom(resource $dirfile, string $field_code, array $in_fields,
        array $m, array $b, [ int $fragment_index = 0 ])

* bool gd_add_linterp(resource $dirfile, string $field_code, string $in_field,
        string $table, [ int $fragment_index = 0 ])

* bool gd_add_mplex(resource $dirfile, string $field_code, string $in_field,
        string $count_field, int $count, [ int $period = 0,
        int $fragment_index = 0 ])

* bool gd_add_multiply(resource $dirfile, string $field_code, string $in_field1,
        string $in_field2, [ int $fragment_index = 0 ])

* bool gd_add_phase(resource $dirfile, string $field_code, string $in_field,
        int $shift, [ int $fragment_index = 0 ])

* bool gd_add_polynom(resource $dirfile, string $field_code, string $in_field,
        array $a, [ int $fragment_index = 0 ])

* bool gd_add_raw(resource $dirfile, string $field_code, int $data_type,
        int $spf, [ int $fragment_index = 0 ])

* bool gd_add_recip(resource $dirfile, string $field_code, string $in_field,
        number $dividend, [ int $fragment_index = 0])

* bool gd_add_sbit(resource $dirfile, string $field_code, string $in_field,
        int $bitnum, [ int $numbits = 1, int $fragment_index = 0 ])

* bool gd_add_spec(resource $dirfile, string $spec, [ int $fragment_index = 0 ])

* bool gd_add_string(resource $dirfile, string $field_code, string $value,
        [ int $fragment_index = 0 ])

* bool gd_add_window(resource $dirfile, string $field_code, string $in_field,
        string $check_field, int $windop, number $threshold,
        [ int $fragment_index = 0 ])

* string gd_alias_target(resource $dirfile, string $field_code)

* array gd_aliases(resource $dirfile, string $field_code)

* bool gd_alter_affixes(resource $dirfile, int $fragment_index, string $prefix,
        string $suffix)

    If $prefix and/or $suffix is null, no change will be made to that affix.  To
    remove an affix, use the empty string (as in the C API).

* bool gd_alter_bit(resource $dirfile, string $field_code,
        [ string $in_field = null, int $bitnum = null, int $numbits = null ])

    Parameters which are null (or not given) are left unchanged.

* bool gd_alter_carray(resource $dirfile, string $field_code,
        [ int $const_type = null, int $array_len = null ])

    Parameters which are null (or not given) are left unchanged.

* bool gd_alter_const(resource $dirfile, string $field_code,
        [ int $const_type = null ])

    Parameters which are null (or not given) are left unchanged.

* bool gd_alter_encoding(resource $dirfile, int $encoding, int $fragment_index,
        [ bool $recode = false ])

* bool gd_alter_endianness(resource $dirfile, int $byte_sex,
        int $fragment_index, [ bool $recode = false ])

* bool gd_alter_entry(resource $dirfile, string $field_code, array $entry,
        [ bool $recode = false ])

    Elements of $entry which are missing or set to null are left unchanged.

* bool gd_alter_frameoffset(resource $dirfile, int $offset, int $fragment_index,
        [ bool $recode = false ])

* bool gd_alter_divide(resource $dirfile, string $field_code,
        [ string $in_field1 = null, string $in_field2 = null ])

    Parameters which are null (or not given) are left unchanged.

* bool gd_alter_lincom(resource $dirfile, string $field_code,
        [ int $n_fields = null, array $in_fields = null, array $m = null,
        array $b = null ])

    Parameters which are null (or not given) are left unchanged.

* bool gd_alter_linterp(resource $dirfile, string $field_code,
    [ string $in_field, string $table, bool $rename = false ])

* bool gd_alter_mplex(resource $dirfile, string $field_code,
    [ string $in_field = null, string $count_field = null, int $count = null,
    int $period = null ])

* bool gd_alter_multiply(resource $dirfile, string $field_code,
        [ string $in_field1, string $in_field2 ])

    Parameters which are null (or not given) are left unchanged.

* bool gd_alter_phase(resource $dirfile, string $field_code,
        [ string $in_field = null, int $shift = null ])

* bool gd_alter_polynom(resource $dirfile, string $field_code,
        [ int $poly_ord = null, string $in_field = null, array $a = null ])

* bool gd_alter_protection(resource $dirfile, int $protection,
        int $fragment_index)

* bool gd_alter_raw(resource $dirfile, string $field_code,
        [ int $data_type = null, int $spf = null, bool $recode = false ])

    Parameters which are null (or not given) are left unchanged.

* bool gd_alter_recip(resource $dirfile, string $field_code,
        [ string $in_field = null, number $dividend = null ])

    Parameters which are null (or not given) are left unchanged.

* bool gd_alter_sbit(resource $dirfile, string $field_code,
        [ string $in_field = null, int $bitnum = null, int $numbits = null ])

    Parameters which are null (or not given) are left unchanged.

* bool gd_alter_spec(resource $dirfile, string $spec, [ bool $recode = false ])

* bool gd_alter_window(resource $dirfile, string $field_code,
        [ string $in_field = null, string $check_field = null,
        int $windop = null, number $threshold = null ])

* int gd_bof(resource $dirfile, string $field_code)

* int gd_carray_len(resource $dirfile, string $field_code)

* array gd_carrays(resource $dirfile, int $return_type, [ bool $unpack ])

      Returns an array of packed strings or unpacked arrays.

* bool gd_close(resource $dirfile)

      On success, the resource $dirfile is deleted.

* data gd_constants(resource $dirfile, int $return_type, [ bool $unpack ])

* bool gd_delete(resource $dirfile, string $field_code, [ int $flags = 0 ])

* bool gd_delete_alias(resource $dirfile, string $field_code,
        [ int $flags = 0 ])

* bool gd_desync(resource $dirfile, [ int $flags = 0 ])

      Returns true or false, as appropriate, on success, and null on error.

* int gd_dirfile_standards(resource $dirfile,
        [ int $version = GD_VERSION_CURRENT ])

* string gd_dirfilekey(resource $dirfile)

    This function, which has no counterpart in the C API, returns the dirfile
    key, which is simply the string passed to gd_open() or gd_popen() when
    the dirfile was opened.  Unlike most strings returned by the extension,
    the string returned may contain embedded NUL characters.  See gd_popen()
    for a discussion on the use of dirfile keys.  If you simply want the
    dirfile path, use gd_dirfilename() below.

* string gd_dirfilename(resource $dirfile)

* bool gd_discard(resource $dirfile)

      On success, the resource $dirfile is deleted.

* int gd_encoding(resource $dirfile, [ int $fragment_index = 0 ])

* int gd_endianness(resource $dirfile, [ int $fragment_index = 0 ])

* array gd_entry(resource $dirfile, string $field_code)

      See above for a description of the returned entry array.

* array gd_entry_list(resource $dirfile, [ string $parent = null,
        int $type = GD_ALL_ENTRIES, flags = 0 ])

* int gd_entry_type(resource $dirfile, string $field_code)

* int gd_eof(resource $dirfile, string $field_code)

* int gd_error(resorce $dirfile)

* string gd_error_string(resource $dirfile)

* array gd_field_list(resource $dirfile)

* array gd_field_list_by_type(resource $dirfile, int $type)

* int gd_flags(resource $dirfile, [ int $set = 0, int $reset = 0 ])

* bool gd_flush(resource $dirfile, [ string $field_code = null ])

* array gd_fragment_affixes(resource $dirfile, int $fragment_index)

* string gd_fragmentname(resoruce $dirfile, int $fragment_index)

* int gd_frameoffset(resource $dirfile, [ int $fragment_index = 0 ])

* float gd_framenum(resource $dirfile, string $field_code, float $value,
        [ int $start = 0, int $stop = 0 ])

      This actually wraps gd_framenum_subset(3).

* data gd_get_carray(resource $dirfile, string $field_code, int $return_type,
        [ int $start = 0, int $len = null, bool $unpack ])

      If $len is null (or not given), all values to the end of the CARRAY are
      returned.

* number gd_get_constant(resource $dirfile, string $field_code, int $return_type)

* string gd_get_string(resource $dirfile, string $field_code)

* data gd_getdata(resource $dirfile, string $field_code, int $first_frame,
        int $first_sample, int $num_frames, int $num_samples, int $return_type,
        [ bool $unpack ])

* bool gd_hidden(resource $dirfile, string $field_code)

      Returns true or false, as appropriate, on success, and null on error.

* bool gd_hide(resource $dirfile, string $field_code)

* int gd_include(resource $dirfile, string $path, int $parent_fragment,
        [ int $flags = 0, string $prefix = "", string $suffix = "" ])

      This actually wraps gd_include_affix(3).

* resource gd_invalid_dirfile(void)

      This returns a Dirfile resource associated with an invalid dirfile.
      There is no corresponding function that returns a persistent resource.

* string gd_linterp_tablename(resource $dirfile, string $field_code)

* bool gd_madd(resource $dirfile, string $parent, array $entry)

    For LINCOM and POLYNOM entries, the "n_fields" and "poly_ord" keys of $entry
    are optional: the extension will use the other supplied parameters to
    deterimine these if not given.  If named scalars are used for parameters,
    the corresponding literal parameter can be omitted.  In most cases, $entry
    members which are one-element arrays can be replaced with a scalar.

* bool gd_madd_alias(resource $dirfile, string $parent, string $field_code,
        string $target)

* bool gd_madd_bit(resource $dirfile, string $parent, string $field_code,
        string $in_field, int $bitnum, [ int $numbits = 1 ])

* bool gd_madd_carray(resource $dirfile, string $parent, string $field_code,
        int $const_type, array $unpacked_data)
  bool gd_madd_carray(resource $dirfile, string $parent, string $field_code,
        int $const_type, int $data_type, data $data)

* bool gd_madd_const(resource $dirfile, string $parent, string $field_code,
        int $const_type, number $value)

* bool gd_madd_divide(resource $dirfile, string $parent, string $field_code,
        string $in_field1, string $in_field2)

* bool gd_madd_lincom(resource $dirfile, string $parent, string $field_code,
        array $in_fields, array $m, array $b)

* bool gd_madd_linterp(resource $dirfile, string $parent, string $field_code,
        string $in_field, string $table)

* bool gd_madd_mplex(resource $dirfile, string $parent, string $field_code,
        string $in_field, string $count_field, int $count, [ int $period = 0 ])

* bool gd_madd_multiply(resource $dirfile, string $parent, string $field_code,
        string $in_field1, string $in_field2)

* bool gd_madd_phase(resource $dirfile, string $parent, string $field_code,
        string $in_field, int $shift)

* bool gd_madd_polynom(resource $dirfile, string $parent, string $field_code,
        string $in_field, array $a)

* bool gd_madd_recip(resource $dirfile, string $parent, string $field_code,
        string $in_field, number $dividend)

* bool gd_madd_sbit(resource $dirfile, string $parent, string $field_code,
        string $in_field, int $bitnum, [ int $numbits = 1 ])

* bool gd_madd_spec(resource $dirfile, string $spec, string $parent)

* bool gd_madd_string(resource $dirfile, string $parent, string $field_code,
        string $value)

* bool gd_madd_window(resource $dirfile, string $parent, string $field_code,
        string $in_field, string $check_field, int $windop, number $threshold)

* bool gd_malter_spec(resource $dirfile, string $psec, string $parent,
        [ bool $recode = false ])

* array gd_mcarrays(resource $dirfile, int $return_type, [ bool $unpack ])

      Returns an array of packed strings or unpacked arrays.

* data gd_mconstants(resource $dirfile, int $return_type, [ bool $unpack ])

* bool gd_metaflush(resource $dirfile)

* array gd_mfield_list(resource $dirfile, string $field_code)

* array gd_mfield_list_by_type(resource $dirfile, string $field_code, int $type)

* bool gd_move(resource $dirfile, string $field_code, $new_fragment,
        [ bool move_data = false ])

* bool gd_move_alias(resource $dirfile, string $field_code, $new_fragment)

* bool gd_mplex_lookback(resource $dirfile, int $lookback)

* array gd_mstrings(resource $dirfile, string $field_code)

* array gd_mvector_list(resource $dirfile, string $field_code)

* int gd_naliases(resource $dirfile, string $field_code)

* int gd_native_type(resource $dirfile, string $field_code)

* int gd_nentries(resource $dirfile, [ string $parent = null,
        int $type = GD_ALL_ENTRIES, flags = 0 ])

* int gd_nfields(resource $dirfile)

* int gd_nfields_by_type(resource $dirfile, int $type)

* int gd_nfragments(resource $dirfile)

* int gd_nframes(resource $dirfile)

* int gd_nmfields(resource $dirfile, string $field_code)

* int gd_nmfields_by_type(resource $dirfile, string $field_code, int $type)

* int gd_nmvectors(resource $dirfile, string $field_code)

* int gd_nvectors(resource $dirfile)

* resource gd_open(string $dirfilename, [ int $flags = 0,
        callable $callback = null, mixed $callback_data = null ])

      If specified (and non-null), $callback should be a string giving the
      name of a callback function.  $callback_data, if given, may be anything;
      it will be passed to the callback without inspection.

      The $callback should accept two parameters.  The first will be an
      associative array of parser data, the second will be the
      $callback_data passed to this function.

      This function only returns false in the case when it couldn't create a
      Dirfile resource; generally, a failed open will still return a Dirfile
      resource.  Use gd_error() to check for success.

      To open a Dirfile persistently, see gd_popen().

* int gd_parent_fragment(resource $dirfile, int $fragment_index)

* resource gd_popen(string $dirfilename, [ int $flags = 0,
        callable $callback = null, mixed $callback_data = null ])

      If specified (and non-null), $callback should be a string giving the
      name of a callback function.  $callback_data, if given, may be anything;
      it will be passed to the callback without inspection.

      The $callback should accept two parameters.  The first will be an
      associative array of parser data, the second will be the
      $callback_data passed to this function.

      This function only returns false in the case when it couldn't create a
      Dirfile resource; generally, a failed open will still return a Dirfile
      resource.  Use gd_error() to check for success.

      The returned resource will be persistent (ie. it won't be deleted when
      it goes out of scope.  If a persistent Dirfile is already open with the
      same $dirfilename (including embedded NULs), its resource will be returned
      and no new Dirfile will be opened.

* int gd_protection(resource $dirfile, int $fragment_index)

* bool gd_put_carray(resource $dirfile, string $field_code, array $unpacked_data)
  bool gd_put_carray(resource $dirfile, string $field_code, int $start,
        array $unpacked_data)
  bool gd_put_carray(resource $dirfile, string $field_code, int $data_type,
        string $packed_data)
  bool gd_put_carray(resource $dirfile, string $field_code, int $start,
        int $data_type, data $data)

* bool gd_put_constant(resource $dirfile, string $field_code, number $datum)

* bool gd_put_string(resource $dirfile, string $field_code, string $value)

* int gd_putdata(resource $dirfile, string $field_code, int $first_frame,
        int $first_sample, array $unpacked_data)
  int gd_putdata(resource $dirfile, string $field_code, int $first_frame,
        int $first_sample, int $data_type, data $data)

* bool gd_raw_close(resource $dirfile, [ string $field_code = null ])

* string gd_raw_filename(resource $dirfile, string $field_code)

* string gd_reference(resource $dirfile, [ string $field_code = null ])

* bool gd_rename(resource $dirfile, string $field_code, string new_name,
        [ int flags = 0 ])

* bool gd_rewrite_fragment(resource $dirfile,
        [ int $fragment_index = GD_ALL_FRAGMENTS ])

* int gd_seek(resource $dirfile, string $field_code, int $frame_num,
        int $sample_num, [ int $flags = 0 ])

* int gd_spf(resource $dirfile, string $field_code)

* array gd_strings(resource $dirfile)

* array gd_strtok(resource $dirfile, string $string)

      Unlike the C API function, this function fully tokenises the string and
      returns an array of string tokens.

* bool gd_sync(resource $dirfile, [ string $field_code = null ])

* int gd_tell(resource $dirfile, string $field_code)

* bool gd_unhide(resource $dirfile, string $field_code)

* bool gd_uninclude(resource $dirfile, int $fragment_index, [ bool $delete = false ])

* bool gd_validate(resource $dirfile, string $field_code)

* array gd_vector_list(resource $dirfile)

* bool gd_verbose_prefix(resource $dirfile, [ string $prefix = "" ])
