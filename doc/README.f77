FORTRAN 77 BINDINGS FOR GETDATA
===============================

This README describes the Fortran 77 bindings for the GetData library.  These
bindings consist of a Fortran compatibility library, 'libfgetdata' (written in
C) and a Fortran 77 source file, 'getdata.f' which defines useful Fortran
parameters and declares the external subroutines.

These bindings are designed to comply to the Fortran 77 standards.  As a result,
identifiers are limited to six characters.   The compatibility library will
take care of converting Fortran CHARACTER stings to C strings.  However, as a
result, when strings are passed to the compatibility library as arguments, the
length of the string must also be passed.

Because Fortran 77 handles neither pointers nor abstract data types, DIRFILE
pointers are not used to refer to dirfile instances.  Instead, an integer
dirfile unit number is used.  Space is available in the compatibility library
for only 1023 dirfile units.  If an application attempts to open more than 1023
dirfiles simultaneously, the compatibility library will emit an error message
on standard error and return an invalid dirfile unit number.  Passing an invalid
dirfile unit number to a subroutines which requires one as input (other than
GDCLOS, which will simply ignore it) will result in the call failing with error
code GD_EBD (= GD_E_BAD_DIRFILE, see below).

Including getdata.f (which will be installed in the same directory as getdata.h)
will define several convenient parameters including the DIRFILE flags, the data
type specifiers, and error codes.  See below for a complete list.  If your
Fortran 77 compiler supports the MIL STD 1753 (DoD Extension) INCLUDE statement
(which any remotely modern compiler should), you can include this file in your
Fortran program to define these constants.

All integer type parameters passed to the compatibility library are of type
INTEGER (i.e. the native size of the platform).  As a result, largefile support
is not be available in the Fortran 77 bindings.

All character string arguments require also an integer indicating the size of
the character buffer.  In cases where the bindings return a string value, the
value will not be returned if the string length supplied is too short.  In
these cases, the character string will be left untouched, but the integer
indicating the string length will be updated to indicate the required string
length.  The exception to this is GDESTR, which simply truncates the string
it outputs, as the C API does.

Available Subroutines
=====================

* GDENCS(support, encoding)

  Output:
    INTEGER support
  Input:
    INTEGER encoding

  This wraps gd_encoding_support(3).  When passed one of the GDE_xx encoding
  parameters, this subroutine will return GD_RW if the library can read from
  and write to that encoding, GD_RO if it can only read from the encoding,
  or -1 if neither reading or writing is supported.
  

Subroutines interacting with the database
-----------------------------------------

* GDOPEN(dirfile_unit, dirfilename, dirfilename_len, flags)
  
  Output:
    INTEGER dirfile_unit
  Input:
    INTEGER dirfilename_len, flags
    CHARACTER*<dirfilename_len> dirfilename

  This wraps gd_open(3), with the same input arguments (dirfilename_len should
  contain the string length of dirfilename).  It returns the dirfile unit number
  in dirfile_unit.  The flags should be a bitwise "or"d list of flag parameters
  (see below).  If no more dirfile unit numbers are available, it returns -1,
  otherwise, this behaves analogously to gd_open() itself: it returns a valid
  dirfile unit even in case of error.

* GDCOPN(dirfile_unit, dirfilename, dirfilename_len, flags, sehandler)

  Output:
    INTEGER dirfile_unit
  Input:
    INTEGER dirfilename_len, flags
    CHARACTER*<dirfilename_len> dirfilename
    EXTERNAL sehandler

  This wraps gd_cbopen(3), and behaves identically to GDOPEN, except for
  requiring the name of the callback subroutine as sehandler.  The callback
  subroutine should accept the following arguments:

    SUBROUTINE CALBCK(act, dirfile_unit, suberror, line)
    INTEGER act, dirfile_unit, suberror
    CHARACTER*(GD_MLL) line

  where GD_MLL is a integer parameter, defined in getdata.f, equal to the value
  of the C macro GD_MAX_LINE_LENGTH.  The callback subroutine may modify line,
  and should set act to one of the syntax handler action parameters (see below).
  If the callback subroutine fails to set act, the default action (GDSX_A =
  GD_SYNTAX_ABORT) will be assumed.  The possible values of suberror are also
  listed below.  If GDCOPN is passed zero as sehandler, no callback is set.

  The callback subroutine is wrapped by the Fortran 77 library to properly
  interface with GetData.  Other than GDCOPN, the only other subroutines which
  potentially could cause the callback subroutine to be called are GDINCL and
  GDINCA.  Use GDCLBK to change the callback function before calling GDINCL or
  GDINCA, if required.

* GDCLBK(dirfile_unit, callback)

  Input:
    INTEGER dirfile_unit
    EXTERNAL callback

  This wraps gd_parser_callback(3), setting the registered parser callback to
  the subroutine given.  The signature of this subroutine is given above under
  GDCOPN.  Unlike the C interface, this function cannot be used to remove a
  registered callback; use GDNOCB for that.

* GDNOCB(dirfile_unit)

  Input:
    INTEGER dirfile_unit

  This calls gd_parser_callback(3) to deregister a previous parser callback
  associated with the specified dirfile.  If it had none, this procedure does
  nothing.

* GDINVD(dirfile_unit)

  Output:
    INTEGER dirfile_unit

  This wraps gd_invalid_dirfile(3), and returns the unit number of a
  newly-created, invalid dirfile.  If no dirfile unit numbers were available,
  it returns -1.

* GDCLOS(dirfile_unit)

  Input:
    INTEGER dirfile_unit

  This wraps gd_close(3).  The argument is the dirfile unit to close.  In
  addition to closing the dirfile itself, this will also disassociate the
  supplied dirfile unit number, which may be subsequently returned by a
  subsequent call to GDOPEN.

* GDDSCD(dirfile_unit)

  Input:
    INTEGER dirfile_unit

  This wraps gd_discard(3), but otherwise behaves identically to GDCLOS.

* GDFLSH(dirfile_unit, field_code, field_code_len)

  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This wraps gd_flush(3).  If field_code_len is zero, the entire dirfile will be
  flushed, and field_code will be ignored.  Otherwise the field named by
  field_code will be flushed.

* GDSYNC(dirfile_unit, field_code, field_code_len)

  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This wraps gd_sync(3).  If field_code_len is zero, the entire dirfile will be
  synced, and field_code will be ignored.  Otherwise the field named by
  field_code will be synced.

* GDRCLO(dirfile_unit, field_code, field_code_len)

  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This wraps gd_raw_close(3).  If field_code_len is zero, the entire dirfile
  will be closed, and field_code will be ignored.  Otherwise the field named by
  field_code will be closed.

* GDMFLS(dirfile_unit)

  Input:
    INTEGER dirfile_unit

  This subroutine wraps gd_metaflush(3), and will cause metadata changes to be
  written to disk.

* GDDSYN(desync, dirfile_unit, flags)

  Output:
    INTEGER desync
  Input:
    INTEGER dirfile_unit, flags

  This wraps gd_desync(3), and sets desync to a non-zero value if the loaded
  dirfile has become desynchronised from the metadata on disk and will,
  optionally, reload the dirfile.

* GDFLAG(flags, dirfile_unit, set, reset)

  Output:
    INTEGER flags
  Input:
    INTEGER dirfile_unit, set, reset

  This wraps gd_flags(3).  The value of the flags after modification are
  returned in flags.

* GDTOKE(token, token_len, dirfile_unit, string, string_len)

  Output:
    CHARACTER*<token_len> token
  Input/Output:
    INTEGER token_len
  Input:
    INTEGER dirfile_unit, string_len
    CHARACTER*<string_len> string

  This wraps gd_strtok(3).  If string_len <= 0, the next token of the previously
  supplied string is returned in token (ie. NULL is passed to gd_strtok(3)),
  otherwise, the first token from string is returned, and the string is cached
  by GetData.  If the output token is longer than the supplied token_len, the
  actual length of the token is returned in token_len and token is unmodified.

* GDVBPX(dirfile_unit, prefix, prefix_len

  Input:
    INTEGER dirfile_unit, prefix_len
    CHARACTER*<prefix_len> prefix

  This wraps gd_verbose_prefix(3).  To remove a prefix, set prefix_len to
  zero, in which case prefix itself is ignored.

Subroutines interacting with data
---------------------------------

* GDGETD(n_read, dirfile_unit, field_code, field_code_len, first_frame,
  first_sample, num_frames, num_samples, return_type, data_out)

  Output:
    INTEGER n_read
    <datatype> data_out(n)
  Input:
    INTEGER dirfile_unit, field_code_len, first_frame, first_sample
    INTEGER num_frames, num_samples, return_type
    CHARACTER*<field_code_len> field_code

  This wraps getdata(3), with the same input arguments (field_code_len should
  contain the string length of the field_code).  The number of samples actually
  read is returned in n_read.  The return_type parameter should be one of the
  parameters defined in getdata.f (see below).  data_out must be of sufficient
  length and of appropriate data type width for the data returned.

* GDGTCA(dirfile_unit, field_code, field_code_len, return_type, data_out)

  Output:
    <datatype> data_out(array_len)
  Input:
    INTEGER dirfile_unit, field_code_len, return_type
    CHARACTER*<field_code_len> field_code

  This wraps gd_get_carray(3), with the same input arguments (field_code_len
  should contain the string length of the field_code).  The return_type
  parameter should be one of the parameters defined in getdata.f.  data_out must
  be of appropriate data type width and length for the data returned.

* GDGCAS(dirfile_unit, field_code, field_code_len, start, n, return_type,
  data_out)

  Output:
    <datatype> data_out(array_len)
  Input:
    INTEGER dirfile_unit, field_code_len, return_type, start, n
    CHARACTER*<field_code_len> field_code

  This wraps gd_get_carray_slice(3), with the same input arguments
  (field_code_len should contain the string length of the field_code).  The
  return_type parameter should be one of the parameters defined in getdata.f.
  data_out must be of appropriate data type width and length for the data
  returned.

* GDGTCO(dirfile_unit, field_code, field_code_len, return_type, data_out)

  Output:
    <datatype> data_out
  Input:
    INTEGER dirfile_unit, field_code_len, return_type
    CHARACTER*<field_code_len> field_code

  This wraps gd_get_constant(3), with the same input arguments (field_code_len
  should contain the string length of the field_code).  The return_type
  parameter should be one of the parameters defined in getdata.f.  data_out must
  be of appropriate data type width for the data returned.

* GDGTST(size, dirfile_unit, field_code, field_code_len, len, data_out)

  Output:
    INTEGER size
    CHARACTER*<len> data_out
  Input:
    INTEGER dirfile_unit, field_code_len, len
    CHARACTER*<field_code_len> field_code

  This wraps gd_get_string(3), with the same input arguments (field_code_len
  should contain the string length of the field_code).  The number of characters
  actually read is returned in size.  At most len characters will be returned.

* GDPUTD(n_wrote, dirfile_unit, field_code, field_code_len, first_frame,
  first_sample, num_frames, num_samples, data_type, data_in)

  Output:
    INTEGER n_wrote
  Input:
    INTEGER dirfile_unit, field_code_len, first_frame, first_sample
    INTEGER num_frames, num_samples, data_type
    CHARACTER*<field_code_len> field_code
    <datatype> data_out(n)

  This wraps gd_putdata(3), with the same input arguments (field_code_len should
  contain the string length of the field_code).  The number of samples actually
  written is returned in n_wrote.  The data_type parameter should be one of the
  parameters defined in getdata.f.  data_in must be of sufficient length and
  of appropriate data type width for the data input.

* GDPTCA(dirfile_unit, field_code, field_code_len, data_type, data_in)

  Input:
    INTEGER dirfile_unit, field_code_len, data_type
    CHARACTER*<field_code_len> field_code
    <datatype> data_in(array_len)

  This wraps gd_put_carray(3), with the same input arguments (field_code_len
  should contain the string length of the field_code).  The data_type parameter
  should be one of the parameters defined in getdata.f.

* GDPCAS(dirfile_unit, field_code, field_code_len, start, n, data_type, data_in)

  Input:
    INTEGER dirfile_unit, field_code_len, data_type
    CHARACTER*<field_code_len> field_code
    <datatype> data_in(n)

  This wraps gd_put_carray_slice(3), with the same input arguments
  (field_code_len should contain the string length of the field_code).  The
  data_type parameter should be one of the parameters defined in getdata.f.

* GDPTCO(dirfile_unit, field_code, field_code_len, data_type, data_in)

  Input:
    INTEGER dirfile_unit, field_code_len, data_type
    CHARACTER*<field_code_len> field_code
    <datatype> data_in

  This wraps gd_put_constant(3), with the same input arguments (field_code_len
  should contain the string length of the field_code).  The data_type parameter
  should be one of the parameters defined in getdata.f.

* GDPTST(dirfile_unit, field_code, field_code_len, len, data_out)

  Input:
    INTEGER dirfile_unit, field_code_len, len
    CHARACTER*<field_code_len> field_code
    CHARACTER*<len> data_in

  This wraps gd_put_string(3), with the same input arguments (field_code_len
  should contain the string length of the field_code, and len should contain the
  string length of data_in).

* GDVLDT(invalid, dirfile_unit, field_code, field_code_len)

  Output:
    INTEGER invalid
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine wraps gd_validate(3), and returns non-zero if there is a
  problem with the specified field.

* GDFNUM(framenum, dirfile_unit, field_code, field_code_len, value)

  Output:
    REAL*8 invalid
  Input:
    INTEGER dirfile_unit, field_code_len
    REAL*8 value
    CHARACTER*<field_code_len> field_code

  This subroutine wraps gd_framenum(3), and performs a reverse look-up on a
  field.

* GDFNSS(framenum, dirfile_unit, field_code, field_code_len, value, field_start,
  field_end)

  Output:
    REAL*8 invalid
  Input:
    INTEGER dirfile_unit, field_code_len, field_start, field_end
    REAL*8 value
    CHARACTER*<field_code_len> field_code

  This subroutine wraps gd_framenum_subset(3), and performs a reverse
  look-up on a field.

* GDSEEK(pos, dirfile_unit, field_code, field_code_len, frame_num, sample_num,
  flags)

  Output:
    INTEGER pos
  Input:
    INTEGER dirfile_unit, field_code_len, frame_num, sample_num, flags
    CHARACTER*<field_code_len> field_code

  This subroutine wraps gd_seek(3), and repositions the field pointer of the
  specified field.  It returns the new value of the field pointer.  The
  'flags' argument should be one of GDSK_S, GDSK_C, or GDSK_E, optionally
  bitwise or'd with GDSK_E.

* GDTELL(pos, dirfile_unit, field_code)

  Output:
    INTEGER pos
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine wraps gd_tell(3), and returns the current value of the field
  pointer for the specified field.

* GDMXLB(dirfile_unit, lookback)

  Input:
    INTEGER dirfile_unit, lookback

  This wraps gd_mplex_lookback(3).


Subroutines interacting with global metadata
--------------------------------------------

* GDNMAT(nentries, dirfile_unit, regex, regex_len, fragment, type, flags)

  Output:
    INTEGER nentries
  Input:
    INTEGER dirfile_unit, regex_len, type, flags
    CHARACTER*<regex_len> regex

  This wraps gd_match_entries(3).  It returns the number of entries in the
  dirfile satisfying the supplied criteria.  If regex_len is zero, regex itself
  is ignored.  To access the matched field names themselves, use GDMATN (see
  below).

* GDNENT(nentries dirfile_unit, parent, parent_len, type, flags)

  Output:
    INTEGER nentries
  Input:
    INTEGER dirfile_unit, parent_len, type, flags
    CHARACTER*<parent_len> parent
 
  This wraps gd_nentries(3).  It returns the number of entries in the dirfile
  satisfying the supplied criteria.  If parent_len is zero, parent itself is
  ignored, and top-level entries are considered.  Otherwise, meta-entries
  under parent are considered.

* GDNFLD(nfields, dirfile_unit)
* GDNFDT(nfields, dirfile_unit, type)
* GDNVEC(nvectors, dirfile_unit)
* GDNMFD(nfields, dirfile, parent, parent_l)
* GDNMFT(nfields, dirfile, parent, parent_l, type)
* GDNMVE(nvectors, dirfile, parent, parent_l)

  These procedures wrap, respectively, gd_nfields(3), gd_nfields_by_type(3),
  gd_nvectors(3), gd_nmfields(3), gd_nmfields_by_type(3), and gd_nmvectors(3),
  and are all special cases of GDNENT.

* GDNALS(nfields, dirfile_unit, field_code, field_code_len)

  Output:
    INTEGER nframes
  Input:
    INTEGER dirfile_unit
    CHARACTER*<field_code_len> field_code

  This wraps gd_naliases(3).  It returns the number of aliases of field_code
  (including field_code itself).

* GDMATX(entry_max, dirfile_unit, regex, regex_len, fragment, type, flags)

  Output:
    INTEGER entry_max
  Input:
    INTEGER dirfile_unit, regex_len, fragment, type, flags
    CHARACTER*<regex_len> regex

  This subroutine, which has no direct analogue in the C API, returns in
  entry_max the length (in characters) of the longest entry name in the
  dirfile in which satisfies the given criteria.  If regex_len is zero,
  regex itself is ignored.  See gd_match_entries(3).

* GDENTX(entry_max, dirfile_unit, parent, parent_len, type, flags)

  Output:
    INTEGER entry_max
  Input:
    INTEGER dirfile_unit, parent_len, type, flags
    CHARACTER*<parent_len> parent
 
  This subroutine, which has no direct analogue in the C API, returns in
  entry_max the length (in characters) of the longest entry name in the
  dirfile in which satisfies the given criteria.  If parent_len is zero,
  parent itself is ignored, and top-level entries are considered.
  Otherwise, meta-entries under parent are considered.
 
* GDFDNX(field_max, dirfile_unit)
* GDMFNX(field_max, dirfile_unit, parent, parent_len)

  These subroutines are special cases of GDENTX, with type and flags both
  equal to zero, and, in the case of GDFDNX, parent_len also zero.
 
* GDMATN(name, name_len, dirfile_unit, regex, regex_len, fragment, type,
  flags, entry_num)

  Output:
    CHARACTER*<name_len> name
  Input/Output:
    INTEGER name_len
  Input:
    INTEGER dirfile_unit, regex_len, fragment, type, flags, entry_num
    CHARACTER*<regex_len> regex

  This subroutine is the replacement for gd_match_entries(3).  It returns in
  name a Fortran 77 string containing the entry name of the entry indexed by
  entry_num (which is should be a number between 1 and the output of GDNMAT).
  If the name of the field is longer than name_len, it will return the actual
  length of the field in name_len and not modify the name argument.  If
  entry_num is out of range, name_len will be set to zero, and name will not be
  modified.

* GDENTN(name, name_len, dirfile_unit, parent, parent_len, type, flags,
  entry_num)

  Output:
    CHARACTER*<name_len> name
  Input/Output:
    INTEGER name_len
  Input:
    INTEGER dirfile_unit, parent_len, type, flags, entry_num
    CHARACTER*<parent_len> parent

  This subroutine is the replacement for gd_entry_list(3).  It returns in
  name a Fortran 77 string containing the entry name of the entry indexed by
  entry_num (which is should be a number between 1 and the output of GDNENT).
  If the name of the field is longer than name_len, it will return the actual
  length of the field in name_len and not modify the name argument.  If
  entry_num is out of range, name_len will be set to zero, and name will not be
  modified.

* GDFLDN(name, name_len, dirfile_unit, field_num)
* GDFDNT(name, name_len, dirfile_unit, type, field_num)
* GDVECN(name, name_len, dirfile_unit, field_num)
* GDMFDN(name, name_len, dirfile_unit, parent, parent_len, field_num)
* GDMFDT(name, name_len, dirfile_unit, parent, parent_len, type, field_num)
* GDMVEN(name, name_len, dirfile_unit, parent, parent_len, field_num)

  These subroutines are replacements for, respectively, gd_field_list(3),
  gd_field_list_by_type(3), gd_vector_list(3), gd_mfield_list(3),
  gd_mfield_list_by_type(3), and gd_mvector_list(3), and are all special cases
  of GDENTN.

* GDALSS(name, name_len, dirfile_unit, field_code, field_code_len, field_num)

  Output:
    CHARACTER*<name_len> name
  Input/Output:
    INTEGER name_len
  Input:
    INTEGER dirfile_unit, field_num, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine is the replacement for gd_aliases(3) and behaves in
  the same manner as GDFLDN.

* GDSTRX(string_max, dirfile_unit)

  Output:
    INTEGER string_max
  Input:
    INTEGER dirfile_unit

  This subroutine, which has no direct analogue in the C API, returns the length
  of the longest STRING field defined in the dirfile.  It takes the dirfile unit
  number as input and returns the length (in characters) of the longest STRING
  in the dirfile in string_max.
 
* GDMSTX(string_max, dirfile_unit, parent, parent_len)

  Output:
    INTEGER string_max
  Input:
    INTEGER dirfile_unit, parent_len
    CHARACTER*<parent_len> parent

  This subroutine, which has no direct analogue in the C API, returns the length
  of the longest STRING defined in the dirfile for META fields of the supplied
  parent field.  It returns the length (in characters) of the longest STRING
  META field for the supplied parent in string_max.
 
* GDCONS(value, dirfile_unit, return_type, field_num)

  Output:
    CHARACTER*<name_len> name
    <datatype> value
  Input:
    INTEGER dirfile_unit, field_num

  This subroutine is the replacement for gd_constants(3) and behaves in the same
  manner as GDFLDN.

* GDMCOS(value, dirfile_unit, parent, parent_len, return_type, field_num)

  Output:
    CHARACTER*<name_len> name
    <datatype> value
  Input:
    INTEGER dirfile_unit, field_num, parent_len
    CHARACTER*<parent_len> parent

  This subroutine is the replacement for gd_mconstants(3) and behaves in the
  same manner as GDFLDN.

* GDSTRS(value, value_len, dirfile_unit, field_num)

  Output:
    CHARACTER*<value_len> value
  Input/Output:
    INTEGER value_len
  Input:
    INTEGER dirfile_unit, field_num

  This subroutine is the replacement for gd_strings(3) and behaves in the same
  manner as GDSTRS.

* GDMSTS(value, value_len, dirfile_unit, parent, parent_len, field_num)

  Output:
    CHARACTER*<value_len> value
  Input/Output:
    INTEGER value_len
  Input:
    INTEGER dirfile_unit, field_num, parent_len
    CHARACTER*<parent_len> parent

  This subroutine is the replacement for gd_mstrings(3) and behaves in the same
  manner as GDFLDN.

* GDNFRM(nframes, dirfile_unit)

  Output:
    INTEGER nframes
  Input:
    INTEGER dirfile_unit

  This wraps gd_nframes(3).  It takes the dirfile unit number as input and
  returns the number of frames in the dirfile in nframes.

* GDALSX(alias_max, dirfile_unit, field_code, field_code_len)

  Output:
    INTEGER alias_max
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine, which has no direct analogue in the C API, returns the
  length of the longest alias defined in the dirfile for field_code.
 
* GDECNT(error_count, dirfile_unit)

  Output:
    INTEGER error_count
  Input:
    INTEGER dirfile_unit

  This subroutine wraps gd_error_count(3).  It takes the dirfile unit number as
  input and returns the number of errors encountered by GetData since the last
  call to this subroutine (or since the dirfile was first opened) in
  error_count.

* GDEROR(error, dirfile_unit)

  Output:
    INTEGER error
  Input:
    INTEGER dirfile_unit

  This subroutine wraps gd_error(3).  It takes the dirfile unit number as input
  and returns the error value arising from the last library call in error.  The
  value of error will equal one of the error codes defined in getdata.f.

* GDESTR(dirfile_unit, buffer, buffer_len)

  Output:
    CHARACTER*<buffer_len> buffer
  Input:
    INTEGER dirfile_unit, buffer_len

  This subroutine takes a dirfile unit as input and will write the error
  string returned by gd_error_string(3) in buffer, which is of length
  buffer_len.

* GDNFRG(nformats, dirfile_unit)

  Output:
    INTEGER nformats
  Input:
    INTEGER dirfile_unit

  This subroutine returns the number of format file fragments in the specified
  dirfile.

* GDNAME(name, name_len, dirfile_unit)

  Output:
    CHARACTER*<name_len> name
  Input/Output:
    INTEGER name_len
  Input:
    INTEGER dirfile_unit

  This wraps gd_dirfilename(3).  The name of the dirfile will be returned in
  name.  If the name of the dirfile is longer than name_len, it will return the
  actual length of the name in name_len and not modify the name argument.

* GDREFE(name, name_len, dirfile_unit, field_code, field_code_len)

  Output:
    CHARACTER*<name_len> name
  Input/Output:
    INTEGER name_len
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This wraps gd_reference(3).  The reference field will be set to field_code,
  unless field_code_len is zero, in which case the reference field will not be
  changed, and field_code will be ignored.  The name of the reference field will
  be returned in name.  If the name of the reference field is longer than
  name_len it will return the actual length of the field in name_len and not
  modify the name argument.

* GDSTDV(version, dirfile_unit)

  Input/Output:
    INTEGER version
  Input:
    INTEGER dirfile_unit

  This wraps gd_dirfile_standards(3).  It attempts to set the current Standards
  Version of the loaded dirfile to the value specified by version (which may
  equal the parameter GDSV_C indicating no change).  It then returns the
  current Version in version, or -1 on error.


Subroutines interacting with fragment metadata
----------------------------------------------

* GDFRGN(filename, filename_len, dirfile_unit, ind)

  Output:
    CHARACTER*<infield_len> infield
  Input/Output:
    INTEGER infield_len
  Input:
    INTEGER ind

  This subroutine returns the name of the format file fragment indexed by ind.
  If the name of the file is longer than filename_len, it will return the
  actual length of the filename in filename_len and not modify the filename
  argument.

* GDINCL(dirfile_unit, file, file_len, fragment_index, flags)

  Input:
    INTEGER dirfile_unit, field_code_len, fragment_index, flags
    CHARACTER*<file_len> file

  This subroutine wraps gd_include(3), and allows the inclusion of another
  format file fragment into the current dirfile.  This may call the registered
  callback subroutine, if any.  See the caveat in the description of GDCOPN
  above.

* GDINCA(dirfile_unit, file, file_len, fragment_index, prefix, prefix_len,
  suffix, suffix_len, flags)

  Input:
    INTEGER dirfile_unit, field_code_len, fragment_index, prefix_len,
    INTEGER suffix_len, flags
    CHARACTER*<file_len> file
    CHARACTER*<prefix_len> prefix
    CHARACTER*<suffix_len> suffix

  This subroutine wraps gd_include_affix(3), and allows the inclusion of another
  format file fragment into the current dirfile with the specified field code
  prefix and suffix.  This may call the registered callback subroutine, if any.
  See the caveat in the description of GDCOPN above.

* GDUINC(dirfile_unit, fragment, del)

  Input:
    INTEGER dirfile_unit, fragment, del

  This subroutine wraps gd_uninclude(3).  It removes the specified fragment
  from the dirfile.  If del is non-zero, the fragment file will be deleted.

* GDGENC(encoding, dirfile_unit, fragment_index)

  Output:
    INTEGER encoding
  Input:
    INTEGER dirfile_unit, fragment_index

  This subroutine wraps gd_encoding(3).  It returns the current encoding
  scheme of the specified fragment, which will be one of the symbols listed
  below.

* GDAENC(dirfile_unit, encoding, fragment, recode)

  Input:
    INTEGER dirfile_unit, encoding, fragment, recode

  This subroutine wraps gd_alter_encoding(3).  It sets the encoding scheme of
  the specified fragment to the value of the encoding parameter, which should
  be one of the encoding flags listed below.  If recode is non-zero, binary
  files associated with this fragment will be modified to compensate for the
  change.

* GDGEND(endianness, dirfile_unit, fragment_index)

  Output:
    INTEGER endianness
  Input:
    INTEGER dirfile_unit, fragment_index

  This subroutine wraps gd_endianness(3).  It returns the current byte sex
  of the specified fragment, which will be one of the symbols listed below.

* GDAEND(dirfile_unit, endianness, fragment, recode)

  Input:
    INTEGER dirfile_unit, endianness, fragment, recode

  This subroutine wraps gd_alter_endianness(3).  It sets the byte sex of the
  specified fragment to the value of the endianness parameter, which should be
  zero or a combination of GD_BE and GD_LE as described on the
  gd_alter_endianness manual page.  If recode is non-zero, binary files
  associated with this fragment will be modified to compensate for the change.

* GDGFOF(frame_offset, dirfile_unit, fragment_index)

  Output:
    INTEGER frame_offset
  Input:
    INTEGER dirfile_unit, fragment_index

  This subroutine wraps gd_frameoffset(3).  It returns the current frame
  offset of the specified fragment.

* GDAFOF(dirfile_unit, frame_offset, fragment, recode)

  Input:
    INTEGER dirfile_unit, frame_offset, fragment, recode

  This subroutine wraps gd_alter_frameoffset(3).  It sets the frame offset of
  the specified fragment to the value of the frame_offset parameter.  If recode
  is non-zero, binary files associated with this fragment will be modified to
  compensate for the change.

* GDGPRT(protection_level, dirfile_unit, fragment_index)

  Output:
    INTEGER protection_level
  Input:
    INTEGER dirfile_unit, fragment_index

  This subroutine wraps gd_protection(3).  It returns the current protection
  level of the specified fragment, which will be one of the symbols listed
  below.

* GDAPRT(dirfile_unit, protection_level, fragment)

  Input:
    INTEGER dirfile_unit, protection_level, fragment

  This subroutine wraps gd_protect(3).  It sets the protection level of the
  specified fragment to the value of the protection_level parameter, which
  should one of the protection level listed below.

* GDPFRG(parent, dirfile_unit, fragment)

  Output:
    INTEGER parent
  Input:
    INTEGER dirfile_unit, fragment

  This subroutine wraps gd_parent_fragment(3).  It returns the parent
  fragment of the specified fragment, or -1 on error.

* GDFRAF(prefix, prefix_len, suffix, suffix_len, dirfile_unit, fragment_index)
  
  Output:
    CHARACTER*<prefix_len> prefix
    CHARACTER*<suffix_len> suffix
  Input/Output:
    INTEGER prefix_len, suffix_len
  Input:
    INTEGER dirfile_unit, fragment_index

  This subroutine wraps gd_fragment_affixes(3), returning the field code
  prefix and suffix for the specified fragment.  On error, it sets prefix_len
  to zero.  In this case the value of the other output parameters in
  unspecified.

* GDAAFX(dirfile_unit, fragment, prefix, prefix_len, suffix, suffix_len

  Input:
    INTEGER dirfile_unit, fragment
    CHARACTER*<prefix_len> prefix
    CHARACTER*<suffix_len> suffix

  This subroutine wraps gd_alter_affix(3).  It sets the field code prefix and
  suffix of the specified fragment to the value of the prefix and suffix
  parameters.


Subroutines interacting with field metadata
-------------------------------------------

* GDGERW(spf, data_type, fragment_index, dirfile_unit, field_code,
  field_code_len)

  Output: 
    INTEGER spf, data_type, fragment_index
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine returns metadata describing a RAW field.  It returns the
  samples-per-frame, native data type, and the format file index in spf and
  data_type.  The data_type will be one of the data type parameters listed
  below.  If field_code is not found, or the field specified is not of RAW type,
  spf will be set to zero.  In this case the value of the other output
  parameters is unspecified.

* GDGECL(nfields, infield1, infield1_len, m1, b1, infield2, infield2_len, m2,
  b2, infield3, infield3_len, m3, b3, fragment_index, dirfile_unit, field_code,
  field_code_len)

  Output:
    INTEGER nfields, fragment_index
    CHARACTER*<infield1_len> infield1
    CHARACTER*<infield2_len> infield2
    CHARACTER*<infield3_len> infield3
    COMPLEX*16 m1, b1, m2, b2, m3, b3
  Input/Output:
    INTEGER infield1_len, infield2_len, infield3_len
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine returns metadata describing a LINCOM field.  Although three
  sets of arguments are required, only nfields of them will be updated.  If
  field_code is not found, or the field specified is not of LINCOM type, nfields
  will be set to zero.  In this case the value of the remaining data is
  unspecified.

* GDGELC(nfields, infield1, infield1_len, m1, b1, infield2, infield2_len, m2,
  b2, infield3, infield3_len, m3, b3, fragment_index, dirfile_unit, field_code,
  field_code_len)

  Output:
    INTEGER nfields, fragment_index
    CHARACTER*<infield1_len> infield1
    CHARACTER*<infield2_len> infield2
    CHARACTER*<infield3_len> infield3
    REAL*8 m1, b1, m2, b2, m3, b3
  Input/Output:
    INTEGER infield1_len, infield2_len, infield3_len
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This is equivalent to GDGELC above, but returns only the real part of the
  scale factors and offset terms.

* GDGELT(infield, infield_len, table, table_len, fragment_index, dirfile_unit,
  field_code, field_code_len)

  Output: 
    CHARACTER*<infield_len> infield
    CHARACTER*<table_len> table
    INTEGER fragment_index
  Input/Output:
    INTEGER infield_len, table_len
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine returns metadata describing a LINTERP field.  If field_code
  is not found, or the field specified is not of LINTERP type, infield_len will
  be set to zero.  In this case the value of the remaining data is unspecified.

* GDGEBT(infield, infield_len, bitnum, numbits, fragment_index, dirfile_unit,
  field_code, field_code_len)

  Output: 
    CHARACTER*<infield_len> infield
    INTEGER bitnum, numbits, fragment_index
  Input/Output:
    INTEGER infield_len
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine returns metadata describing a BIT field.  If field_code
  is not found, or the field specified is not of BIT type, infield_len will
  be set to zero.  In this case the value of the remaining data is unspecified.

* GDGESB(infield, infield_len, bitnum, numbits, fragment_index, dirfile_unit,
  field_code, field_code_len)

  Output: 
    CHARACTER*<infield_len> infield
    INTEGER bitnum, numbits, fragment_index
  Input/Output:
    INTEGER infield_len
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine returns metadata describing a SBIT field.  If field_code
  is not found, or the field specified is not of SBIT type, infield_len will
  be set to zero.  In this case the value of the remaining data is unspecified.

* GDGECR(infield, infield_len, dividend, fragment_index, dirfile_unit,
  field_code, field_code_len)

  Output: 
    CHARACTER*<infield_len> infield
    COMPLEX*16 dividend
    INTEGER fragment_index
  Input/Output:
    INTEGER infield_len
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine returns metadata describing a RECIP field.  If field_code
  is not found, or the field specified is not of RECIP type, infield_len will
  be set to zero.  In this case the value of the remaining data is unspecified.

* GDGERC(infield, infield_len, dividend, fragment_index, dirfile_unit,
  field_code, field_code_len)

  Output: 
    CHARACTER*<infield_len> infield
    REAL*8 dividend
    INTEGER fragment_index
  Input/Output:
    INTEGER infield_len
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine returns metadata describing a RECIP field.  If field_code
  is not found, or the field specified is not of RECIP type, infield_len will
  be set to zero.  In this case the value of the remaining data is unspecified.

* GDGEDV(infield1, infield1_len, infield2, infield2_len, fragment_index,
  dirfile_unit, field_code, field_code_len)

  Output: 
    CHARACTER*<infield1_len> infield1
    CHARACTER*<infield2_len> infield2
    INTEGER fragment_index
  Input/Output:
    INTEGER infield1_len, infield2_len
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine returns metadata describing a DIVIDE field.  If field_code
  is not found, or the field specified is not of DIVIDE type, infield1_len
  will be set to zero.  In this case the value of the remaining data is
  unspecified.

* GDGEMT(infield1, infield1_len, infield2, infield2_len, fragment_index,
  dirfile_unit, field_code, field_code_len)

  Output: 
    CHARACTER*<infield1_len> infield1
    CHARACTER*<infield2_len> infield2
    INTEGER fragment_index
  Input/Output:
    INTEGER infield1_len, infield2_len
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine returns metadata describing a MULTIPLY field.  If field_code
  is not found, or the field specified is not of MULTIPLY type, infield1_len
  will be set to zero.  In this case the value of the remaining data is
  unspecified.

* GDGEPH(infield, infield_len, shift, fragment_index, dirfile_unit, field_code,
  field_code_len)

  Output: 
    CHARACTER*<infield_len> infield
    INTEGER shift, fragment_index
  Input/Output:
    INTEGER infield_len
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine returns metadata describing a PHASE field.  If field_code
  is not found, or the field specified is not of PHASE type, infield_len will
  be set to zero.  In this case the value of the remaining data is unspecified.

* GDGEPN(poly_ord, infield, infield_len, a0, a1, a2, a3, a4, a5, fragment_index,
  dirfile_unit, field_code, field_code_len)

  Output:
    INTEGER poly_ord, fragment_index
    CHARACTER*<infield_len> infield
    REAL*8 a0, a1, a2, a3, a4, a5
  Input/Output:
    INTEGER infield1_len, infield2_len, infield3_len
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This is equivalent to GDGECP above, but returns only the real part of the
  coefficients.

* GDGECP(poly_ord, infield, infield_len, a0, a1, a2, a3, a4, a5, fragment_index,
  dirfile_unit, field_code, field_code_len)

  Output:
    INTEGER poly_ord, fragment_index
    CHARACTER*<infield_len> infield
    COMPLEX*16 a0, a1, a2, a3, a4, a5
  Input/Output:
    INTEGER infield1_len, infield2_len, infield3_len
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine returns metadata describing a POLYNOM field.  Although six
  coefficients must be provided, only poly_ord + 1 of them will be updated.  If
  field_code is not found, or the field specified is not of POLYNOM type,
  nfields will be set to zero.  In this case the value of the remaining data is
  unspecified.

* GDGEWD(infield, infield_len, checkfield, checkfield_len, windop, ithreshold,
  rthreshold, fragment_index, dirfile_unit, field_code, field_code_len)

  Output:
    INTEGER windop, ithreshold, fragment_index
    CHARACTER*<infield_len> infield
    CHARACTER*<checkfield_len> checkfield
  Input/Output:
    INTEGER infield_len, checkfield_len
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine returns metadata describing a WINDOW field.  Only one of
  ithreshold and rthreshold is ever updated.  If the returned windop is one of
  GDW_EQ, GDW_NE, GDW_ST, or GDW_CL, ithreshold will be updated, otherwise,
  rthreshold will be updated.  If field_code is not found, or the field
  specified is not of POLYNOM type, infield_len will be set to zero.  In this
  case the value of the remaining data is unspecified.

* GDGEMX(infield, infield_len, countfield, countfield_len, countval, period,
  fragment_index, dirfile_unit, field_code, field_code_len)

  Output:
    INTEGER countval, period, fragment_index
    CHARACTER*<infield_len> infield
    CHARACTER*<countfield_len> countfield
  Input/Output:
    INTEGER infield_len, countfield_len
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine returns metadata describing a MPLEX field.  If field_code
  is not found, or the field specified is not of MPLEX type, infield_len will
  be set to zero.  In this case the value of the remaining data is unspecified.

* GDGECA(const_type, array_len, fragment_index, dirfile_unit, field_code,
  field_code_len)

  Output: 
    INTEGER const_type, array_len, fragment_index
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine returns metadata describing a CARRAY field.  If field_code
  is not found, or the field specified is not of CARRAY type, const_type will
  be set to zero.  In this case the value of the remaining data is unspecified.

* GDGECO(const_type, fragment_index, dirfile_unit, field_code, field_code_len)

  Output: 
    INTEGER const_type, fragment_index
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine returns metadata describing a CONST field.  If field_code
  is not found, or the field specified is not of CONST type, const_type will
  be set to zero.  In this case the value of the remaining data is unspecified.

* GDARLN(array_len, dirfile_unit, field_code, field_code_len)

  Output:
    INTEGER array_len
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This wraps gd_array_len(3).  The field_code_len parameter should contain the
  string length of field_code.  The length of the field will be returned in
  array_len.

* GDGSPF(spf, dirfile_unit, field_code, field_code_len)

  Output:
    INTEGER spf
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This wraps gd_spf(3).  The field_code_len parameter should contain the
  string length of field_code.  The number of samples per frame in field_code
  will be returned in spf.

* GDENTY(entry_type, dirfile_unit, field_code, field_code_len)

  Output:
    INTEGER type
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine wraps gd_entry_type(3), and returns the field type of the
  specified field_code in entry_type.  The entry_type will be one of the entry
  type parameters listed below.

* GDFRGI(fragment_index, dirfile_unit, field_code, field_code_len)

  Output: 
    INTEGER fragment_index
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine wraps gd_fragment_index(3), and returns the format file
  fragment index for the supplied field.  If the field does not exist, or an
  error occurred, -1 is returned.

* GDATRG(targ, targ_len, dirfile_unit, field_code, field_code_len)

  Output:
    CHARACTER*<targ_len> targ
  Input/Output:
    INTEGER targ_len
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine wraps gd_alias_target(3), and returns the target of the
  alias specified by format_file.

* GDHIDN(result, dirfile_unit, field_code, field_code_len)

  Output:
    INTEGER result
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine wraps gd_hidden(3).  It sets result to one if the specified
  field is hidden, or zero if it is not.  On error, it sets result to -1.

* GDHIDE(dirfile_unit, field_code, field_code_len)

  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine wraps gd_hide(3).  It hides the specified field.

* GDUHID(dirfile_unit, field_code, field_code_len)

  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine wraps gd_unhide(3).  It unhides the specified field.

* GDGBOF(bof, dirfile_unit, field_code, field_code_len)

  Output:
    INTEGER bof
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine wraps gd_bof(3), and returns the location of the beginning-
  of-field marker for the specified field.

* GDGEOF(eof, dirfile, field_code, field_code_len)

  Output:
    INTEGER eof
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine wraps gd_eof(3), and returns the location of the end-of-field
  marker for the specified field.

* GDLSRW(dirfile_unit, field_code, field_code_len, data_type, spf, spf_scalar,
  spf_scalar_len, spf_scalar_index, recode)

  Input:
    INTEGER dirfile_unit, field_code_len, data_type, spf, spf_scalar_len
    INTEGER spf_scalar_index, recode
    CHARACTER*<field_code_len> field_code
    CHARACTER*<spf_scalar_len> spf_scalar

  This subroutine modifies the RAW field specified according to the supplied
  parameters.  Passing -1 as one of the spf_scalar_len will delete an existing
  scalar field code, if one is present.  Passing 0 for this parameter indicates
  no change to the corresponding parameter field code.  If recode is non-zero,
  the binary file associated with this field will be modified to account for the
  changes.

* GDALRW(dirfile_unit, field_code, field_code_len, data_type, spf, recode)

  Input:
    INTEGER dirfile_unit, field_code_len, data_type, spf, recode
    CHARACTER*<field_code_len> field_code

  This subroutine wraps gd_alter_raw(3), and modifies the specified field
  metadata.  If recode is non-zero, the binary file associated with this field
  will be modified to account for the changes.

* GDLSCL(dirfile_unit, field_code, field_code_len, nfields, in_field1,
  in_field1_len, m1, m1_scalar, m1_scalar_len, m1_scalar_index, b1, b1_scalar,
  b1_scalar_len, b1_scalar_index, in_field2, in_field2_len, m2, m2_scalar,
  m2_scalar_len, m2_scalar_index, b2, b2_scalar, b2_scalar_len, b2_scalar_index,
  in_field3, in_field3_len, m3, m3_scalar, m3_scalar_len, m3_scalar_index, b3,
  b3_scalar, b3_scalar_len, b3_scalar_index)

  Input:
    INTEGER dirfile_unit, field_code_len, nfields, in_field1_len, m1_scalar_len
    INTEGER m1_scalar_index, b1_scalar_len, b1_scalar_index, in_field2_len
    INTEGER m2_scalar_len, m2_scalar_index, b2_scalar_len, b2_scalar_index
    INTEGER in_field3_len, m3_scalar_len, m3_scalar_index, b3_scalar_len
    INTEGER b3_scalar_index
    COMPLEX*16 m1, b1, m2, b2, m3, b3
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field1_len> in_field1
    CHARACTER*<in_field2_len> in_field2
    CHARACTER*<in_field3_len> in_field3

  This subroutine modifies the LINCOM field specified according to the supplied
  parameters.  Passing -1 as one of the .._scalar_len parameters will delete
  an existing scalar field code, if one is present.  Passing 0 for these
  parameters indicates no change to the corresponding parameter field code.
  
* GDALCL(dirfile_unit, field_code, field_code_len, nfields, in_field1,
  in_field1_len, m1, b1, in_field2, in_field2_len, m2, b2, in_field3,
  in_field3_len, m3, b3)

  Input:
    INTEGER dirfile_unit, field_code_len, nfields, in_field1_len, in_field2_len
    INTEGER in_field3_len
    COMPLEX*16 m1, b1, m2, b2, m3, b3
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field1_len> in_field1
    CHARACTER*<in_field2_len> in_field2
    CHARACTER*<in_field3_len> in_field3

  This subroutine wraps gd_alter_lincom(3), and modifies the specified field
  metadata.

* GDLSLC(dirfile_unit, field_code, field_code_len, nfields, in_field1,
  in_field1_len, m1, m1_scalar, m1_scalar_len, m1_scalar_index, b1, b1_scalar,
  b1_scalar_len, b1_scalar_index, in_field2, in_field2_len, m2, m2_scalar,
  m2_scalar_len, m2_scalar_index, b2, b2_scalar, b2_scalar_len, b2_scalar_index,
  in_field3, in_field3_len, m3, m3_scalar, m3_scalar_len, m3_scalar_index, b3,
  b3_scalar, b3_scalar_len, b3_scalar_index)

  Input:
    INTEGER dirfile_unit, field_code_len, nfields, in_field1_len, m1_scalar_len
    INTEGER m1_scalar_index, b1_scalar_len, b1_scalar_index, in_field2_len
    INTEGER m2_scalar_len, m2_scalar_index, b2_scalar_len, b2_scalar_index
    INTEGER in_field3_len, m3_scalar_len, m3_scalar_index, b3_scalar_len
    INTEGER b3_scalar_index
    REAL*8 m1, b1, m2, b2, m3, b3
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field1_len> in_field1
    CHARACTER*<in_field2_len> in_field2
    CHARACTER*<in_field3_len> in_field3

  This is equivalent to GDLSCL above, but takes purely real parameters.

* GDALLC(dirfile_unit, field_code, field_code_len, nfields, in_field1,
  in_field1_len, m1, b1, in_field2, in_field2_len, m2, b2, in_field3,
  in_field3_len, m3, b3)

  Input:
    INTEGER dirfile_unit, field_code_len, nfields, in_field1_len, in_field2_len
    INTEGER in_field3_len
    REAL*8 m1, b1, m2, b2, m3, b3
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field1_len> in_field1
    CHARACTER*<in_field2_len> in_field2
    CHARACTER*<in_field3_len> in_field3

  This is equivalent to GDALCL above, but takes purely real parameters.

* GDALLT(dirfile_unit, field_code, field_code_len, in_field, in_field_len,
  table, table_len, move)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field_len, table_len, move
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field_len> in_field
    CHARACTER*<table_len> table

  This subroutine wraps gd_alter_linterp(3), and modifies the specified field
  metadata.  If move is non-zero, the look-up table will be moved to the path
  specified by table.

* GDLSBT(dirfile_unit, field_code, field_code_len, in_field, in_field_len,
  bitnum, bitnum_scalar, bitnum_scalar_len, bitnum_scalar_index,
  numbits, numbits_scalar, numbits_scalar_len, numbits_scalar_index)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field_len, bitnum
    INTEGER bitnum_scalar_len, bitnum_scalar_index, numbits, numbits_scalar_len
    INTEGER numbits_scalar_index
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field_len> in_field
    CHARACTER*<bitnum_scalar_len> bitnum_scalar_len
    CHARACTER*<numbits_scalar> numbits_scalar_len

  This subroutine modifies the BIT field specified according to the supplied
  parameters.  Passing -1 as bitnum_scalar_len or numbits_scalar_len will delete
  an existing scalar field code, if one is present.  Passing 0 for these
  parameters indicates no change to the corresponding parameter field code.

* GDALBT(dirfile_unit, field_code, field_code_len, in_field, in_field_len,
  bitnum, numbits)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field_len, bitnum, numbits
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field_len> in_field

  This subroutine wraps gd_alter_bit(3), and modifies the specified field
  metadata.

* GDALDV(dirfile_unit, field_code, field_code_len, in_field1, in_field1_len,
  in_field2, in_field2_len)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field1_len, in_field2_len
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field1_len> in_field1
    CHARACTER*<in_field2_len> in_field2

  This subroutine wraps gd_alter_divide(3), and modifies the specified field
  metadata.

* GDALMT(dirfile_unit, field_code, field_code_len, in_field1, in_field1_len,
  in_field2, in_field2_len)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field1_len, in_field2_len
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field1_len> in_field1
    CHARACTER*<in_field2_len> in_field2

  This subroutine wraps gd_alter_multiply(3), and modifies the specified field
  metadata.

* GDLSPH(dirfile_unit, field_code, field_code_len, in_field1, in_field1_len,
  shift, shift_scalar, shift_scalar_len, shift_scalar_index)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field1_len, shift, fragment_index
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field_len> in_field

  This subroutine modifies the PHASE field specified according to the supplied
  parameters.  Passing -1 as shift_scalar_len will delete an existing scalar
  field code, if one is present.  Passing 0 for this parameter indicates no
  change to the corresponding parameter field code.

* GDALPH(dirfile_unit, field_code, field_code_len, in_field1, in_field1_len,
  shift)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field1_len, shift, fragment_index
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field_len> in_field

  This subroutine wraps gd_alter_phase(3), and modifies the specified field
  metadata.

* GDLSCP(dirfile_unit, field_code, field_code_len, poly_ord, in_field,
  in_field_len, a0, a0_scalar, a0_scalar_len, a0_scalar_index, a1, a1_scalar,
  a1_scalar_len, a1_scalar_index, a2, a2_scalar, a2_scalar_len, a2_scalar_index,
  a3, a3_scalar, a3_scalar_len, a3_scalar_index, a4, a4_scalar, a4_scalar_len,
  a4_scalar_index, a5, a5_scalar, a5_scalar_len, a5_scalar_index)

  Input:
    INTEGER dirfile_unit, field_code_len, nfields, in_field_len, a0_scalar_len
    INTEGER a0_scalar_index, a1_scalar_len, a1_scalar_index, a2_scalar_len
    INTEGER a2_scalar_index, a3_scalar_len, a3_scalar_index, a4_scalar_len
    INTEGER a4_scalar_index, a5_scalar_len, a5_scalar_index
    COMPLEX*16 a0, a1, a2, a3, a4, a5
    CHARACTER*<field_code_len> field_code
    CHARACTER*<a0_scalar_len> a0_scalar
    CHARACTER*<a1_scalar_len> a1_scalar
    CHARACTER*<a2_scalar_len> a2_scalar
    CHARACTER*<a3_scalar_len> a3_scalar
    CHARACTER*<a4_scalar_len> a4_scalar
    CHARACTER*<a5_scalar_len> a5_scalar

  This subroutine modifies the POLYNOM field specified according to the supplied
  parameters.  Passing -1 as one of the .._scalar_len parameters will delete
  an existing scalar field code, if one is present.  Passing 0 for these
  parameters indicates no change to the corresponding parameter field code.

* GDALCP(dirfile_unit, field_code, field_code_len, poly_ord, in_field,
  in_field_len, a0, a1, a2, a3, a4, a5)

  Input:
    INTEGER dirfile_unit, field_code_len, nfields, in_field_len
    COMPLEX*16 a0, a1, a2, a3, a4, a5
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field_len> in_field

  This subroutine wraps gd_alter_polynom(3), and modifies the specified field
  metadata.

* GDLSPN(dirfile_unit, field_code, field_code_len, poly_ord, in_field,
  in_field_len, a0, a0_scalar, a0_scalar_len, a0_scalar_index, a1, a1_scalar,
  a1_scalar_len, a1_scalar_index, a2, a2_scalar, a2_scalar_len, a2_scalar_index,
  a3, a3_scalar, a3_scalar_len, a3_scalar_index, a4, a4_scalar, a4_scalar_len,
  a4_scalar_index, a5, a5_scalar, a5_scalar_len, a5_scalar_index)

  Input:
    INTEGER dirfile_unit, field_code_len, nfields, in_field_len, a0_scalar_len
    INTEGER a0_scalar_index, a1_scalar_len, a1_scalar_index, a2_scalar_len
    INTEGER a2_scalar_index, a3_scalar_len, a3_scalar_index, a4_scalar_len
    INTEGER a4_scalar_index, a5_scalar_len, a5_scalar_index
    REAL*8 a0, a1, a2, a3, a4, a5
    CHARACTER*<field_code_len> field_code
    CHARACTER*<a0_scalar_len> a0_scalar
    CHARACTER*<a1_scalar_len> a1_scalar
    CHARACTER*<a2_scalar_len> a2_scalar
    CHARACTER*<a3_scalar_len> a3_scalar
    CHARACTER*<a4_scalar_len> a4_scalar
    CHARACTER*<a5_scalar_len> a5_scalar

  This is equivalent to GDLSCP, but with purely real parameters.

* GDALPN(dirfile_unit, field_code, field_code_len, poly_ord, in_field,
  in_field_len, a0, a1, a2, a3, a4, a5)

  Input:
    INTEGER dirfile_unit, field_code_len, nfields, in_field_len
    REAL*8 a0, a1, a2, a3, a4, a5
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field_len> in_field

  This subroutine is equivalent to GDALCP above, but takes purely real
  parameters.

* GDLSSB(dirfile_unit, field_code, field_code_len, in_field, in_field_len,
  bitnum, bitnum_scalar, bitnum_scalar_len, bitnum_scalar_index,
  numbits, numbits_scalar, numbits_scalar_len, numbits_scalar_index)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field_len, bitnum
    INTEGER bitnum_scalar_len, bitnum_scalar_index, numbits, numbits_scalar_len
    INTEGER numbits_scalar_index
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field_len> in_field
    CHARACTER*<bitnum_scalar_len> bitnum_scalar_len
    CHARACTER*<numbits_scalar> numbits_scalar_len

  This subroutine modifies the SBIT field specified according to the supplied
  parameters.  Passing -1 as bitnum_scalar_len or numbits_scalar_len will delete
  an existing scalar field code, if one is present.  Passing 0 for these
  parameters indicates no change to the corresponding parameter field code.

* GDALSB(dirfile_unit, field_code, field_code_len, in_field, in_field_len,
  bitnum, numbits)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field_len, bitnum, numbits
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field_len> in_field

  This subroutine wraps gd_alter_sbit(3), and modifies the specified field
  metadata.

* GDLSCR(dirfile_unit, field_code, field_code_len, in_field, in_field_len,
  dividend, dividend_scalar, dividend_scalar_len, dividend_scalar_index)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field_len, dividend_scalar_len,
    INTEGER dividend_scalar_index
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field_len> in_field
    CHARACTER*<dividend_scalar_len> dividend_scalar
    COMPLEX*16 dividend

  This subroutine modifies the RECIP field specified according to the supplied
  parameters.  Passing -1 as dividend_scalar_len parameters will delete
  an existing scalar field code, if one is present.  Passing 0 for this
  parameter indicates no change to the corresponding parameter field code.

* GDALCR(dirfile_unit, field_code, field_code_len, in_field, in_field_len,
  dividend)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field_len
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field_len> in_field
    COMPLEX*16 dividend

  This subroutine wraps gd_alter_crecip(3), and modifies the specified field
  metadata.

* GDLSRC(dirfile_unit, field_code, field_code_len, in_field, in_field_len,
  dividend, dividend_scalar, dividend_scalar_len, dividend_scalar_index)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field_len, dividend_scalar_len,
    INTEGER dividend_scalar_index
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field_len> in_field
    CHARACTER*<dividend_scalar_len> dividend_scalar
    REAL*16 dividend

  This is equivalent to GDLSCR, but with purely real parameters.

* GDALRC(dirfile_unit, field_code, field_code_len, in_field, in_field_len,
  dividend)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field_len
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field_len> in_field
    REAL*8 dividend

  This subroutine wraps gd_alter_recip(3), and modifies the specified field
  metadata.

* GDLSWD(dirfile_unit, field_code, field_code_len, in_field, in_field_len,
  check_field, check_field_len, windop, threshold, threshold_scalar,
  threshold_scalar_len, threshold_scalar_index)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field_len, check_field_len, windop
    INTEGER threshold_scalar_len, threshold_scalar_index
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field_len> in_field
    CHARACTER*<check_field_len> check_field
    CHARACTER*<threshold_scalar_len> threshold_scalar
    INTEGER threshold
      or
    REAL*8 threshold

  This subroutine modifies the WINDOW field specified according to the supplied
  parameters.  Passing -1 as threshold_scalar_len will delete an existing scalar
  field code, if one is present.  Passing 0 for this parameter indicates no
  change to the corresponding parameter field code.  If windop is one of
  GDW_EQ, GDW_NE, GDW_ST, or GDW_CL, threshold should be of type integer,
  otherwise it should be double precision.

* GDALWD(dirfile_unit, field_code, field_code_len, in_field, in_field_len,
  check_field, check_field_len, windop, threshold)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field_len, check_field_len, windop
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field_len> in_field
    CHARACTER*<check_field_len> check_field
    INTEGER threshold
      or
    REAL*8 threshold

  This subroutine wraps gd_alter_windop(3), and modifies the specified field
  metadata.  If windop is one of GDW_EQ, GDW_NE, GDW_ST, or GDW_CL, threshold
  should be of type integer, otherwise it should be double precision.

* GDLSMX(dirfile_unit, field_code, field_code_len, infield, infield_len,
  countfield, countfield_len, countval, countval_scalar, countval_scalar_len,
  countval_scalar_index, period, period_scalar_len, period_scalar_index)

  Input:
    INTEGER dirfile_unit, field_code_len, infield_len, countfile_len, countval
    INTEGER countval_scalar_len, countval_scalar_index, period,
    INTEGER period_scalar_len, period_scalar_index
    CHARACTER*<field_code_len> field_code
    CHARACTER*<infield_len> infield
    CHARACTER*<countfield_len> countfield

  This subroutine modifies the MPLEX field specified according to the supplied
  parameters.  Passing -1 as one of the .._scalar_len parameters will delete
  an existing scalar field code, if one is present.  Passing 0 for these
  parameters indicates no change to the corresponding parameter field code.

* GDALMX(dirfile_unit, field_code, field_code_len, infield, infield_len,
  countfield, countfield_len, countval, period)

  Input:
    INTEGER dirfile_unit, field_code_len, infield_len, countfile_len, countval
    INTEGER period
    CHARACTER*<field_code_len> field_code
    CHARACTER*<infield_len> infield
    CHARACTER*<countfield_len> countfield

  This subroutine wraps gd_alter_mplex(3), and modifies the specified field
  metadata.

* GDALCA(dirfile_unit, field_code, field_code_len, const_type, array_len)

  Input:
    INTEGER dirfile_unit, field_code_len, const_type, array_len
    CHARACTER*<field_code_len> field_code

  This subroutine wraps gd_alter_carray(3), and modifies the specified field
  metadata.

* GDALCO(dirfile_unit, field_code, field_code_len, const_type)

  Input:
    INTEGER dirfile_unit, field_code_len, const_type
    CHARACTER*<field_code_len> field_code

  This subroutine wraps gd_alter_const(3), and modifies the specified field
  metadata.

* GDALSP(dirfile_unit, spec, spec_len, move)

  Input:
    INTEGER dirfile_unit, move, spec_len
    CHARACTER*<spec_len> spec

  This subroutine wraps gd_alter_spec(3), and modifies the specified field
  metadata.  If move is non-zero, and the field is a RAW field, the binary
  file will be modified.  If move is non-zero, and the field is a LINTERP,
  the look-up table will be moved.  Otherwise, move is ignored.

* GDMLSP(dirfile_unit, spec, spec_len, parent, parent_len, move)

  Input:
    INTEGER dirfile_unit, spec_len, parent_len, move
    CHARACTER*<spec_len> spec
    CHARACTER*<parent_len> parent

  This subroutine wraps gd_malter_spec(3), and behaves similarly to GDALSP, but
  also requires the name of the metafield's parent.  The spec should contain
  only the name of the metafield, and not the metafield's full field code.

* GDRWFN(name, name_len, dirfile_unit, field_code, field_code_len)

  Output:
    CHARACTER*<name_len> name
  Input/Output:
    INTEGER name_len
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine wraps gd_raw_filename(3).  It returns in name the name of
  the binary file associated with the raw field indicated by field_code.  On
  error, it sets name_len to zero.

* GDMOVE(dirfile_unit, field_code, field_code_len, new_fragment, flags)

  Input:
    INTEGER dirfile_unit, field_code_len, new_fragment, flags
    CHARACTER*<field_code_len> field_code

  This subroutine wraps gd_move(3), and moves the specified field or alias to
  indicated fragment.  The flags parameter should be zero or more of the GDR_xx
  parameters, bitwise or'd together.

* GDRENM(dirfile_unit, field_code, field_code_len, new_name, new_name_len,
  flags)

  Input:
    INTEGER dirfile_unit, field_code_len, new_name_len, flags
    CHARACTER*<field_code_len> field_code
    CHARACTER*<new_name_len> new_name

  This subroutine wraps gd_rename(3), and changes the name of a field.  The
  flags parameter should be zero or more of the GDR_xx parameters, bitwise or'd
  together.

* GDNTYP(ntype, dirfile_unit, field_code, field_code_len)

  Output:
    INTEGER ntype
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine wraps gd_native_type(3), and returns the native type of
  the specified field.  The return value will be one of the data type symbols
  listed below.

* GDENFL(flags, dirfile_unit, field_code, field_code_len)

  Output:
    INTEGER flags
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine returns the flags member of the specified field.

* GDGSCA(scalar, scalar_len, scalar_index, dirfile_unit, field_code,
  field_code_len, index)

  Output:
    CHARACTER*<scalar_len> scalar
    INTEGER scalar_index
  Input/Output:
    INTEGER scalar_len
  Input:
    INTEGER dirfile_unit, field_code_len, index
    CHARACTER*<field_code_len> field_code

  This subroutine returns the element indexed by index of the scalar array of
  the gd_entry_t object associated with the specified field code.  If index is
  too large for the specified field, behaviour is undefined.  The array is
  indexed starting from one.

* GDGACA(dirfile_unit, field_code, field_code_len, index, scalar, scalar_len,
  scalar_index, recode)

  Input:
    INTEGER dirfile_unit, field_code_len, scalar_len, index, recode
    INTEGER scalar_index
    CHARACTER*<field_code_len> field_code
    CHARACTER*<scalar_len> scalar

  This subroutine modifies the element indexed by index of the scalar array
  member of the gd_entry_t object associated with the specified field code.  If
  index is too large for the specified field, nothing happens.  The array is
  indexed starting from one.  If scalar indicates a CONST field, scalar_index
  is ignored.


Subroutines which add or delete fields and aliases
--------------------------------------------------

* GDDELE(dirfile_unit, field_code, field_code_len, flags)

  Input:
    INTEGER dirfile_unit, field_code_len, flags
    CHARACTER*<field_code_len> field_code

  This subroutine wraps gd_delete(3).  It deletes the indicated field or alias.
  The flags parameter should be either zero, or one or more of the delete flags
  listed below bitwise or'd together.

* GDADAL(dirfile_unit, field_code, field_code_len, targ, targ_len,
  fragment_index)

  Input:
    INTEGER dirfile_unit, field_code_len, targ_len, fragment_index
    CHARACTER*<field_code_len> field_code
    CHARACTER*<targ_len> targ

  This subroutine wraps gd_add_alias(3).  It adds an alias named field_code
  pointing to targ in the fragment indexed by fragment_index.

* GDASRW(dirfile_unit, field_code, field_code_len, data_type, spf,
  spf_scalar, spf_scalar_len, spf_scalar_index, fragment_index)

  Input:
    INTEGER dirfile_unit, field_code_len, data_type, spf, spf_scalar_len
    INTEGER spf_scalar_index, fragment_index
    CHARACTER*<field_code_len> field_code
    CHARACTER*<spf_scalar_len> spf_scalar

  This subroutine adds a RAW field with the supplied parameters to the
  specified fragment of the dirfile.  If spf_scalar_len is zero, spf_scalar and
  spf_scalar_index are ignored, and the literal value spf is used for the
  parameter.  If spf_scalar_len is non-zero, the literal spf is ignored and the
  field code specified by spf_scalar and spf_scalar_index is used for the
  parameter.

* GDADRW(dirfile_unit, field_code, field_code_len, data_type, spf,
  fragment_index)

  Input:
    INTEGER dirfile_unit, field_code_len, data_type, spf, fragment_index
    CHARACTER*<field_code_len> field_code

  This is equivalent to GDASRW with spf_scalar_len set to zero (i.e. with a
  literal parameter only).

* GDASCL(dirfile_unit, field_code, field_code_len, nfields, in_field1,
  in_field1_len, m1, m1_scalar, m1_scalar_len, m1_scalar_index, b1, b1_scalar,
  b1_scalar_len, b1_scalar_index, in_field2, in_field2_len, m2, m2_scalar,
  m2_scalar_len, m2_scalar_index, b2, b2_scalar, b2_scalar_len, b2_scalar_index,
  in_field3, in_field3_len, m3, m3_scalar, m3_scalar_len, m3_scalar_index, b3,
  b3_scalar, b3_scalar_len, b3_scalar_index, fragment_index)

  Input:
    INTEGER dirfile_unit, field_code_len, nfields, in_field1_len, m1_scalar_len
    INTEGER m1_scalar_index, b1_scalar_len, b1_scalar_index, in_field2_len
    INTEGER m2_scalar_len, m2_scalar_index, b2_scalar_len, b2_scalar_index
    INTEGER in_field3_len, m3_scalar_len, m3_scalar_index, b3_scalar_len
    INGEGER b3_scalar_index, fragment_index
    COMPLEX*16 m1, b1, m2, b2, m3, b3
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field1_len> in_field1
    CHARACTER*<m1_scalar_len> m1_scalar
    CHARACTER*<b1_scalar_len> b1_scalar
    CHARACTER*<in_field2_len> in_field2
    CHARACTER*<m2_scalar_len> m2_scalar
    CHARACTER*<b2_scalar_len> b2_scalar
    CHARACTER*<in_field3_len> in_field3
    CHARACTER*<m3_scalar_len> m3_scalar
    CHARACTER*<b3_scalar_len> b3_scalar

  This subroutine adds a LINCOM field with the supplied parameters to the
  specified format file fragment of the dirfile.  All three sets of input
  parameters are required to be passed to the call, but only the first
  nfields sets will be examined.  If a .._scalar_len parameter is zero, the
  corresponding .._scalar and .._scalar_index parameters are ignored, and the
  literal parameter (m1, b1, &c.) is used for that parameter.  If the
  .._scalar_len parameter is non-zero, the literal parameter is ignored, and the
  scalar is set to the field code specified by the corresponding .._scalar and
  .._scalar_index parameters.

* GDADCL(dirfile_unit, field_code, field_code_len, nfields, in_field1,
  in_field1_len, m1, b1, in_field2, in_field2_len, m2, b2, in_field3,
  in_field3_len, m3, b3, fragment_index)

  Input:
    INTEGER dirfile_unit, field_code_len, nfields, in_field1_len, in_field2_len
    INTEGER in_field3_len, fragment_index
    COMPLEX*16 m1, b1, m2, b2, m3, b3
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field1_len> in_field1
    CHARACTER*<in_field2_len> in_field2
    CHARACTER*<in_field3_len> in_field3

  This is equivalent to GDASCL with all the .._scalar_len parameters set to
  zero. (i.e. using literal scalars only.)

* GDASLC(dirfile_unit, field_code, field_code_len, nfields, in_field1,
  in_field1_len, m1, m1_scalar, m1_scalar_len, m1_scalar_index, b1, b1_scalar,
  b1_scalar_len, b1_scalar_index, in_field2, in_field2_len, m2, m2_scalar,
  m2_scalar_len, m2_scalar_index, b2, b2_scalar, b2_scalar_len, b2_scalar_index,
  in_field3, in_field3_len, m3, m3_scalar, m3_scalar_len, m3_scalar_index, b3,
  b3_scalar, b3_scalar_len, b3_scalar_index, fragment_index)

  Input:
    INTEGER dirfile_unit, field_code_len, nfields, in_field1_len, m1_scalar_len
    INTEGER m1_scalar_index, b1_scalar_len, b1_scalar_index, in_field2_len
    INTEGER m2_scalar_len, m2_scalar_index, b2_scalar_len, b2_scalar_index
    INTEGER in_field3_len, m3_scalar_len, m3_scalar_index, b3_scalar_len
    INGEGER b3_scalar_index, fragment_index
    REAL*8 m1, b1, m2, b2, m3, b3
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field1_len> in_field1
    CHARACTER*<m1_scalar_len> m1_scalar
    CHARACTER*<b1_scalar_len> b1_scalar
    CHARACTER*<in_field2_len> in_field2
    CHARACTER*<m2_scalar_len> m2_scalar
    CHARACTER*<b2_scalar_len> b2_scalar
    CHARACTER*<in_field3_len> in_field3
    CHARACTER*<m3_scalar_len> m3_scalar
    CHARACTER*<b3_scalar_len> b3_scalar

  This is equivalent to GDASCL above, but takes purely real parameters.

* GDADLC(dirfile_unit, field_code, field_code_len, nfields, in_field1,
  in_field1_len, m1, b1, in_field2, in_field2_len, m2, b2, in_field3,
  in_field3_len, m3, b3, fragment_index)

  Input:
    INTEGER dirfile_unit, field_code_len, nfields, in_field1_len, in_field2_len
    INTEGER in_field3_len, fragment_index
    REAL*8 m1, b1, m2, b2, m3, b3
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field1_len> in_field1
    CHARACTER*<in_field2_len> in_field2
    CHARACTER*<in_field3_len> in_field3

  This is equivalent to GDADCL above, but takes purely real parameters.

* GDADLT(dirfile_unit, field_code, field_code_len, in_field, in_field_len,
  table, table_len, fragment_index)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field_len, table_len
    INTEGER fragment_index
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field_len> in_field
    CHARACTER*<table_len> table

  This subroutine adds a LINTERP field with the supplied parameters to the
  specified format file fragment of the dirfile.

* GDASBT(dirfile_unit, field_code, field_code_len, in_field, in_field_len,
  bitnum, bitnum_scalar, bitnum_scalar_len, bitnum_scalar_index,
  numbits, numbits_scalar, numbits_scalar_len, numbits_scalar_index,
  fragment_index)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field_len, fragment_index
    INTEGER bitnum, bitnum_scalar_len, bitnum_scalar_index
    INTEGER numbits, numbits_scalar_len, numbits_scalar_index
    CHARACTER*<field_code_len> field_code
    CHARACTER*<bitnum_scalar_len> bitnum_scalar
    CHARACTER*<numbits_scalar_len> numbits_scalar
    CHARACTER*<in_field_len> in_field

  This subroutine adds a BIT field with the supplied parameters to the
  specified format file fragment of the dirfile.  If bitnum_scalar_len is zero,
  the bitnum_scalar and bitnum_scalar_index values are ignored and the literal
  value of bitnum is used.  If bitnum_scalar_len is non-zero, the value of
  bitnum is ignored and bitnum_scalar and bitnum_scalar_index is used to form
  a scalar field code for bitnum.  Similarly with numbits.  See also GDADBT.

* GDADBT(dirfile_unit, field_code, field_code_len, in_field, in_field_len,
  bitnum, numbits, fragment_index)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field_len, bitnum, numbits
    INTEGER fragment_index
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field_len> in_field

  This function is equivalent to calling GDASBT (above) with bitnum_scalar_len
  and numbits_scalar_len set to zero (i.e. with literal parameters only).

* GDASSB(dirfile_unit, field_code, field_code_len, in_field, in_field_len,
  bitnum, bitnum_scalar, bitnum_scalar_len, bitnum_scalar_index,
  numbits, numbits_scalar, numbits_scalar_len, numbits_scalar_index,
  fragment_index)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field_len, fragment_index
    INTEGER bitnum, bitnum_scalar_len, bitnum_scalar_index
    INTEGER numbits, numbits_scalar_len, numbits_scalar_index
    CHARACTER*<field_code_len> field_code
    CHARACTER*<bitnum_scalar_len> bitnum_scalar
    CHARACTER*<numbits_scalar_len> numbits_scalar
    CHARACTER*<in_field_len> in_field

  This subroutine adds a SBIT field with the supplied parameters to the
  specified format file fragment of the dirfile.  If bitnum_scalar_len is zero,
  the bitnum_scalar and bitnum_scalar_index values are ignored and the literal
  value of bitnum is used.  If bitnum_scalar_len is non-zero, the value of
  bitnum is ignored and bitnum_scalar and bitnum_scalar_index is used to form
  a scalar field code for bitnum.  Similarly with numbits.  See also GDADBT.

* GDADSB(dirfile_unit, field_code, field_code_len, in_field, in_field_len,
  bitnum, numbits, fragment_index)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field_len, bitnum, numbits
    INTEGER fragment_index
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field_len> in_field

  This function is equivalent to calling GDASSB (above) with bitnum_scalar_len
  and numbits_scalar_len set to zero (i.e. with literal parameters only).

* GDASCR(dirfile_unit, field_code, field_code_len, in_field1, in_field1_len,
  divident, divident_scalar, divident_scalar_len, divident_scalar_index,
  fragment_index)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field1_len
    INTEGER dividend_scalar_len, dividend_scalar_index, fragment_index
    COMPLEX*16 dividend
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field_len> in_field
    CHARACTER*<dividend_scalar_len> dividend_scalar.

  This subroutine adds a PHASE field with the supplied parameters to the
  specified fragment of the dirfile.  If dividend_scalar_len is zero,
  dividend_scalar and dividend_scalar_index are ignored, and the literal value
  dividend is used for the parameter.  If dividend_scalar_len is non-zero, the
  literal dividend is ignored and the field code specified by dividend_scalar
  and dividend_scalar_index is used for the parameter.

* GDADCR(dirfile_unit, field_code, field_code_len, in_field, in_field_len,
  dividend, fragment_index)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field_len
    INTEGER fragment_index
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field_len> in_field
    COMPLEX*16 dividend

  This is equivalent to GDASCR with dividend_scalar_len set to zero (i.e. with a
  literal parameter only).

* GDASRC(dirfile_unit, field_code, field_code_len, in_field1, in_field1_len,
  divident, divident_scalar, divident_scalar_len, divident_scalar_index,
  fragment_index)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field1_len
    INTEGER dividend_scalar_len, dividend_scalar_index, fragment_index
    REAL*8 dividend
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field_len> in_field
    CHARACTER*<dividend_scalar_len> dividend_scalar.

  This is equivalent to GDASCR, but with a purely real dividend.

* GDADRC(dirfile_unit, field_code, field_code_len, in_field, in_field_len,
  dividend, fragment_index)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field_len
    INTEGER fragment_index
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field_len> in_field
    REAL*8 dividend

  This is equivalent to GDASRC with dividend_scalar_len set to zero (i.e. with a
  literal parameter only).

* GDADMT(dirfile_unit, field_code, field_code_len, in_field1, in_field1_len,
  in_field2, in_field2_len, fragment_index)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field1_len, in_field2_len
    INTEGER fragment_index
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field1_len> in_field1
    CHARACTER*<in_field2_len> in_field2

  This subroutine adds a MULTIPLY field with the supplied parameters to the
  specified format file fragment of the dirfile.

* GDADDV(dirfile_unit, field_code, field_code_len, in_field1, in_field1_len,
  in_field2, in_field2_len, fragment_index)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field1_len, in_field2_len
    INTEGER fragment_index
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field1_len> in_field1
    CHARACTER*<in_field2_len> in_field2

  This subroutine adds a DIVIDE field with the supplied parameters to the
  specified format file fragment of the dirfile.

* GDASCP(dirfile_unit, field_code, field_code_len, poly_ord, in_field,
  in_field_len, a0, a0_scalar, a0_scalar_len, a0_scalar_index, a1, a1_scalar,
  a1_scalar_len, a1_scalar_index, a2, a2_scalar, a2_scalar_len, a2_scalar_index,
  a3, a3_scalar, a3_scalar_len, a3_scalar_index, a4, a4_scalar, a4_scalar_len,
  a4_scalar_index, a5, a5_scalar, a5_scalar_len, a5_scalar_index,
  fragment_index)

  Input:
    INTEGER dirfile_unit, field_code_len, poly_ord, in_field_len
    INTEGER a0_scalar_len, a0_scalar_index, a1_scalar_len, a1_scalar_index
    INTEGER a2_scalar_len, a2_scalar_index, a3_scalar_len, a3_scalar_index
    INTEGER a4_scalar_len, a4_scalar_index, a5_scalar_len, a5_scalar_index
    INTEGER fragment_index
    COMPLEX*16 a0, a1, a2, a3, a4, a5
    CHARACTER*<field_code_len> field_code
    CHARACTER*<a0_scalar_len> a0_scalar_len
    CHARACTER*<a1_scalar_len> a1_scalar_len
    CHARACTER*<a2_scalar_len> a2_scalar_len
    CHARACTER*<a3_scalar_len> a3_scalar_len
    CHARACTER*<a4_scalar_len> a4_scalar_len
    CHARACTER*<a5_scalar_len> a5_scalar_len
    CHARACTER*<in_field_len> in_field

  This subroutine adds a POLYNOM field with the supplied parameters to the
  specified format file fragment of the dirfile.  All six coefficients are
  required to be passed to the call, but only the first poly_ord + 1 will be
  examined.  If a .._scalar_len parameter is zero, the corresponding .._scalar
  and .._scalar_index parameters are ignored, and the literal parameter (a0, a1,
  &c.) is used for that parameter.  If the .._scalar_len parameter is non-zero,
  the literal parameter is ignored, and the scalar is set to the field code
  specified by the corresponding .._scalar and .._scalar_index parameters.

* GDADCP(dirfile_unit, field_code, field_code_len, poly_ord, in_field,
  in_field_len, a0, a1, a2, a3, a4, a5, fragment_index)

  Input:
    INTEGER dirfile_unit, field_code_len, poly_ord, in_field_len
    INTEGER fragment_index
    COMPLEX*16 a0, a1, a2, a3, a4, a5
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field_len> in_field

  This is equivalent to GDASCP with all the .._scalar_len parameters set to
  zero. (i.e. using literal scalars only.)

* GDASPN(dirfile_unit, field_code, field_code_len, poly_ord, in_field,
  in_field_len, a0, a0_scalar, a0_scalar_len, a0_scalar_index, a1, a1_scalar,
  a1_scalar_len, a1_scalar_index, a2, a2_scalar, a2_scalar_len, a2_scalar_index,
  a3, a3_scalar, a3_scalar_len, a3_scalar_index, a4, a4_scalar, a4_scalar_len,
  a4_scalar_index, a5, a5_scalar, a5_scalar_len, a5_scalar_index,
  fragment_index)

  Input:
    INTEGER dirfile_unit, field_code_len, poly_ord, in_field_len
    INTEGER a0_scalar_len, a0_scalar_index, a1_scalar_len, a1_scalar_index
    INTEGER a2_scalar_len, a2_scalar_index, a3_scalar_len, a3_scalar_index
    INTEGER a4_scalar_len, a4_scalar_index, a5_scalar_len, a5_scalar_index
    INTEGER fragment_index
    REAL*8 a0, a1, a2, a3, a4, a5
    CHARACTER*<field_code_len> field_code
    CHARACTER*<a0_scalar_len> a0_scalar_len
    CHARACTER*<a1_scalar_len> a1_scalar_len
    CHARACTER*<a2_scalar_len> a2_scalar_len
    CHARACTER*<a3_scalar_len> a3_scalar_len
    CHARACTER*<a4_scalar_len> a4_scalar_len
    CHARACTER*<a5_scalar_len> a5_scalar_len
    CHARACTER*<in_field_len> in_field

  This subroutine is equivalent GDASCP, but takes purely real parameters.

* GDADPN(dirfile_unit, field_code, field_code_len, poly_ord, in_field,
  in_field_len, a0, a1, a2, a3, a4, a5, fragment_index)

  Input:
    INTEGER dirfile_unit, field_code_len, poly_ord, in_field_len
    INTEGER fragment_index
    REAL*8 a0, a1, a2, a3, a4, a5
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field_len> in_field

  This subroutine is equivalent GDADCP, but takes purely real parameters.

* GDASPH(dirfile_unit, field_code, field_code_len, in_field1, in_field1_len,
  shift, shift_scalar, shift_scalar_len, shift_scalar_index, fragment_index)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field1_len, shift
    INTEGER shift_scalar_len, shift_scalar_index, fragment_index
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field_len> in_field
    CHARACTER*<shift_scalar_len> shift_scalar.

  This subroutine adds a PHASE field with the supplied parameters to the
  specified fragment of the dirfile.  If shift_scalar_len is zero, shift_scalar
  and shift_scalar_index are ignored, and the literal value shift is used for
  the parameter.  If shift_scalar_len is non-zero, the literal shift is ignored
  and the field code specified by shift_scalar and shift_scalar_index is used
  for the parameter.

* GDADPH(dirfile_unit, field_code, field_code_len, in_field1, in_field1_len,
  shift, fragment_index)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field1_len, shift, fragment_index
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field_len> in_field

  This is equivalent to GDASPH above with shift_scalar_len set to zer (i.e. with
  a literal parameter only).

* GDASWD(dirfile_unit, field_code, field_code_len, in_field, in_field_len,
  check_field, check_field_len, windop, threshold, threshold_scalar,
  threshold_scalar_len, threshold_scalar_index, fragment_index)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field_len, check_field_len
    INTEGER threshold_scalar_len, threshold_scalar_index, windop
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field_len> in_field
    CHARACTER*<check_field_len> check_field
    INTEGER threshold
      or
    REAL*8 threshold

  This subroutine adds a WINDOW field with the supplied parameters to the
  specified fragment of the dirfile.  If threshold_scalar_len is zero,
  threshold_scalar and threshold_scalar_index are ignored, and the literal value
  threshold is used for the parameter.  In this case, if windop is one of
  GDW_EQ, GDW_NE, GDW_ST, or GDW_CL, threshold should be an integer; otherwise,
  threshold should be a real.  If threshold_scalar_len is non-zero, the literal
  threshold is ignored and the field code specified by threshold_scalar and
  threshold_scalar_index is used for the parameter.

* GDADWD(dirfile_unit, field_code, field_code_len, in_field, in_field_len,
  check_field, check_field_len, windop, threshold, fragment_index)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field_len, check_field_len, windop
    INTEGER fragment_index
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field_len> in_field
    CHARACTER*<check_field_len> check_field
    INTEGER threshold
      or
    REAL*8 threshold

  This is equivalent to GDASWD with threshold_scalar_len set to zero (i.e. with
  a literal parameter only).

* GDASMX(dirfile_unit, field_code, field_code_len, infield, infield_len,
  countfield, countfield_len, countval, countval_scalar, countval_scalar_len,
  countval_scalar_index, period, period_scalar, period_scalar_len,
  period_scalar_index, fragment_index)

  Input:
    INTEGER dirfile_unit, field_code_len, infield_len, countfile_len, countval
    INTEGER countval_scalar_len, countval_scalar_index, period
    INTEGER period_scalar_len, period_scalar_index, fragment_index
    CHARACTER*<field_code_len> field_code
    CHARACTER*<infield_len> infield
    CHARACTER*<countfield_len> countfield
    CHARACTER*<countval_scalar_len> countval_scalar
    CHARACTER*<period_scalar_len> period_scalar

  This subroutine adds a MPLEX field with the supplied parameters to the
  specified fragment of the dirfile.  If countval_scalar_len is zero,
  countval_scalar and countval_scalar_index are ignored and the literal value
  countval is used for the parameter.  If countval_scalar_len is non-zero, the
  literal countval is ignored, and the field code specified by countval_scalar
  and countval_scalar_index are used for the parameter.  Similarly with period.

* GDADMX(dirfile_unit, field_code, field_code_len, infield, infield_len,
  countfield, countfield_len, countval, period, fragment_index)

  Input:
    INTEGER dirfile_unit, field_code_len, infield_len, countfile_len, countval
    INTEGER period, fragment_index
    CHARACTER*<field_code_len> field_code
    CHARACTER*<infield_len> infield
    CHARACTER*<countfield_len> countfield

  This subroutine is equivalent to GDASMX above with countval_scalar_len and
  period_scalar_len set to zero (i.e. with literal parameters only).

* GDADCA(dirfile_unit, field_code, field_code_len, const_type, array_len,
  data_type, data, fragment_index)

  Input:
    INTEGER dirfile_unit, field_code_len, const_type, data_type, fragment_index
    INTEGER array_len
    CHARACTER*<field_code_len> field_code
    <data_type> data(array_len)

  This subroutine adds a CARRAY field with the supplied parameters to the
  specified format file fragment of the dirfile.  const_type is the data type
  of the field when stored in the dirfile.  data_type is the data type of the
  supplied data.  These need not be the same.

* GDADCO(dirfile_unit, field_code, field_code_len, const_type, data_type,
  value, fragment_index)

  Input:
    INTEGER dirfile_unit, field_code_len, const_type, data_type, fragment_index
    CHARACTER*<field_code_len> field_code
    <data_type> value

  This subroutine adds a CONST field with the supplied parameters to the
  specified format file fragment of the dirfile.  const_type is the data type
  of the field when stored in the dirfile.  data_type is the data type of the
  supplied value.  These need not be the same.

* GDADST(dirfile_unit, field_code, field_code_len, value, value_len,
  fragment_index)

  Input:
    INTEGER dirfile_unit, field_code_len, value_len, fragment_index
    CHARACTER*<field_code_len> field_code
    CHARACTER*<value_len> value

  This subroutine adds a STRING field with the supplied parameters to the
  specified format file fragment of the dirfile

* GDADSP(dirfile_unit, spec, spec_len, fragment_index)

  Input:
    INTEGER dirfile_unit, fragment_index, spec_len
    CHARACTER*<spec_len> spec

  This subroutine wraps gd_add_spec(3), and allows adding a field to a dirfile
  given a field specification line.

* GDMDSP(dirfile_unit, spec, spec_len, parent, parent_len)

  Input:
    INTEGER dirfile_unit, spec_len, parent_len
    CHARACTER*<spec_len> spec
    CHARACTER*<parent_len> parent

  This subroutine wraps gd_madd_spec(3), and allows adding a metafield to a
  dirfile given a field specification line.

* GDMDAL(dirfile_unit, parent, parent_len, field_code, field_code_len, targ,
  targ_len)
* GDMDBT(dirfile_unit, parent, parent_len, field_code, field_code_len, in_field,
  in_field_len, bitnum, numbits)
* GDMDCL(dirfile_unit, parent, parent_len, field_code, field_code_len, nfields,
  in_field1, in_field1_len, m1, b1, in_field2, in_field2_len, m2, b2, in_field3,
  in_field3_len, m3, b3)
* GDFACO(dirfile_unit, parent, parent_len, field_code, field_code_len,
  const_type, data_type, value)
* GDMDCP(dirfile_unit, parent, parent_len, field_code, field_code_len, poly_ord,
  int_field, in_field_len, a0, a1, a2, a3, a4, a5)
* GDMDCR(dirfile_unit, parent, parent_len, field_code, field_code_len, in_field,
  in_field_len, dividend)
* GDMDDV(dirfile_unit, parent, parent_len, field_code, field_code_len,
  in_field1, in_field1_len, in_field2, in_field2_len)
* GDMDLC(dirfile_unit, parent, parent_len, field_code, field_code_len, nfields,
  in_field1, in_field1_len, m1, b1, in_field2, in_field2_len, m2, b2, in_field3,
  in_field3_len, m3, b3)
* GDMDLT(dirfile_unit, parent, parent_len, field_code, field_code_len, in_field,
  in_field_len, table, table_len)
* GDMDMT(dirfile_unit, parent, parent_len, field_code, field_code_len,
  in_field1, in_field1_len, in_field2, in_field2_len)
* GDMDPH(dirfile_unit, parent, parent_len, field_code, field_code_len,
  in_field, in_field_len, shift)
* GDMDPN(dirfile_unit, parent, parent_len, field_code, field_code_len, poly_ord,
  int_field, in_field_len, a0, a1, a2, a3, a4, a5)
* GDMDRC(dirfile_unit, parent, parent_len, field_code, field_code_len, in_field,
  in_field_len, dividend)
* GDMDSB(dirfile_unit, parent, parent_len, field_code, field_code_len, in_field,
  in_field_len, bitnum, numbits)
* GDMDWD(dirfile_unit, parent, field_code, field_code_len, in_field,
  in_field_len, check_field, check_field_len, windop, threshold)
* GDMDMX(dirfile_unit, parent, field_code, field_code_len, in_field,
  in_field_len, count_field, count_field_len, count_val, period)
* GDMDCA(dirfile_unit, parent, parent_len, field_code, field_code_len,
  const_type, array_len, data_type, data)
* GDMDCO(dirfile_unit, parent, parent_len, field_code, field_code_len,
  const_type, data_type, value)
* GDMDST(dirfile_unit, parent, parent_len, field_code, field_code_len, value,
  value_len)

  These functions are the corresponding META field functions for the GDADxx
  functions above. They add META fields to the parent field indicated.

Defined Parameters
==================

The following parameters, listed here with their C library analogues, are
defined in getdata.f which may be included in any Fortran program using the
Fortran 77 bindings.

Error codes (returned by GDEROR):

  F77 symbol      C symbol             Notes
  ----------      -------------------  ---------------------------------------
  GD_EOK          GD_E_OK              This is guaranteed to be equal to zero.
  GD_EAC          GD_E_ACCMODE
  GD_EAL          GD_E_ALLOC
  GD_EAR          GD_E_ARGUMENT
  GD_EBC          GD_E_BAD_CODE
  GD_EBD          GD_E_BAD_DIRFILE
  GD_EBE          GD_E_BAD_ENTRY
  GD_EBF          GD_E_BAD_FIELD_TYPE
  GD_EBI          GD_E_BAD_INDEX
  GD_EBO          GD_E_BOUNDS
  GD_EBP          GD_E_BAD_PROTECTION  Deprecated; kept as an alias for GD_EAR.
  GD_EBR          GD_E_BAD_REFERENCE
  GD_EBS          GD_E_BAD_SCALAR
  GD_EBT          GD_E_BAD_TYPE
  GD_ECB          GD_E_CALLBACK
  GD_ECR          GD_E_CREAT
  GD_EDL          GD_E_DELETE
  GD_EDM          GD_E_DIMENSION
  GD_EDO          GD_E_DOMAIN
  GD_EDU          GD_E_DUPLICATE
  GD_EEN          GD_E_BAD_ENDIANNESS  Deprecated; kept as an alias for GD_EAR.
  GD_EEX          GD_E_EXISTS
  GD_EFL          GD_E_FLUSH
  GD_EFO          GD_E_FORMAT
  GD_EIE          GD_E_INTERNAL_ERROR
  GD_EOF          GD_E_OPEN_FRAGMENT
  GD_EOI          GD_E_OPEN_INCLUDE    Deprecated; kept as an alias for GD_EOF.
  GD_EOL          GD_E_OPEN_LINFILE
  GD_EOP          GD_E_OPEN
  GD_EPT          GD_E_PROTECTED
  GD_ERA          GD_E_RANGE
  GD_ERL          GD_E_RECURSE_LEVEL
  GD_ERP          GD_E_BAD_REPR
  GD_ERW          GD_E_RAW_IO
  GD_ETL          GD_E_LINE_TOO_LONG
  GD_ETR          GD_E_TRUNC
  GD_EUE          GD_E_UNKNOWN_ENCODING
  GD_EVR          GD_E_BAD_VERSION    Deprecated; kept as an alias for GD_EAR.
  GD_UCL          GD_E_UNCLEAN_DB
  GD_UNS          GD_E_UNSUPPORTED

Dirfile flags (required by GDOPEN, GDCOPN, GDINCL, GDINCA, and GDFLAG):

  F77 symbol      C symbol          Notes
  ----------      ----------------- --------------------------------------
  GD_RO           GD_RDONLY         The flags argument passed to GDOPEN
  GD_RW           GD_RDWR           must contain at least GD_RO or GD_RW

  GD_AE           GD_ARM_ENDIAN
  GD_BE           GD_BIG_ENDIAN
  GD_CR           GD_CREAT
  GD_EX           GD_EXCL
  GD_FC           GD_FORCE_ENCODING
  GD_FE           GD_FORCE_ENDIAN
  GD_ID           GD_IGNORE_DUPS
  GD_IR           GD_IGNORE_REFS
  GD_LE           GD_LITTLE_ENDIAN
  GD_NA           GD_NOT_ARM_ENDIAN
  GD_PE           GD_PEDANTIC
  GD_PM           GD_PERMISSIVE
  GD_PP           GD_PRETTY_PRINT
  GD_TR           GD_TRUNC
  GD_TS           GD_TRUNCSUB
  GD_VB           GD_VERBOSE

Encoding types:

  F77 symbol      C symbol          Notes
  ----------      ----------------- --------------------------------------
  GDE_AU          GD_AUTO_ENCODED
  GDE_BZ          GD_BZIP2_ENCODED
  GDE_GZ          GD_GZIP_ENCODED
  GDE_LZ          GD_LZMA_ENCODED
  GDE_SL          GD_SLIM_ENCODED
  GDE_SI          GD_SIE_ENCODED
  GDE_TX          GD_TEXT_ENCODED
  GDE_UN          GD_UNENCODED
  GDE_ZS          GD_ZZSLIM_ENCODED
  GDE_ZZ          GD_ZZIP_ENCODED

Entry types (required by GDFLDT):

  F77 symbol      C symbol          Notes
  ----------      ----------------- --------------------------------------
  GD_NOE          GD_NO_ENTRY       Indicating an invalid field type
  GD_RWE          GD_RAW_ENTRY
  GD_LCE          GD_LINCOM_ENTRY
  GD_LTE          GD_LINTERP_ENTRY
  GD_BTE          GD_BIT_ENTRY
  GD_MTE          GD_MULTIPLY_ENTRY
  GD_PHE          GD_PHASE_ENTRY
  GD_IXE          GD_INDEX_ENTRY
  GD_PNE          GD_POLYNOM_ENTRY
  GD_SBE          GD_SBIT_ENTRY
  GD_DVE          GD_DIVIDE_ENTRY
  GD_RCE          GD_RECIP_ENTRY
  GD_WDE          GD_WINDOW_ENTRY
  GD_MXE          GD_MPLEX_ENTRY
  GD_COE          GD_CONST_ENTRY
  GD_CAE          GD_CARRAY_ENTRY
  GD_STE          GD_STRING_ENTRY

Data types.  Note, Fortran does not support unsigned data types, but GDGERW may
still return an unsigned type, so all types are defined here.  The unsigned data
type specifiers will be accepted by the other subroutines, but the data returned
may not be properly interpretable by Fortran 77.

  F77 symbol      C symbol          Notes
  ----------      ----------------- --------------------------------------
  GD_NUL          GD_NULL           Not suitable to be passed to GDPUTD
  GD_U8           GD_UINT8
  GD_I8           GD_INT8
  GD_U16          GD_UINT16
  GD_I16          GD_INT16
  GD_U32          GD_UINT32
  GD_I32          GD_INT32
  GD_U64          GD_UINT64
  GD_I64          GD_INT64
  GD_F32          GD_FLOAT32
  GD_F64          GD_FLOAT64
  GD_C64          GD_COMPLEX64
  GDC128          GD_COMPLEX128

Delete flags (required by GDDELE):

  F77 symbol      C symbol
  ----------      -----------------
  GDD_MT          GD_DEL_META
  GDD_DT          GD_DEL_DATA
  GDD_DR          GD_DEL_DEREF
  GDD_FO          GD_DEL_FORCE

Rename flags (requred by GDRENM):

  F77 symbol      C symbol
  ----------      -----------------
  GDR_DT          GD_REN_DATA
  GDR_UP          GD_REN_UPDB

Protection levels (returned by GDGPRT and required by GDAPRT):

  F77 symbol      C symbol          Notes
  ----------      ----------------- --------------------------------------
  GDPR_N          GD_PROTECT_NONE
  GDPR_F          GD_PROTECT_FORMAT
  GDPR_D          GD_PROTECT_DATA
  GDPR_A          GD_PROTECT_ALL    This is the bitwise or of GDPR_D and GDPR_A

Syntax error handler actions (returned by the registered callback function, see
GDCOPN)

  F77 symbol      C symbol
  ----------      -------------------
  GDSX_A          GD_PROTECT_ABORT
  GDSX_S          GD_PROTECT_RESCAN
  GDSX_I          GD_PROTECT_IGNORE
  GDSX_C          GD_PROTECT_CONTINUE

Syntax suberrors (provided to the registered callback function, see GDCOPN):

  F77 symbol      C symbol
  ----------      ---------------------
  GDF_AL          GD_E_FORMAT_ALIAS
  GDF_BN          GD_E_FORMAT_BITNUM
  GDF_CH          GD_E_FORMAT_CHARACTER
  GDF_DU          GD_E_FORMAT_DUPLICATE
  GDF_EN          GD_E_FORMAT_ENDIAN
  GDF_LI          GD_E_FORMAT_BAD_LINE
  GDF_LO          GD_E_FORMAT_LOCATION
  GDF_LT          GD_E_FORMAT_LITERAL
  GDF_MM          GD_E_FORMAT_META_META
  GDF_MR          GD_E_FORMAT_METARAW
  GDF_MV          GD_E_FORMAT_MPLEXVAL
  GDF_NA          GD_E_FORMAT_BAD_NAME
  GDF_NB          GD_E_FORMAT_NUMBITS
  GDF_NF          GD_E_FORMAT_N_FIELDS
  GDF_NT          GD_E_FORMAT_N_TOK
  GDF_PA          GD_E_FORMAT_NO_PARENT
  GDF_PR          GD_E_FORMAT_PROTECT
  GDF_RN          GD_E_FORMAT_RES_NAME
  GDF_SF          GD_E_FORMAT_BAD_SPF
  GDF_SZ          GD_E_FORMAT_BITSIZE
  GDF_TY          GD_E_FORMAT_BAD_TYPE
  GDF_UM          GD_E_FORMAT_UNTERM
  GDF_WO          GD_E_FORMAT_WINDOP

Special version symbols:

  F77 symbol      C symbol
  ----------      ---------------------
  GDSV_C          GD_VERSION_CURRENT
  GDSV_L          GD_VERSION_LATEST
  GDSV_E          GD_VERSION_EARLIEST

Seek flags:

  F77 symbol      C symbol
  ----------      ---------------------
  GDSK_C          GD_SEEK_CUR
  GDSK_E          GD_SEEK_END
  GDSK_P          GD_SEEK_PAD
  GDSK_S          GD_SEEK_SET

WINDOW entry operations:

  F77 symbol      C symbol
  ----------      ---------------------
  GDW_CL          GD_WINDOP_CLR
  GDW_EQ          GD_WINDOP_EQ
  GDW_GE          GD_WINDOP_GE
  GDW_GT          GD_WINDOP_GT
  GDW_LE          GD_WINDOP_LE
  GDW_LT          GD_WINDOP_LT
  GDW_NE          GD_WINDOP_NE
  GDW_ST          GD_WINDOP_SET
  GDW_UN          GD_WINDOP_UNK

Desync flags:

  F77 symbol      C symbol
  ----------      ---------------------
  GDDS_P          GD_DESYNC_PATHCHECK
  GDDS_O          GD_DESYNC_REOPEN

MPLEX lookback parameters:

  F77 symbol      C symbol
  ----------      ---------------------
  GDLB_A          GD_LOOKBACK_ALL
  GDLB_D          GD_DEFAULT_LOOKBACK

Miscellaneous parameters:

  F77 symbol      C symbol
  ----------      -------------------------
  GD_ALL          GD_ALL_FRAGMENTS
  GD_DSV          GD_DIRFILE_STANDARDS_VERSION
  GD_HER          GD_HERE
  GD_MLL          GD_MAX_LINE_LENGTH
