FORTRAN 77 BINDINGS FOR GETDATA
===============================

This README describes the Fortran 77 bindings for the GetData library.  These
bindings consist of a Fortran compatibility library `libfgetdata' (writen in
C) and a Fortran 77 source file `getdata.f' which defines useful Fortran
parameters and declares the external subroutines.

These bindings are designed to comply to the Fortran 77 standards.  As a result,
identifiers are limited to six characters.   The compatibility library will
take care of converting Fortran CHARACTER stings to C strings.  However, as a
result, when strings are passed to the compatibility library as arguments, the
length of the string must also be passed.

Because Fortran 77 handles neither pointers nor abstract datatypes, DIRFILE
pointers are not used to refer to dirfile instances.  Instead, an integer
dirfile unit number is used.  Space is available in the compatibility library
for only 1023 dirfile units.  If an application attempts to open more than 1023
dirfiles simultaneously, the compatibility library will emit an error message
and raise SIGABRT.  Passing an invalid dirfile unit number to a subroutines
which requires one as input (other than GDFCLS, which will simply ignore it)
will result in the call failing with error code GD_EBD (= GD_E_BAD_DIRFILE, see
below).

Including getdata.f (which will be installed in the same directory as getdata.h)
will define several convenient parameters including the DIRFILE flags, the data
type specifiers, and error codes.  See below for a complete list.  If your
Fortran 77 compiler supports the MIL STD 1753 (DoD Extension) INCLUDE statement
(which any remotely modern compiler should), you can include this file in your
Fortran program to define these constants.

All integer type parameters passed to the compatibility library are of type
INTEGER (ie. the native size of the platform).  As a result, largefile support
will not be available in the Fortran 77 bindings on a 32-bit system.

All character string arguments require also an integer indicating the size of
the character buffer.  In cases where the bindings return string value, the
value will not be returned if the string length supplied is too short.  In
these cases, the character string will be left untouched, but the integer
indicating the string length will be updated to indicate the required string
length.  The exception to this is GDFSTR, which simply truncates the string
it outputs, as the C API does.

Available Subroutines
=====================

* GDFOPN(dirfile_unit, dirfilename, dirfilename_len, flags)
  
  Output:
    INTEGER dirfileunit
  Input:
    INTEGER dirfilename_len, flags
    CHARACTER*<dirfilename_len> dirfilename

  This wraps dirfile_open(3), with the same input arguments (dirfilename_len
  should contain the string length of dirfilename).  It returns the dirfile
  unit number in dirfile_unit.  The flags should be a bitwise "or"d list of flag
  parameters (see below).  This behaves analogously to dirfile_open() itself:
  it returns a valid dirfile unit even in case of error.

* GDFCLS(dirfile_unit)

  Input:
    INTEGER dirfileu_nit

  This wraps dirfile_close(3).  The argument is the dirfile unit to close.
  In addition to closing the dirfile itself, this will also disassociate the
  supplied dirfile unit number, which may be subsequently returned by a
  subsequent call to GDFOPN.

* GDFFLS(dirfile_unit, field_code, field_code_len)

  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This wraps dirfile_flush(3).  If field_code_len is zero, the entire dirfile
  will be flushed, and field_code will be ignored.  Otherwise the field named
  by field_code will be flushed.

* GDFGET(n_read, dirfile_unit, field_code, field_code_len, first_frame,
  first_sample, num_frames, num_samples, return_type, data_out)

  Output:
    INTEGER n_read
    <datatype>*<n> data_out
  Input:
    INTEGER dirfile_unit, field_code_len, first_frame, first_sample
    INTEGER num_frames, num_samples, return_type
    CHARACTER*<field_code_len> field_code

  This wraps getdata(3), with the same input arguments (field_code_len should
  contain the string length of the field_code).  The number of samples actually
  read is returned in n_read.  The return_type parameter should be one of the
  parameters defined in getdata.f (see below).  data_out must be of sufficient
  length and of appropriate data type width for the data returned.

* GDFGCO(n_read, dirfile_unit, field_code, field_code_len, return_type,
  data_out)

  Output:
    INTEGER n_read
    <datatype> data_out
  Input:
    INTEGER dirfile_unit, field_code_len, return_type
    CHARACTER*<field_code_len> field_code

  This wraps get_constant(3), with the same input arguments (field_code_len
  should contain the string length of the field_code).  If the call is
  successful, n_read will be non-zero.  The return_type parameter should be one
  of the parameters defined in getdata.f.  data_out must be of appropriate data
  type width for the data returned.

* GDFGST(n_read, dirfile_unit, field_code, field_code_len, len, data_out)

  Output:
    INTEGER n_read
    CHARACTER*<len> data_out
  Input:
    INTEGER dirfile_unit, field_code_len, len
    CHARACTER*<field_code_len> field_code

  This wraps get_string(3), with the same input arguments (field_code_len should
  contain the string length of the field_code).  The number of characters
  actually read is returned in n_read.  At most len characters will be returned.

* GDFNFD(nfields, dirfile_unit)

  Output:
    INTEGER nframes
  Input:
    INTEGER dirfile_unit

  This wraps get_nfields(3).  It takes the dirfile unit number as input and
  returns the number of fields in the dirfile in nfields.

* GDFNFT(nfields, dirfile_unit, type)

  Output:
    INTEGER nframes
  Input:
    INTEGER dirfile_unit, type

  This wraps get_nfields_by_type(3).  It takes the dirfile unit number, and type
  specifier as input and returns the number of fields of the specified type in
  the dirfile in nfields.

* GDFNVE(nvectors, dirfile_unit)

  Output:
    INTEGER nvectors
  Input:
    INTEGER dirfile_unit

  This wraps get_nvectors(3).  It takes the dirfile unit number as input and
  returns the number of vector fields in the dirfile in nfields.

* GDFNMF(nfields, dirfile, parent, parent_l)
* GDFNMT(nfields, dirfile, parent, parent_l, type)
* GDFNMV(nvectors, dirfile, parent, parent_l)

  These subroutine wrap get_nmetafields(3), get_nmetafields_by_type(3), and
  get_nmetavectors(3).  They behave analogously to GDFNFD, GDFNFT, and GDFNVE.

* GDFFDX(field_max, dirfile_unit)

  Output:
    INTEGER field_max
  Input:
    INTEGER dirfile_unit

  This subroutine, which has no direct analogue in the C API, returns the
  length of the longest field name defined in the dirfile.  It takes the
  dirfile unit number as input and returns the length (in characters) of
  the longest field name in the dirfile in field_max.
 
* GDFFDN(name, name_len, dirfile_unit, field_num)

  Output:
    CHARACTER*<name_len> name
  Input/Output:
    INTEGER name_len
  Input:
    INTEGER dirfile_unit, field_num

  This subroutine is the replacement for get_field_list(3).  It returns in
  name a Fortran 77 string containing the field name of the field indexed by
  field_num (which is should be a number between 1 and the output of GDFNFD).
  If the name of the field is longer than name_len, it will return the actual
  length of the field in name_len and not modify the name argument.  If
  field_num is out of range, name_len will be set to zero, and name will not be
  modified.

* GDFFNT(name, name_len, dirfile_unit, type, field_num)
  
  Output:
    CHARACTER*<name_len> name
  Input/Output:
    INTEGER name_len
  Input:
    INTEGER dirfile_unit, field_num, type

  This subroutine is the replacement for get_field_list_by_type(3) and behaves
  in the same manner as GDFFDN.  Type should be one of the parameters defined
  in getdata.f (see below).
    
* GDFVEN(name, name_len, dirfile_unit, field_num)

  Output:
    CHARACTER*<name_len> name
  Input/Output:
    INTEGER name_len
  Input:
    INTEGER dirfile_unit, field_num

  This subroutine is the replacement for get_vector_list(3) and behaves in the
  same manner as GDFFDN.

* GDFNFR(nframes, dirfile_unit)

  Output:
    INTEGER nframes
  Input:
    INTEGER dirfile_unit

  This wraps get_nframes(3).  It takes the dirfile unit number as input and
  returns the number of frames in the dirfile in nframes.

* GDFSPF(spf, dirfile_unit, field_code, field_code_len)

  Output:
    INTEGER spf
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This wraps get_spf(3).  The field_code_len parameter should contain the
  string length of field_code.  The number of samples per frame in field_code
  will be returned in spf.

* GDFPUT(n_wrote, dirfile_unit, field_code, field_code_len, first_frame,
  first_sample, num_frames, num_samples, data_type, data_in)

  Output:
    INTEGER n_wrote
  Input:
    INTEGER dirfile_unit, field_code_len, first_frame, first_sample
    INTEGER num_frames, num_samples, data_type
    CHARACTER*<field_code_len> field_code
    <datatype>*<n> data_out

  This wraps putdata(3), with the same input arguments (field_code_len should
  contain the string length of the field_code).  The number of samples actually
  written is returned in n_wrote.  The data_type parameter should be one of the
  parameters defined in getdata.f.  data_in must be of sufficient length and
  of appropriate data type width for the data input.

* GDFPCO(n_read, dirfile_unit, field_code, field_code_len, data_type,
  data_in)

  Output:
    INTEGER n_wrote
  Input:
    INTEGER dirfile_unit, field_code_len, data_type
    CHARACTER*<field_code_len> field_code
    <datatype> data_in

  This wraps put_constant(3), with the same input arguments (field_code_len
  should contain the string length of the field_code).  If the call is
  successful, n_wrote will be non-zero.  The data_type parameter should be one
  of the parameters defined in getdata.f.

* GDFPST(n_read, dirfile_unit, field_code, field_code_len, len, data_out)

  Output:
    INTEGER n_wrote
  Input:
    INTEGER dirfile_unit, field_code_len, len
    CHARACTER*<field_code_len> field_code
    CHARACTER*<len> data_in

  This wraps put_string(3), with the same input arguments (field_code_len should
  contain the string length of the field_code, and len should contain the string
  length of data_in).  The number of characters actually wrote is returned in
  n_wrote.

* GDFERR(error, dirfile_unit)

  Output:
    INTEGER error
  Input:
    INTEGER dirfile_unit

  This subroutine takes a dirfile unit as input and returns the DIRFILE.error
  value associated with it in error.  The value of error will equal one of the
  error codes defined in getdata.f.

* GDFSTR(dirfile_unit, buffer, buffer_len)

  Output:
    CHARACTER*<buffer_len> buffer
  Input:
    INTEGER dirfile_unit, buffer_len

  This subroutine takes a dirfile unit as input and will write the error
  string returned by get_error_string(3) in buffer, which is of length
  buffer_len.

* GDFFDT(entry_type, dirfile_unit, field_code, field_code_len)

  Output:
    INTEGER type
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine returns the field type of the specified field_code in
  entry_type.  The entry_type will be one of the entry type parameters listed
  below.

* GDFERW(spf, data_type, format_file, dirfile_unit, field_code, field_code_len)

  Output: 
    INTEGER spf, data_type, format_file
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine returns metadata describing a RAW field.  It returns the
  samples-per-frame, native data type, and the format file index in spf and
  data_type.  The data_type will be one of the data type parameters listed
  below.  If field_code is not found, or the field specified is not of RAW type,
  spf will be set to zero.  In this case the value of the other output
  parameters is unspecified.

* GDFELC(nfields, infield1, infield1_len, m1, b1, infield2, infield2_len, m2,
  b2, infield3, infield3_len, m3, b3, format_file, dirfile_unit, field_code,
  field_code_len)

  Output:
    INTEGER nfields, format_file
    CHARACTER*<infield1_len> infield1
    CHARACTER*<infield2_len> infield2
    CHARACTER*<infield3_len> infield3
    REAL*8 m1, b1, m2, b2, m3, b3
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

* GDFELT(infield, infield_len, table, table_len, format_file, dirfile_unit,
  field_code, field_code_len)

  Output: 
    CHARACTER*<infield_len> infield
    CHARACTER*<table_len> table
    INTEGER format_file
  Input/Output:
    INTEGER infield_len, table_len
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine returns metadata describing a LINTERP field.  If field_code
  is not found, or the field specified is not of LINTERP type, infield_len will
  be set to zero.  In this case the value of the remaining data is unspecified.

* GDFEBT(infield, infield_len, bitnum, numbits, format_file, dirfile_unit,
  field_code, field_code_len)

  Output: 
    CHARACTER*<infield_len> infield
    INTEGER bitnum, numbits, format_file
  Input/Output:
    INTEGER infield_len
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine returns metadata describing a BIT field.  If field_code
  is not found, or the field specified is not of BIT type, infield_len will
  be set to zero.  In this case the value of the remaining data is unspecified.

* GDFEMT(infield1, infield1_len, infield2, infield2_len, format_file,
  dirfile_unit, field_code, field_code_len)

  Output: 
    CHARACTER*<infield1_len> infield1
    CHARACTER*<infield2_len> infield2
    INTEGER format_file
  Input/Output:
    INTEGER infield1_len, infield2_len
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine returns metadata describing a MULTIPLY field.  If field_code
  is not found, or the field specified is not of MULTIPLY type, infield1_len
  will be set to zero.  In this case the value of the remaining data is
  unspecified.

* GDFEPH(infield, infield_len, shift, format_file, dirfile_unit, field_code,
  field_code_len)

  Output: 
    CHARACTER*<infield_len> infield
    INTEGER shift, format_file
  Input/Output:
    INTEGER infield_len
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine returns metadata describing a PHASE field.  If field_code
  is not found, or the field specified is not of PHASE type, shift will
  be set to zero.  In this case the value of the remaining data is unspecified.

* GDFFFI(format_file, dirfile_unit, field_code, field_code_len

  Output: 
    INTEGER format_file
  Input:
    INTEGER dirfile_unit, field_code_len
    CHARACTER*<field_code_len> field_code

  This subroutine returns the format file fragment index for the supplied field.
  If the field does not exist, or an error occurred, -1 is returned.

* GDFARW(dirfile_unit, field_code, field_code_len, data_type, spf, format_file)

  Input:
    INTEGER dirfile_unit, field_code_len, data_type, spf, format_file
    CHARACTER*<field_code_len> field_code

  This subroutine adds a RAW field with the supplied parameters to the
  specified format file fragment of the dirfile.

* GDFALC(dirfile_unit, field_code, field_code_len, nfields, in_field1,
  in_field1_len, m1, b1, in_field2, in_field2_len, m2, b2, in_field3,
  in_field3_len, m3, b3, format_file)

  Input:
    INTEGER dirfile_unit, field_code_len, nfields, in_field1_len, in_field2_len
    INTEGER in_field3_len, format_file
    REAL*8 m1, b1, m2, b2, m3, b3
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field1_len> in_field1
    CHARACTER*<in_field2_len> in_field2
    CHARACTER*<in_field3_len> in_field3

  This subroutine adds a LINCOM field with the supplied parameters to the
  specified format file fragment of the dirfile.  All three sets of input
  parameters are required to be passed to the call, but only the first
  nfield sets will be examined.

* GDFALT(dirfile_unit, field_code, field_code_len, in_field, in_field_len,
  table, table_len, format_file)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field_len, table_len, format_file
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field_len> in_field
    CHARACTER*<table_len> table

  This subroutine adds a LINTERP field with the supplied parameters to the
  specified format file fragment of the dirfile.

* GDFABT(dirfile_unit, field_code, field_code_len, in_field, in_field_len,
  bitnum, numbits, format_file)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field_len, bitnum, numbits
    INTEGER format_file
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field_len> in_field

  This subroutine adds a BIT field with the supplied parameters to the
  specified format file fragment of the dirfile.

* GDFAMT(dirfile_unit, field_code, field_code_len, in_field1, in_field1_len,
  in_field2, in_field2_len, format_file)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field1_len, in_field2_len
    INTEGER format_file
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field1_len> in_field1
    CHARACTER*<in_field2_len> in_field2

  This subroutine adds a MULTIPLY field with the supplied parameters to the
  specified format file fragment of the dirfile.


* GDFAPH(dirfile_unit, field_code, field_code_len, in_field1, in_field1_len,
  shift, format_file)

  Input:
    INTEGER dirfile_unit, field_code_len, in_field1_len, shift, format_file
    CHARACTER*<field_code_len> field_code
    CHARACTER*<in_field_len> in_field

  This subroutine adds a PHASE field with the supplied parameters to the
  specified format file fragment of the dirfile.

* GDFACO(dirfile_unit, field_code, field_code_len, const_type, data_type,
  value, format_file)

  Input:
    INTEGER dirfile_unit, field_code_len, const_type, data_type, format_file
    CHARACTER*<field_code_len> field_code
    <data_type> value

  This subroutine adds a CONST field with the supplied parameters to the
  specified format file fragment of the dirfile.  const_type is the data type
  of the field when stored in the dirfile.  data_type is the data type of the
  supplied value.  These need not be the same.

* GDFAST(dirfile_unit, field_code, field_code_len, value, value_len,
  format_file)

  Input:
    INTEGER dirfile_unit, field_code_len, value_len, format_file
    CHARACTER*<field_code_len> field_code
    CHARACTER*<value_len> value

  This subroutine adds a STRING field with the supplied parameters to the
  specified format file fragment of the dirfile

* GDFASP(dirfile_unit, format_file, spec, spec_len)

  Input:
    INTEGER dirfile_unit, format_file, spec_len
    CHARACTER*<spec_len> spec

  This subroutine wraps dirfile_add_spec(3), and allows adding a field to
  a dirfile given a field specification line.

* GDFMBT(dirfile_unit, parent, parent_len, field_code, field_code_len, in_field,
  in_field_len, bitnum, numbits)
* GDFACO(dirfile_unit, parent, parent_len, field_code, field_code_len,
  const_type, data_type, value)
* GDFMLC(dirfile_unit, parent, parent_len, field_code, field_code_len, nfields,
  in_field1, in_field1_len, m1, b1, in_field2, in_field2_len, m2, b2, in_field3,
  in_field3_len, m3, b3)
* GDFMLT(dirfile_unit, parent, parent_len, field_code, field_code_len, in_field,
  in_field_len, table, table_len)
* GDFMMT(dirfile_unit, parent, parent_len, field_code, field_code_len,
  in_field1, in_field1_len, in_field2, in_field2_len)
* GDFMPH(dirfile_unit, parent, parent_len, field_code, field_code_len,
  in_field1, in_field1_len, shift)
* GDFMCO(dirfile_unit, parent, parent_len, field_code, field_code_len,
  const_type, data_type, value)
* GDFMST(dirfile_unit, parent, parent_len, field_code, field_code_len, value,
  value_len)

  These functions are the corresponding META field functions for the GDFAxx
  functions above. They add META fields to the parent field indicated.

* GDFFFN(filename, filename_len, dirfile_unit, ind)

  Output:
    CHARACTER*<infield_len> infield
  Input/Output:
    INTEGER infield_len
  Input:
    INTEGER ind

  This subroutine returns the name of the format file frament indexed by ind.
  If the name of the file is longer than filename_len, it will return the
  actual length of the filename in filename_len and not modify the filename
  argument.

* GDFNFO(nformats, dirfile_unit)

  Output:
    INTEGER nformats
  Input:
    INTEGER dirfile_unit

  This subroutine returns the number of format file fragments in the specified
  dirfile.

* GDFFLM(dirfile_unit)

  Input:
    INTEGER dirfile_unit

  This subroutine wraps dirfile_flush_metadata(3), and will cause metadata
  changes to be written to disk.

* GDFINC(dirfile_unit, file, file_l, format_file, flags)

  This subroutine wraps dirfile_include(3), and allows the inclusion of another
  format file fragment into the current dirfile.

Defined Parameters
==================

The following parameters, listed here with their C library analogues, are
defined in getdata.f which may be included in any Fortran program using the
Fortran 77 bindings.

Error codes (returned by GDFERR):

  F77 symbol      C symbol          Notes
  ----------      ----------------- --------------------------------------
  GD_EOK          GD_E_OK           This is guaranteed to be equal to zero
  GD_EOP          GD_E_OPEN
  GD_EFO          GD_E_FORMAT
  GD_ETR          GD_E_TRUNC
  GD_ECR          GD_E_CREAT
  GD_EBC          GD_E_BAD_CODE
  GD_EBT          GD_E_BAD_TYPE
  GD_ERW          GD_E_RAW_IO
  GD_EOI          GD_E_OPEN_INCLUDE
  GD_EIE          GD_E_INTERNAL_ERROR
  GD_EAL          GD_E_ALLOC
  GD_ERA          GD_E_RANGE
  GD_EOL          GD_E_OPEN_LINFILE
  GD_ERL          GD_E_RECURSE_LEVEL
  GD_EBD          GD_E_BAD_DIRFILE
  GD_EBF          GD_E_BAD_FIELD_TYPE
  GD_EAC          GD_E_ACCMODE
  GD_EBE          GD_E_BAD_ENTRY
  GD_EDU          GD_E_DUPLICATE
  GD_EDM          GD_E_DIMENSION
  GD_EBI          GD_E_BAD_INDEX
  GD_EBS          GD_E_BAD_SCALAR

Dirfile flags (required by GDFOPN):

  F77 symbol      C symbol          Notes
  ----------      ----------------- --------------------------------------
  GD_RO           GD_RDONLY         The flags argument passed to GDFOPN
  GD_RW           GD_RDWR           must contain at least GD_RO or GD_RW
  GD_CR           GD_CREAT
  GD_EX           GD_EXCL
  GD_TR           GD_TRUNC
  GD_BE           GD_BIG_ENDIAN
  GD_LE           GD_LITTLE_ENDIAN
  GD_FC           GD_FORCE_ENCODING
  GD_FE           GD_FORCE_ENDIAN
  GD_PE           GD_PEDANTIC
  GD_EA           GD_AUTO_ENCODED
  GD_EN           GD_UNENCODED
  GD_ET           GD_TEXT_ENCODED
  GD_ES           GD_SLIM_ENCODED

Entry types (required by GDFFDT):

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
  GD_COE          GD_CONST_ENTRY
  GD_STE          GD_STRING_ENTRY

Data types.  Note, Fortran does not support unsigned data types, but GDFERW may
still return an unsigned type, so all types are defined here.  The unsigned data
type specifiers will be accepted by the other subroutines, but the data returned
may not be properly interpretable by Fortran 77.

  F77 symbol      C symbol          Notes
  ----------      ----------------- --------------------------------------
  GD_NUL          GD_NULL           Not suitable to be passed to GDFPUT
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
