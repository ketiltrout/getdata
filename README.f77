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

Available Subroutines
=====================

* GDFOPN(dirfile_unit, dirfilename, dirfilename_len, flags)
  INTEGER dirfileunit, dirfilename_len, flags
  CHARACTER*<dirfilename_len> dirfilename

  This wraps dirfile_open(3), with the same input arguments (dirfilename_len
  should contain the string length of dirfilename).  It returns the dirfile
  unit number in dirfile_unit.  The flags should be a bitwise "or"d list of flag
  parameters (see below).  This behaves analogously to dirfile_open() itself:
  it returns a valid dirfile unit even in case of error.

* GDFCLS(dirfile_unit)
  INTEGER dirfileu_nit

  This wraps dirfile_close(3).  The argument is the dirfile unit to close.
  In addition to closing the dirfile itself, this will also disassociate the
  supplied dirfile unit number, which may be subsequently returned by a
  subsequent call to GDFOPN.

* GDFFLS(dirfile_unit, field_code, field_code_len)
  INTEGER dirfile_unit, field_code_len
  CHARACTER*<field_code_len> field_code

  This wraps dirfile_flush(3).  If field_code_len is zero, the entire dirfile
  will be flushed, and field_code will be ignored.  Otherwise the field named
  by field_code will be flushed.

* GDFGET(n_read, dirfile_unit, field_code, field_code_len, first_frame,
  first_sample, num_frames, num_samples, return_type, data_out)
  INTEGER n_read, dirfile_unit, field_code_len, first_frame, first_sample
  INTEGER num_frames, num_samples, return_type
  CHARACTER*<field_code_len> field_code
  <datatype>*<n> data_out

  This wraps getdata(3), with the same input arguments (field_code_len should
  contain the string length of the field_code).  The number of samples actually
  read is returned in n_read.  The return_type parameter should be one of the
  parameters defined in getdata.f.  data_out must be of sufficient length and
  of appropriate data type width for the data returned.

* GDFNFD(nfields, dirfile_unit)
  INTEGER nframes, dirfile_unit

  This wraps get_nfields(3).  It takes the dirfile unit number as input and
  returns the number of fields in the dirfile in nfields.

* GDFFDX(field_max, dirfile_unit)
  INTEGER field_max, dirfile_unit

  This subroutine, which has no direct analogue in the C API, returns the
  length of the longest field name defined in the dirfile.  It takes the
  dirfile unit number as input and returns the length (in characters) of
  the longest field name in the dirfile in field_max.
 
* GDFFDN(name, name_len, dirfile_unit, field_num)
  CHARACTER*<name_len> name
  INTEGER name_len, dirfile_unit, field_num

  This subroutine is the replacement for get_field_list(3).  In addition to
  the dirfile unit, it returns in name a Fortran 77 string containing the
  field name of the field indexed by field_num (which is should be a number
  between 1 and the output of GDFNFD).  If the name of the field is longer
  than name_len, it will return the actual length of the field in name_len
  and not modify the name argument.  If field_num is out of range, name_len
  will be set to zero, and name will not be modified.

* GDFNFR(nframes, dirfile_unit)
  INTEGER nframes, dirfile_unit

  This wraps get_nframes(3).  It takes the dirfile unit number as input and
  returns the number of frames in the dirfile in nframes.

* GDFSPF(spf, dirfile_unit, field_code, field_code_len)
  INTEGER spf, dirfile_unit, field_code_len
  CHARACTER*<field_code_len> field_code

  This wraps get_spf(3).  The field_code_len parameter should contain the
  string length of field_code.  The number of samples per frame in field_code
  will be returned in spf.

* GDFPUT(n_wrote, dirfile_unit, field_code, field_code_len, first_frame,
  first_sample, num_frames, num_samples, data_type, data_in)
  INTEGER n_wrote, dirfile_unit, field_code_len, first_frame, first_sample
  INTEGER num_frames, num_samples, data_type
  CHARACTER*<field_code_len> field_code
  <datatype>*<n> data_out

  This wraps putdata(3), with the same input arguments (field_code_len should
  contain the string length of the field_code).  The number of samples actually
  written is returned in n_wrote.  The data_type parameter should be one of the
  parameters defined in getdata.f.  data_in must be of sufficient length and
  of appropriate data type width for the data input.

* GDFERR(error, dirfile_unit)
  INTEGER error, dirfile_unit

  This subroutine takes a dirfile unit as input and returns the DIRFILE.error
  value associated with it in error.  The value of error will equal one of the
  error codes defined in getdata.f.

* GDFSTR(dirfile_unit, buffer, buffer_len)

  This subroutine takes a dirfile unit as input and will write the error
  string returned by get_error_string(3) in buffer, which is of length
  buffer_len.

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
  GD_EEM          GD_E_EMPTY
  GD_EAL          GD_E_ALLOC
  GD_ERA          GD_E_RANGE
  GD_EOL          GD_E_OPEN_LINFILE
  GD_ERL          GD_E_RECURSE_LEVEL
  GD_EBD          GD_E_BAD_DIRFILE
  GD_EBP          GD_E_BAD_PUT_FIELD
  GD_EAC          GD_E_ACCMODE

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
  GD_FE           GD_FORCE_ENDIAN
  GD_PE           GD_PEDANTIC

Data types (required by GDFGET and GDFPUT).  Note, because Fortran does not
support unsigned data types, only signed datatypes are defined here.  The
unsigned data type specifiers will still be accepted by these subroutines, and
can be specified by litteral value (which can be obtained by examining
getdata.h).  However, litteral values of constants defined in getdata.f and
getdata.h should not be expected to remain the same from one release to another,
so using litteral values should always be used done with caution.

  F77 symbol      C symbol          Notes
  ----------      ----------------- --------------------------------------
  GD_NUL          GD_NULL           Not suitable to be passed to GDFPUT
  GD_I8           GD_INT8
  GD_I16          GD_INT16
  GD_I32          GD_INT32
  GD_I64          GD_INT64
  GD_F32          GD_FLOAT32
  GD_F64          GD_FLOAT64
