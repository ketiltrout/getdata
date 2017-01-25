FORTRAN 95 BINDINGS FOR GETDATA
===============================

This README describes the Fortran 95 bindings for the GetData library.  These
bindings consist of a Fortran 95 compatibility library `libf95getdata' (written
in Fortran 95) and a Fortran 95 module file `getdata.mod' which defines the
interface.  The Fortran 95 bindings require the Fortran 77 compatibility
library `libfgetdata' for operation.

For programs which can use Fortran 95 modules, these bindings should be
preferred over the Fortran 77 bindings, since these bindings provide type
safety and more legible symbols.

As in the Fortran 77 bindings, dirfiles are referred to by "dirfile unit"
numbers, which are internally converted to the C API DIRFILE pointers.
Space is available in the compatibility library for only 1023 dirfile units.  If
an application attempts to open more than 1023 dirfiles simultaneously, the
compatibility library will emit an error message on standard error and return
and invalid dirfile unit number.  Passing an invalid dirfile unit number to a
procedure which requires one as input (other than fgd_close, which will simply
ignore it) will result in the call failing with error code GD_E_BAD_DIRFILE.

The "getdata" module, which these bindings define, is described in
`getdata.mod', which will be installed in the same directory as getdata.h.  The
getdata module defines the same error codes (GD_E_OK, GD_E_OPEN, &c.), the same
open flags (GD_RDONLY, GD_CREAT, &c.) and the same data type specifiers
(GD_INT8, GD_FLOAT32, &c.), and other symbolic parameters as the C API.

Available Procedures
====================

Notably, unlike the Fortran 77 bindings, the Fortran 95 bindings do not
require passing character string lengths along with the string itself to
procedures.  The downside to this convenience is that strings with trailing
whitespace cannot be used.

Procedures which are essentially equivalent to their C API counterparts, with
the exception of including an initial `f' in their names, and using dirfile
unit numbers in place of C's DIRFILE pointers are:

* integer function fgd_open(dirfilename, flags)
  character (len=*), intent(in) :: dirfilename
  integer, intent(in) :: flags

* integer function fgd_cbopen (dirfilename, flags, sehandler)
  character (len=*), intent (in) :: dirfilename
  integer, intent (in) :: flags
  interface
    subroutine sehandler(act, dirfile_unit, suberror, line)
      integer, intent (out) :: act
      integer, intent (in) :: dirfile_unit, suberror
      character (len=GD_MAX_LINE_LENGTH), intent (inout) :: line
    end subroutine
  end interface

* subroutine fgd_close (dirfile_unit)
  integer, intent(in) :: dirfile

* integer function fgd_desync (dirfile_unit, flags)
  integer, intent(in) :: dirfile, flags

* subroutine fgd_discard (dirfile_unit)
  integer, intent(in) :: dirfile

* subroutine fgd_verbose_prefix(dirfile, prefix)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: prefix

* subroutine fgd_mplex_lookback(dirfile, lookback)
  integer, intent(in) :: dirfile, lookback

* subroutine fgd_flush (dirfile_unit, field_code)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_code

* subroutine fgd_sync (dirfile, field_code)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_code

* subroutine fgd_raw_close (dirfile, field_code)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_code

  (For fgd_flush, fgd_sync and fgd_raw_close, if field_code is the empty string,
  the operation will be performed on the entire dirfile.)

* subroutine fgd_metaflush (dirfile_unit)
  integer, intent(in) :: dirfile

* character (len=GD_MAX_LINE_LENGTH) function fgd_fragmentname(dirfile_unit,
  ind)
  integer, intent(in) :: dirfile_unit, ind

* integer function fgd_nentries (dirfile, parent, entype, flags)
  integer, intent(in) :: dirfile, entype, flags
  character (len=*), intent(in) :: parent

* integer function fgd_nfields (dirfile_unit)
  integer, intent(in) :: dirfile_unit

* integer function fgd_nfields_by_type (dirfile_unit, type)
  integer, intent(in) :: dirfile_unit, type

* integer function fgd_nvectors (dirfile_unit)
  integer, intent(in) :: dirfile_unit

* integer function fgd_nmetafields (dirfile_unit, parent)
  integer, intent(in) :: dirfile_unit
  character, intent(in) :: parent

* integer function fgd_nmetafields_by_type (dirfile_unit, parent, type)
  integer, intent(in) :: dirfile_unit, type
  character, intent(in) :: parent

* integer function fgd_nmetavectors (dirfile_unit, parent)
  integer, intent(in) :: dirfile_unit
  character, intent(in) :: parent

* integer function fgd_nformats (dirfile_unit)
  integer, intent(in) :: dirfile_unit

* integer function fgd_nframes (dirfile_unit)
  integer, intent(in) :: dirfile_unit

* integer function fgd_spf (dirfile_unit, field_code)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_code

* integer function fgd_error (dirfile_unit)
  integer, intent(in) :: dirfile_unit
 
* integer function fgd_error_count (dirfile_unit)
  integer, intent(in) :: dirfile_unit
 
* integer function fgd_error_string (dirfile, buffer, len)
  integer, intent(in) :: dirfile, len
  character (len=<len>), intent(out) :: buffer

* integer function fgd_get_string (dirfile, field_code, length, data_out)
  integer, intent(in) :: dirfile, length
  character (len=*), intent(in) :: field_code
  character (len=*), intent(out) :: data_out

* subroutine fgd_put_string (dirfile, field_code, data_in)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_code, data_in

* subroutine fgd_add_spec (dirfile, spec, fragment_index)
  integer, intent(in) :: dirfile, fragment_index
  character (len=*), intent(in) :: spec

* subroutine fgd_madd_spec (dirfile, spec, parent)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: parent, spec

* subroutine fgd_include (dirfile, inc_file, flags)
  integer, intent(in) :: dirfile, fragment_index, flags
  character (len=*), intent(in) :: inc_file

* subroutine fgd_include_affix (dirfile, fragmentname, fragment_index, prefix,
  suffix, flags)
  integer, intent(in) :: dirfile, fragment_index, flags
  character (len=*), intent(in) :: fragmentname, prefix, suffix

* integer function fgd_array_len (dirfile_unit, field_code)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_code

* subroutine fgd_add_alias (dirfile, field_code, targ, fragment_index)
  integer, intent(in) :: dirfile, fragment_index
  character(len=*), intent(in) :: field_code, targ

* subroutine fgd_add_bit (dirfile, field_name, in_field, bitnum, numbits,
  fragment_index)
  integer, intent(in) :: dirfile, bitnum, numbits, fragment_index
  character (len=*), intent(in) :: field_name, in_field

* subroutine fgd_add_clincom (dirfile, field_name, n_fields, in_field1, m1,
  b1, in_field2, m2, b2, in_field3, m3, b3, fragment_index)
  integer, intent(in) :: dirfile, n_fields, fragment_index
  character (len=*), intent(in) :: field_name, in_field1, in_field2, in_field3
  double complex, intent(in) :: m1, b1, m2, b2, m3, b3

* subroutine fgd_add_carray (dirfile, field_name, const_type, array_len,
  fragment_index)
  integer, intent(in) :: dirfile, const_type, array_len, fragment_index
  character (len=*), intent(in) :: field_name

  (Unlike the C counterpart, this version cannot be used to simultaneously set
  the value of the field.  Use one of the fgd_put_carray procedures after
  creating the field).

* subroutine fgd_add_const (dirfile, field_name, const_type,
  fragment_index)
  integer, intent(in) :: dirfile, const_type, fragment_index
  character (len=*), intent(in) :: field_name

  (Unlike the C counterpart, this version cannot be used to simultaneously set
  the value of the constant.  Use one of the fgd_put_constant procedures after
  creating the field).

* subroutine fgd_add_cpolynom (dirfile, field_name, poly_ord, in_field, a0,
  a1, a2, a3, a4, a5, fragment_index)
  integer, intent(in) :: dirfile, poly_ord, fragment_index
  character (len=*), intent(in) :: field_name, in_field
  double complex, intent(in) :: a0, a1, a2, a3, a4, a5

* subroutine fgd_add_crecip (dirfile, field_name, in_field, cdividend,
  fragment_index)
  integer, intent(in) :: dirfile, fragment_index
  character (len=*), intent(in) :: field_name, in_field
  double complex, intent(in) :: cdividend

* subroutine fgd_add_divide (dirfile, field_name, in_field1, in_field2,
  fragment_index)
  integer, intent(in) :: dirfile, fragment_index
  character (len=*), intent(in) :: field_name, in_field1, in_field2

* subroutine fgd_add_lincom (dirfile, field_name, n_fields, in_field1, m1,
  b1, in_field2, m2, b2, in_field3, m3, b3, fragment_index)
  integer, intent(in) :: dirfile, n_fields, fragment_index
  character (len=*), intent(in) :: field_name, in_field1, in_field2, in_field3
  double precision, intent(in) :: m1, b1, m2, b2, m3, b3

* subroutine fgd_add_linterp (dirfile, field_name, in_field, table,
  fragment_index)
  integer, intent(in) :: dirfile, fragment_index
  character (len=*), intent(in) :: field_name, in_field, table

* subroutine fgd_add_mplex (dirfile, field_code, in_field, count_field,
  count_val, period, fragment_index)
  character(len=*), intent(in) :: field_code, in_field, count_field
  integer, intent(in) :: dirfile, count_val, period, fragment_index

* subroutine fgd_add_multiply (dirfile, field_name, in_field1, in_field2,
  fragment_index)
  integer, intent(in) :: dirfile, fragment_index
  character (len=*), intent(in) :: field_name, in_field1, in_field2

* subroutine fgd_add_phase (dirfile, field_name, in_field, phase,
  fragment_index)
  integer, intent(in) :: dirfile, phase, fragment_index
  character (len=*), intent(in) :: field_name, in_field

* subroutine fgd_add_polynom (dirfile, field_name, poly_ord, in_field, a0,
  a1, a2, a3, a4, a5, fragment_index)
  integer, intent(in) :: dirfile, poly_ord, fragment_index
  character (len=*), intent(in) :: field_name, in_field
  double precision, intent(in) :: a0, a1, a2, a3, a4, a5

* subroutine fgd_add_raw (dirfile, field_code, data_type, spf,
  fragment_index)
  integer, intent(in) :: dirfile, data_type, spf, fragment_index
  character (len=*), intent(in) :: field_code

* subroutine fgd_add_recip (dirfile, field_name, in_field, dividend,
  fragment_index)
  integer, intent(in) :: dirfile, fragment_index
  character (len=*), intent(in) :: field_name, in_field
  double precision, intent(in) :: dividend

* subroutine fgd_add_sbit (dirfile, field_name, in_field, bitnum, numbits,
  fragment_index)
  integer, intent(in) :: dirfile, bitnum, numbits, fragment_index
  character (len=*), intent(in) :: field_name, in_field

* subroutine fgd_add_string (dirfile, field_code, fragment_index)
  integer, intent(in) :: dirfile, fragment_index
  character (len=*), intent(in) :: field_code

  (Unlike the C counterpart, this version cannot be used to simultaneously set
  the value of the string.  Use the gd_put_string procedures after creating the
  field).

* subroutine fgd_add_window_i (dirfile, field_code, in_field, check_field,
  windop, threshold, fragment_index)
  character(len=*), intent(in) :: field_code, in_field, check_field
  integer, intent(in) :: dirfile, windop, threshold, fragment_index

* subroutine fgd_add_window_r (dirfile, field_code, in_field, check_field, &
  windop, threshold, fragment_index)
  character(len=*), intent(in) :: field_code, in_field, check_field
  integer, intent(in) :: dirfile, windop, fragment_index
  double precision, intent(in) :: threshold

* subroutine fgd_madd_alias (dirfile, parent, field_code, targ)
  integer, intent(in) :: dirfile
  character(len=*), intent(in) :: parent, field_code, targ

* subroutine fgd_madd_bit (dirfile, parent, field_name, in_field, bitnum,
  numbits)
  integer, intent(in) :: dirfile, bitnum, numbits
  character (len=*), intent(in) :: field_name, in_field, parent

* subroutine fgd_madd_clincom (dirfile, parent, field_name, n_fields,
  in_field1, m1, b1, in_field2, m2, b2, in_field3, m3, b3)
  integer, intent(in) :: dirfile, n_fields
  character (len=*), intent(in) :: field_name, in_field1, in_field2, in_field3
  character (len=*), intent(in) :: parent
  double complex, intent(in) :: m1, b1, m2, b2, m3, b3

* subroutine fgd_madd_carray (dirfile, parent, field_name, const_type,
  array_len)
  integer, intent(in) :: dirfile, const_type, array_len
  character (len=*), intent(in) :: field_name, parent

  (Unlike the C counterpart, this version cannot be used to simultaneously set
  the value of the field.  Use one of the fgd_put_carray procedures after
  creating the field).

* subroutine fgd_madd_const (dirfile, parent, field_name, const_type)
  integer, intent(in) :: dirfile, const_type
  character (len=*), intent(in) :: field_name, parent

  (Unlike the C counterpart, this version cannot be used to simultaneously set
  the value of the constant.  Use one of the gd_put_constant procedures after
  creating the field).

* subroutine fgd_madd_cpolynom (dirfile, parent, field_name, poly_ord,
  in_field, a0, a1, a2, a3, a4, a5)
  integer, intent(in) :: dirfile, poly_ord
  character (len=*), intent(in) :: field_name, in_field, parent
  double precision, intent(in) :: a0, a1, a2, a3, a4, a5

* subroutine fgd_madd_crecip (dirfile, parent, field_name, in_field, cdividend)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_name, in_field, parent
  double complex, intent(in) :: cdividend

* subroutine fgd_madd_divide (dirfile, parent, field_name, in_field1,
  in_field2)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_name, in_field1, in_field2, parent

* subroutine fgd_madd_lincom (dirfile, parent, field_name, n_fields,
  in_field1, m1, b1, in_field2, m2, b2, in_field3, m3, b3)
  integer, intent(in) :: dirfile, n_fields
  character (len=*), intent(in) :: field_name, in_field1, in_field2, in_field3
  character (len=*), intent(in) :: parent
  double precision, intent(in) :: m1, b1, m2, b2, m3, b3

* subroutine fgd_madd_divide (dirfile, parent, field_name, in_field1,
  in_field2)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_name, in_field1, in_field2, parent

* subroutine fgd_madd_linterp (dirfile, parent, field_name, in_field,
  table)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_name, in_field, table, parent

* subroutine fgd_madd_mplex (dirfile, parent, field_code, in_field, count_field,
  count_val, period)
  character(len=*), intent(in) :: parent, field_code, in_field, count_field
  integer, intent(in) :: dirfile, count_val, period

* subroutine fgd_madd_phase (dirfile, parent, field_name, in_field, phase)
  integer, intent(in) :: dirfile, phase
  character (len=*), intent(in) :: field_name, in_field, parent

* subroutine fgd_madd_polynom (dirfile, parent, field_name, poly_ord,
  in_field, a0, a1, a2, a3, a4, a5)
  integer, intent(in) :: dirfile, poly_ord
  character (len=*), intent(in) :: field_name, in_field, parent
  double precision, intent(in) :: a0, a1, a2, a3, a4, a5

* subroutine fgd_madd_recip (dirfile, parent, field_name, in_field, dividend)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_name, in_field, parent
  double precision, intent(in) :: dividend

* subroutine fgd_madd_sbit (dirfile, parent, field_name, in_field, bitnum,
  numbits)
  integer, intent(in) :: dirfile, bitnum, numbits
  character (len=*), intent(in) :: field_name, in_field, parent

* subroutine fgd_madd_string (dirfile, parent, field_code)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_code, parent

  (Unlike the C counterpart, this version cannot be used to simultaneously
  set the value of the string.  Use the gd_put_string procedures after creating
  the field).

* subroutine fgd_madd_window_i (dirfile, parent, field_code, in_field,
  check_field, windop, threshold)
  character(len=*), intent(in) :: parent, field_code, in_field, check_field
  integer, intent(in) :: dirfile, windop, threshold

* subroutine fgd_madd_window_r (dirfile, parent, field_code, in_field,
  check_field, windop, threshold)
  character(len=*), intent(in) :: parent, field_code, in_field, check_field
  integer, intent(in) :: dirfile, windop
  double precision, intent(in) :: threshold

* character (len=GD_FIELD_LEN) function fgd_reference (dirfile)
  integer, intent(in) :: dirfile

* character (len=GD_FIELD_LEN)  function fgd_reference (dirfile, field_code)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_code

* subroutine fgd_parser_callback (dirfile, sehandler)
  integer, intent(in) :: dirfile
  interface
    subroutine sehandler(act, dirfile_unit, suberror, line)
      integer, intent (out) :: act
      integer, intent (in) :: dirfile_unit, suberror
      character (len=GD_MAX_LINE_LENGTH), intent (inout) :: line
    end subroutine
  end interface

* integer function fgd_encoding (dirfile, fragment)
  integer, intent(in) :: dirfile, fragment

* subroutine fgd_alter_encoding (dirfile, encoding, fragment, recode)
  integer, intent(in) :: dirfile, encoding, fragment, recode

* integer function fgd_endianness (dirfile, fragment)
  integer, intent(in) :: dirfile, fragment

* subroutine fgd_alter_endianness (dirfile, endianness, fragment, recode)
  integer, intent(in) :: dirfile, endianness, fragment, recode

* integer function fgd_frameoffset (dirfile, fragment)
  integer, intent(in) :: dirfile, fragment

* subroutine fgd_alter_frameoffset (dirfile, frameoffset, fragment, recode)
  integer, intent(in) :: dirfile, frameoffset, fragment, recode

* integer function fgd_protection (dirfile, fragment)
  integer, intent(in) :: dirfile, fragment

* subroutine fgd_alter_protection (dirfile, protection_level, fragment)
  integer, intent(in) :: dirfile, protection_level, fragment

* subroutine fgd_fragment_affixes (prefix, suffix, dirfile, fragment_index)
  character(len=*), intent(out) :: prefix, suffix
  integer, intent(in) :: dirfile, fragment_index

* subroutine fgd_alter_affixes (dirfile, fragment_index, prefix, suffix)
  integer, intent(in) :: dirfile, fragment_index
  character(len=*), intent(in) :: prefix, suffix

* integer function fgd_native_type (dirfile, field_code)
  integer integer(in) :: dirfile
  character (len=*), intent(in) :: field_code

* integer function fgd_parent_fragment (dirfile, fragment)
  integer, intent(in) :: dirfile, fragment

* subroutine fgd_uninclude (dirfile, fragment, del)
  integer, intent(in) :: dirfile, fragment, del

* character (len=GD_MAX_LINE_LENGTH) function fgd_raw_filename (dirfile,
  field_code)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_code

* subroutine fgd_move (dirfile, field_code, new_fragment, flags)
  integer, intent(in) :: dirfile, new_fragment, flags
  character (len=*), intent(in) :: field_code

* subroutine fgd_alter_bit (dirfile, field_name, in_field, bitnum, numbits)
  integer, intent(in) :: dirfile, bitnum, numbits
  character (len=*), intent(in) :: field_name, in_field

* subroutine fgd_alter_clincom (dirfile, field_name, n_fields, in_field1,
  cm1, cb1, in_field2, cm2, cb2, in_field3, cm3, cb3)
  integer, intent(in) :: dirfile, n_fields
  character (len=*), intent(in) :: field_name, in_field1, in_field2, in_field3
  double complex, intent(in) :: cm1, cb1, cm2, cb2, cm3, cb3

* subroutine fgd_alter_carray (dirfile, field_name, const_type, array_len)
  integer, intent(in) :: dirfile, const_type, array_len
  character (len=*), intent(in) :: field_name

* subroutine fgd_alter_const (dirfile, field_name, const_type)
  integer, intent(in) :: dirfile, const_type
  character (len=*), intent(in) :: field_name

* subroutine fgd_alter_cpolynom (dirfile, field_name, poly_ord, in_field, ca0,
  ca1, ca2, ca3, ca4, ca5)
  integer, intent(in) :: dirfile, poly_ord
  character (len=*), intent(in) :: field_name, in_field1, in_field2, in_field3
  double complex, intent(in) :: ca0, ca1, ca2, ca3, ca4, ca5

* subroutine fgd_alter_crecip (dirfile, field_name, in_field, dividend)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_name, in_field
  double complex, intent(in) :: cdividend

* subroutine fgd_alter_divide (dirfile, field_name, in_field1, in_field2)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_name, in_field1, in_field2

* subroutine fgd_alter_lincom (dirfile, field_name, n_fields, in_field1,
  m1, b1, in_field2, m2, b2, in_field3, m3, b3)
  integer, intent(in) :: dirfile, n_fields
  character (len=*), intent(in) :: field_name, in_field1, in_field2, in_field3
  double precision, intent(in) :: m1, b1, m2, b2, m3, b3

* subroutine fgd_alter_linterp (dirfile, field_name, in_field, table, move)
  integer, intent(in) :: dirfile, move
  character (len=*), intent(in) :: field_name, in_field, table

* subroutine fgd_alter_mplex (dirfile, field_name, in_field, count_field,
  count_val, period)
  integer, intent(in) :: dirfile, count_val, period
  character (len=*), intent(in) :: field_name, in_field, count_field

* subroutine fgd_alter_multiply (dirfile, field_name, in_field1, in_field2)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_name, in_field1, in_field2

* subroutine fgd_alter_phase (dirfile, field_name, in_field, phase)
  integer, intent(in) :: dirfile, phase
  character (len=*), intent(in) :: field_name, in_field

* subroutine fgd_alter_polynom (dirfile, field_name, poly_ord, in_field, a0,
  a1, a2, a3, a4, a5)
  integer, intent(in) :: dirfile, poly_ord
  character (len=*), intent(in) :: field_name, in_field1, in_field2, in_field3
  double precision, intent(in) :: a0, a1, a2, a3, a4, a5

* subroutine fgd_alter_raw (dirfile, field_code, data_type, spf, move)
  integer, intent(in) :: dirfile, data_type, spf, move
  character (len=*), intent(in) :: field_code

* subroutine fgd_alter_recip (dirfile, field_name, in_field, dividend)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_name, in_field
  double precision, intent(in) :: dividend

* subroutine fgd_alter_sbit (dirfile, field_name, in_field, bitnum, numbits)
  integer, intent(in) :: dirfile, bitnum, numbits
  character (len=*), intent(in) :: field_name, in_field

* subroutine fgd_alter_window_i (dirfile, field_name, in_field, check_field,
  windop, threshold)
  integer, intent(in) :: dirfile, windop, threshold
  character (len=*), intent(in) :: field_name, in_field, check_field

* subroutine fgd_alter_window_r (dirfile, field_name, in_field, check_field,
  windop, threshold)
  integer, intent(in) :: dirfile, windop
  character (len=*), intent(in) :: field_name, in_field, check_field
  double precision, intent(in) :: threshold

* subroutine fgd_alter_spec (dirfile, spec, recode)
  integer, intent(in) :: dirfile, recode
  character (len=*), intent(in) :: spec

* subroutine fgd_malter_spec (dirfile, spec, parent, recode)
  integer, intent(in) :: dirfile, recode
  character (len=*), intent(in) :: parent, spec

* subroutine fgd_rename (dirfile, field_code, new_name, flags)
  integer, intent(in) :: dirfile, flags
  character (len=*), intent(in) :: field_code, new_name

* subroutine fgd_delete (dirfile, field_code, flags)
  integer, intent(in) :: dirfile, flags
  character (len=*), intent(in) :: field_code

* integer function fgd_entry_type (dirfile, field_code)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_code

* integer function fgd_fragment_index (dirfile, field_code)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_code

* integer function fgd_validate (dirfile, field_code)
  integer integer(in) :: dirfile
  character (len=*), intent(in) :: field_code

* double precision function fgd_framenum (dirfile, field_code, value)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_code
  double precision, intent(in) :: value

* double precision function fgd_framenum_subset (dirfile, field_code, value,
  frame_start, frame_end)
  integer, intent(in) :: dirfile, frame_start, frame_end
  character (len=*), intent(in) :: field_code
  double precision, intent(in) :: value 

* subroutine fgd_dirfilename (dirfilename, dirfilename_l, dirfile,
  fragment_index)
  character (len=*), intent(out) :: dirfilename
  integer, intent(in) :: dirfile, fragment_index
  integer, intent(inout) :: dirfilename_l

* integer function fgd_bof (dirfile, field_code)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_code

* integer function fgd_eof (dirfile, field_code)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_code

* integer function fgd_dirfile_standards (dirfile, version)
  integer, intent(in) :: dirfile, version

* integer function fgd_invalid_dirfile ()

* integer function fgd_seek (dirfile, field_code, frame_num, sample_num, flags)
  integer, intent(in) :: dirfile, frame_num, sample_num, flags
  character (len=*), intent(in): field_code

* subroutine fgd_strtok (token, token_len, dirfile, string)
  character (len=*), intent(out) :: token
  integer, intent(inout) :: token_len
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: string

  (If passed the empty string, the next token of the previous string is
  returned.)

* integer function fgd_tell (dirfile, field_code)
  integer, intent(in) :: dirfile
  character (len=*), intent(in): field_code

* character (len=GD_MAX_LINE_LENGTH) function fgd_alias_target (dirfile,
  field_code)
  integer, intent(in) :: dirfile
  character(len=*), intent(in) :: field_code

* integer function fgd_naliases (dirfile, field_code)
  integer, intent(in) :: dirfile
  character(len=*), intent(in) :: field_code

* integer function fgd_hidden (dirfile, field_code)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_code

* subroutine fgd_hide (dirfile, field_code)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_code

* subroutine fgd_unhide (dirfile, field_code)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_code

* integer function fgd_encoding_support (encoding)
  integer, intent(in) :: encoding


In order to respect type safety, the gd_getdata and gd_putdata analogues encode
the data type of their array in their function name, rather than as a parameter.
Otherwise, they behave the same as their C counterparts.

* integer function fgd_getdata_n (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples)
  integer, intent(in) :: dirfile_unit
  integer, intent(in) :: first_frame, first_sample, num_frames, num_samples
  character (len=*), intent(in) :: field_code

  This calls getdata(3) with return_type = GD_NULL (i.e. return no data).  As a
  result, no data_out parameter is required.

* integer function fgd_getdata_i1 (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples, data_out)
* integer function fgd_getdata_i2 (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples, data_out)
* integer function fgd_getdata_i4 (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples, data_out)
* integer function fgd_getdata_i8 (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples, data_out)
* integer function fgd_getdata_r4 (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples, data_out)
* integer function fgd_getdata_r8 (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples, data_out)
* integer function fgd_getdata_c8 (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples, data_out)
* integer function fgd_getdata_c16 (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples, data_out)
  integer, intent(in) :: dirfile_unit
  integer, intent(in) :: first_frame, first_sample, num_frames, num_samples
  character (len=*), intent(in) :: field_code
  <datatype>, dimension(:), intent(out) :: data_out

  These call getdata(3) with return_type = GD_INT8, GD_INT16, GD_INT32,
  GD_INT64, GD_FLOAT32, GD_FLOAT64, GD_COMPLEX64, and GD_COMPLEX128
  respectively.  Here <datatype> is an integer or real type of the appropriate
  width.

  Analogously, for putdata:

* integer function fgd_putdata_i1 (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples, data_in)
* integer function fgd_putdata_i2 (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples, data_in)
* integer function fgd_putdata_i4 (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples, data_in)
* integer function fgd_putdata_i8 (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples, data_in)
* integer function fgd_putdata_r4 (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples, data_in)
* integer function fgd_putdata_r8 (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples, data_in)
* integer function fgd_putdata_c8 (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples, data_out)
* integer function fgd_putdata_c16 (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples, data_out)
  integer, intent(in) :: dirfile_unit
  integer, intent(in) :: first_frame, first_sample, num_frames, num_samples
  character (len=*), intent(in) :: field_code
  <datatype>, dimension(:), intent(in) :: data_in

  No corresponding fgd_putdata_n function exists, since GD_NULL is not an
  acceptable input data_type for gd_putdata(3).

  Analogously for gd_constants, gd_get_carray_slice, gd_get_constant,
  gd_mconstants, gd_put_carray_slice, and gd_put_constant, for which only the
  _i1 versions are shown here:

* subroutine fgd_constants_i1(values, dirfile)
  integer(1), dimension(:), intent(out) :: values
* subroutine fgd_get_carray_i1 (dirfile, field_code, data_out, start, array_len)
  integer, intent(in) :: dirfile_unit, start, array_len
  character (len=*), intent(in) :: field_code
  integer(1), intent(out) :: data_out
* subroutine fgd_get_constant_i1 (dirfile, field_code, data_out)
  integer, intent(in) :: dirfile_unit
  character (len=*), intent(in) :: field_code
  integer(1), intent(out) :: data_out
* subroutine fgd_mconstants_i1(values, dirfile, parent)
  integer(1), dimension(:), intent(out) :: values
  character (len=*), intent(in) :: parent
* subroutine fgd_put_carray_i1 (dirfile, field_code, data_in, start, array_len)
  integer, intent(in) :: dirfile_unit, start, array_len
  character (len=*), intent(in) :: field_code
  integer(1), intent(in) :: data_in
* subroutine fgd_put_constant_i1 (dirfile, field_code, data_in)
  integer, intent(in) :: dirfile_unit
  character (len=*), intent(in) :: field_code
  integer(1), intent(in) :: data_in

  For the carray functions, if len is zero, gd_get_carray(3) or gd_put_carray(3)
  will be called to read or write the whole array.  Otherwise these subroutines
  will call gd_get_carray_slice(3) or gd_put_carray_slice(3).

Other procedures in the Fortran 95 bindings are:

* integer function fgd_match_entries_max (dirfile, regex, fragment, entype, flags)
  integer, intent(in) :: dirfile, fragment, entype, flags
  character (len=*), intent(in) :: regex

  This function returns the length of the longest entry name defined in the
  dirfile satisfying the given criteria.  The parameters are the same as they
  are for fgd_match_entries.

* integer function fgd_entry_name_max (dirfile, parent, entype, flags)
  integer, intent(in) :: dirfile, entype, flags
  character (len=*), intent(in) :: parent

  This function returns the length of the longest entry name defined in the
  dirfile satisfying the given criteria.  The parameters are the same as they
  are for fgd_nentries.

* integer fgd_field_name_max (dirfile_unit)
  integer, intent(in) :: dirfile_unit

  This function returns the length of the longest field name defined in the
  dirfile.

* integer fgd_mfield_name_max (dirfile_unit, parent)
  integer, intent(in) :: dirfile_unit
  character (len=*), intent(in) :: parent

  This function returns the length of the longest field name defined in the
  dirfile for META fields of the supplied parent field.

* integer fgd_string_value_max (dirfile_unit)
  integer, intent(in) :: dirfile_unit

  This function returns the length of the longest STRING field (excluding /META
  fields) in the database.

* integer fgd_mstring_value_max (dirfile_unit, parent)
  integer, intent(in) :: dirfile_unit
  character (len=*), intent(in) :: parent

  This function returns the length of the longest STRING metafield for the given
  parent field.

* integer fgd_alias_max (dirfile, field_code)
  integer, intent(in) :: dirfile
  character(len=*), intent(in) :: field_code

  This function returns the length of the longest alias for the given
  field_code.

* integer function fgd_match_entries (entry_list, dirfile, regex, fragment,
  entype, flags, entry_len)
  character(len=*), dimension(:), intent(out) :: entry_list
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: regex
  integer, intent(inout) :: entry_len

  This function behave analogously to gd_match_entries(3), except that it
  requires an additional argument, entry_len, which is the longest entry name
  which will fit in the supplied entry_list array.  If the longest entry name in
  the dirfile is longer than entry_len, entry_len will be set to this value
  (which is equivalent to the return value of fgd_match_entries_max) and entry_list
  will remain untouched.

* subroutine fgd_entry_list (entry_list, dirfile, parent, entype, flags,
  entry_len)
  character(len=*), dimension(:), intent(out) :: entry_list
  integer, intent(in) :: dirfile, entype, flags
  integer, intent(inout) :: entry_len
  character (len=*), intent(in) :: parent

  This subroutine behaves analogously to gd_entry_list(3), except that it
  requires an additional argument, entry_len, which is the longest entry name
  which will fit in the supplied entry_list array.  If the longest entry name in
  the dirfile is longer than entry_len, entry_len will be set to this value
  (which is equivalent to the return value of fgd_entry_name_max) and entry_list
  will remain untouched.

  Analogously:

* subroutine fgd_field_list (field_list, dirfile, field_len)
  character (len=<field_len>) dimension(:), intent(out) :: field_list
  integer, intent(in) :: dirfile_unit
  integer, intent(inout) :: field_len
* subroutine fgd_field_list_by_type (field_list, dirfile, entype, field_len)
  character (len=<field_len>) dimension(:), intent(out) :: field_list
  integer, intent(in) :: dirfile_unit, entype
  integer, intent(inout) :: field_len
* subroutine fgd_vector_list (field_list, dirfile, field_len)
  character (len=<field_len>) dimension(:), intent(out) :: field_list
  integer, intent(in) :: dirfile_unit
  integer, intent(inout) :: field_len
* subroutine fgd_strings (strings, dirfile, string_len)
  character (len=<string_len>) dimension(:), intent(out) :: string_list
  integer, intent(in) :: dirfile_unit
  integer, intent(inout) :: string_len
* subroutine fgd_aliases (aliases, dirfile, field_code, alias_len)
  character(len=<alias_len> dimension(:), intent(out) :: aliases
  integer, intent(in) :: dirfile
  character(len=*), intent(in) :: field_code
  integer, intent(inout) :: alias_len

  Also analogously, except that field_len will be equivalent to the return
  value of fgd_mfield_name_max or fgd_string_value_max, if too small:

* subroutine fgd_mfield_list (field_list, dirfile, parent, field_len)
  character (len=<field_len>) dimension(:), intent(out) :: field_list
  integer, intent(in) :: dirfile_unit
  integer, intent(inout) :: field_len
  character (len=*), intent(in) :: parent
* subroutine fgd_mfield_list_by_type (field_list, dirfile, entype,
  parent, field_len)
  character (len=<field_len>) dimension(:), intent(out) :: field_list
  integer, intent(in) :: dirfile_unit, entype
  integer, intent(inout) :: field_len
  character (len=*), intent(in) :: parent
* subroutine fgd_mvector_list (field_list, dirfile, parent, field_len)
  character (len=<field_len>) dimension(:), intent(out) :: field_list
  integer, intent(in) :: dirfile_unit
  integer, intent(inout) :: field_len
  character (len=*), intent(in) :: parent
* subroutine fgd_mstrings (string_list, dirfile, parent, string_len)
  character (len=<string_len>) dimension(:), intent(out) :: string_list
  integer, intent(in) :: dirfile_unit
  integer, intent(inout) :: string_len
  character (len=*), intent(in) :: parent

* integer function fgd_entry (dirfile_unit, field_code, ent)
  integer, intent(in) :: dirfile_unit
  character (len=*), intent(in) :: field_code
  type(gd_entry), intent(out) :: ent

  This fills ent with the metadata for field_code obtained by calling
  gd_entry(3).  It returns the field type, or GD_NO_ENTRY if an error
  occurred in the gd_entry() call.  The gd_entry type is defined in the
  getdata module to be:

  type gd_entry
    integer :: field_type, n_fields, spf, data_type, bitnum, numbits, shift
    integer :: fragment_index, flags, poly_ord, array_len, windop
    integer :: ithreshold, count_val, period
    character (len=GD_FIELD_LEN), dimension(3) :: field
    character (len=GD_FIELD_LEN), dimension(6) :: scalar
    integer, dimension(6) :: scalar_ind
    double precision, dimension(3) :: m, b
    double precision, dimension(6) :: a
    double precision :: dividend, rthreshold
    double complex, dimension(3) :: cm, cb
    double complex, dimension(6) :: ca
    double complex :: cdividend
  end type

  where GD_FIELD_LEN is a constant parameter equal to 4096.  Character strings
  are all stored in field(:) according to the following table:

    TYPES               FIELD(1)    FIELD(2)    FIELD(3)
    ------------------  ----------  ----------  ----------
    RAW                  --          --          --

    LINCOM              infield[0]  infield[1]  infield[2]

    LINTERP             infield[0]  table        --

    BIT / SBIT /
    PHASE / RECIP /     infield[0]   --          --
    POLYNOM

    MULTIPLY / DIVIDE / infield[0]  infield[1]   --
    WINDOW


  Furthermore, data_type does double duty as the data_type parameter for RAW
  fields, and the const_type parameter for CONST and CARRAY fields.  For WINDOW
  fields, only one of ithreshold and rthreshold will be set, according to the
  windop parameter.

* subroutine fgd_add (dirfile_unit, field_code, ent)
  integer, intent(in) :: dirfile_unit
  character (len=*), intent(in) :: field_code
  type(gd_entry), intent(in) :: ent

  This subroutine adds the field indicated by field_code and described by ent
  into specified dirfile.

  Analogously, for adding meta fields:

* subroutine fgd_madd (dirfile_unit, parent, field_code, ent)
  integer, intent(in) :: dirfile_unit
  character (len=*), intent(in) :: parent, field_code
  type(gd_entry), intent(in) :: ent

* subroutine fgd_alter_entry (dirfile, field_code, ent, recode)
  integer, intent(in) :: dirfile, recode
  character (len=*), intent(in) :: field_code
  type(gd_entry), intent(in) :: ent

  This subroutine alters the field or metafield specified.
