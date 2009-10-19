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
compatibility library will emit an error message on standard error and raise
SIGABRT.  Passing an invalid dirfile unit number to a procedure which requires
one as input (other than fdirfile_close, which will simply ignore it) will
result in the call failing with error code GD_E_BAD_DIRFILE.

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

* integer function fdirfile_open(dirfilename, flags)
  character (len=*), intent(in) :: dirfilename
  integer, intent(in) :: flags

* integer function fdirfile_cbopen (dirfilename, flags, sehandler)
  character (len=*), intent (in) :: dirfilename
  integer, intent (in) :: flags
  interface
    subroutine sehandler(act, dirfile_unit, suberror, line)
      integer, intent (out) :: act
      integer, intent (in) :: dirfile_unit, suberror
      character (len=GD_MAX_LINE_LENGTH), intent (inout) :: line
    end subroutine
  end interface

  (GD_FIELD_LEN is a constant parameter defined in the module)

* subroutine fdirfile_close (dirfile_unit)
  integer, intent(in) :: dirfile

* subroutine fdirfile_discard (dirfile_unit)
  integer, intent(in) :: dirfile

* subroutine fdirfile_flush (dirfile_unit, field_code)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_code

  (If field_code is the empty string, the entire dirfile will be flushed.)

* subroutine fdirfile_metaflush (dirfile_unit)
  integer, intent(in) :: dirfile

* character (len=GD_FIELD_LEN) function fget_format_filename(dirfile_unit, ind)
  integer, intent(in) :: dirfile_unit, ind

  (GD_FIELD_LEN is a constant parameter defined in the module)

* integer function fget_nfields (dirfile_unit)
  integer, intent(in) :: dirfile_unit

  * integer function fget_nfields_by_type (dirfile_unit, type)
  integer, intent(in) :: dirfile_unit, type

* integer function fget_nvectors (dirfile_unit)
  integer, intent(in) :: dirfile_unit

* integer function fget_nmetafields (dirfile_unit, parent)
  integer, intent(in) :: dirfile_unit
  character, intent(in) :: parent

* integer function fget_nmetafields_by_type (dirfile_unit, parent, type)
  integer, intent(in) :: dirfile_unit, type
  character, intent(in) :: parent

* integer function fget_nmetavectors (dirfile_unit, parent)
  integer, intent(in) :: dirfile_unit
  character, intent(in) :: parent

* integer function fget_nformats (dirfile_unit)
  integer, intent(in) :: dirfile_unit

* integer function fget_nframes (dirfile_unit)
  integer, intent(in) :: dirfile_unit

* integer function fget_spf (dirfile_unit, field_code)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_code

* integer function fget_error_string (dirfile, buffer, len)
  integer, intent(in) :: dirfile, len
  character (len=<len>), intent(out) :: buffer

* integer function fget_string (dirfile, field_code, length, data_out)
  integer, intent(in) :: dirfile, length
  character (len=*), intent(in) :: field_code
  character (len=*), intent(out) :: data_out

* integer function fput_string (dirfile, field_code, data_in)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_code, data_in

* subroutine fdirfile_add_spec (dirfile, spec, format_file)
  integer, intent(in) :: dirfile, format_file
  character (len=*), intent(in) :: spec

* subroutine fdirfile_madd_spec (dirfile, spec, parent)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: parent, spec

* subroutine fdirfile_include (dirfile, inc_file, flags)
  integer, intent(in) :: dirfile, format_file, flags
  character (len=*), intent(in) :: inc_file

* subroutine fdirfile_add_bit (dirfile, field_name, in_field, bitnum, numbits,
  fragment_index)
  integer, intent(in) :: dirfile, bitnum, numbits, fragment_index
  character (len=*), intent(in) :: field_name, in_field

* subroutine fdirfile_add_const (dirfile, field_name, const_type,
  fragment_index)
  integer, intent(in) :: dirfile, const_type, fragment_index
  character (len=*), intent(in) :: field_name

  (Unlike the C counterpart, this version cannot be used to simultaneously
  set the value of the constant.  Use one of the put_constant procedures after
  creating the field).

* subroutine fdirfile_add_clincom (dirfile, field_name, n_fields, in_field1, m1,
  b1, in_field2, m2, b2, in_field3, m3, b3, fragment_index)
  integer, intent(in) :: dirfile, n_fields, fragment_index
  character (len=*), intent(in) :: field_name, in_field1, in_field2, in_field3
  double complex, intent(in) :: m1, b1, m2, b2, m3, b3

* subroutine fdirfile_add_lincom (dirfile, field_name, n_fields, in_field1, m1,
  b1, in_field2, m2, b2, in_field3, m3, b3, fragment_index)
  integer, intent(in) :: dirfile, n_fields, fragment_index
  character (len=*), intent(in) :: field_name, in_field1, in_field2, in_field3
  double precision, intent(in) :: m1, b1, m2, b2, m3, b3

* subroutine fdirfile_add_linterp (dirfile, field_name, in_field, table,
  fragment_index)
  integer, intent(in) :: dirfile, fragment_index
  character (len=*), intent(in) :: field_name, in_field, table

* subroutine fdrifile_add_multiply (dirfile, field_name, in_field1, in_field2,
  fragment_index)
  integer, intent(in) :: dirfile, fragment_index
  character (len=*), intent(in) :: field_name, in_field1, in_field2

* subroutine fdrifile_add_phase (dirfile, field_name, in_field, phase,
  fragment_index)
  integer, intent(in) :: dirfile, phase, fragment_index
  character (len=*), intent(in) :: field_name, in_field

* subroutine fdirfile_add_polynom (dirfile, field_name, poly_ord, in_field, a0,
  a1, a2, a3, a4, a5, fragment_index)
  integer, intent(in) :: dirfile, poly_ord, fragment_index
  character (len=*), intent(in) :: field_name, in_field
  double precision, intent(in) :: a0, a1, a2, a3, a4, a5

* subroutine fdirfile_add_cpolynom (dirfile, field_name, poly_ord, in_field, a0,
  a1, a2, a3, a4, a5, fragment_index)
  integer, intent(in) :: dirfile, poly_ord, fragment_index
  character (len=*), intent(in) :: field_name, in_field
  double complex, intent(in) :: a0, a1, a2, a3, a4, a5

* subroutine fdirfile_add_raw (dirfile, field_code, data_type, spf,
  fragment_index)
  integer, intent(in) :: dirfile, data_type, spf, fragment_index
  character (len=*), intent(in) :: field_code

* subroutine fdirfile_add_sbit (dirfile, field_name, in_field, bitnum, numbits,
  fragment_index)
  integer, intent(in) :: dirfile, bitnum, numbits, fragment_index
  character (len=*), intent(in) :: field_name, in_field

* subroutine fdirfile_add_string (dirfile, field_code, fragment_index)
  integer, intent(in) :: dirfile, fragment_index
  character (len=*), intent(in) :: field_code

  (Unlike the C counterpart, this version cannot be used to simultaneously
  set the value of the string.  Use the put_string procedures after creating
  the field).

* subroutine fdirfile_madd_bit (dirfile, parent, field_name, in_field, bitnum,
  numbits)
  integer, intent(in) :: dirfile, bitnum, numbits
  character (len=*), intent(in) :: field_name, in_field, parent

* subroutine fdirfile_madd_const (dirfile, parent, field_name, const_type)
  integer, intent(in) :: dirfile, const_type
  character (len=*), intent(in) :: field_name, parent

  (Unlike the C counterpart, this version cannot be used to simultaneously
  set the value of the constant.  Use one of the put_constant procedures after
  creating the field).

* subroutine fdirfile_madd_lincom (dirfile, parent, field_name, n_fields,
  in_field1, m1, b1, in_field2, m2, b2, in_field3, m3, b3)
  integer, intent(in) :: dirfile, n_fields
  character (len=*), intent(in) :: field_name, in_field1, in_field2, in_field3
  character (len=*), intent(in) :: parent
  double precision, intent(in) :: m1, b1, m2, b2, m3, b3

* subroutine fdirfile_madd_clincom (dirfile, parent, field_name, n_fields,
  in_field1, m1, b1, in_field2, m2, b2, in_field3, m3, b3)
  integer, intent(in) :: dirfile, n_fields
  character (len=*), intent(in) :: field_name, in_field1, in_field2, in_field3
  character (len=*), intent(in) :: parent
  double complex, intent(in) :: m1, b1, m2, b2, m3, b3

* subroutine fdirfile_madd_linterp (dirfile, parent, field_name, in_field,
  table)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_name, in_field, table, parent

* subroutine fdrifile_madd_multiply (dirfile, parent, field_name, in_field1,
  in_field2)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_name, in_field1, in_field2, parent

* subroutine fdrifile_madd_phase (dirfile, parent, field_name, in_field, phase)
  integer, intent(in) :: dirfile, phase
  character (len=*), intent(in) :: field_name, in_field, parent

* subroutine fdirfile_madd_polynom (dirfile, parent, field_name, poly_ord,
  in_field, a0, a1, a2, a3, a4, a5)
  integer, intent(in) :: dirfile, poly_ord
  character (len=*), intent(in) :: field_name, in_field, parent
  double precision, intent(in) :: a0, a1, a2, a3, a4, a5

* subroutine fdirfile_madd_cpolynom (dirfile, parent, field_name, poly_ord,
  in_field, a0, a1, a2, a3, a4, a5)
  integer, intent(in) :: dirfile, poly_ord
  character (len=*), intent(in) :: field_name, in_field, parent
  double precision, intent(in) :: a0, a1, a2, a3, a4, a5

* subroutine fdirfile_madd_sbit (dirfile, parent, field_name, in_field, bitnum,
  numbits)
  integer, intent(in) :: dirfile, bitnum, numbits
  character (len=*), intent(in) :: field_name, in_field, parent

* subroutine fdirfile_madd_string (dirfile, parent, field_code)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_code, parent

  (Unlike the C counterpart, this version cannot be used to simultaneously
  set the value of the string.  Use the put_string procedures after creating
  the field).

* character (len=GD_FIELD_LEN) function fget_reference (dirfile)
  integer, intent(in) :: dirfile

* character (len=GD_FIELD_LEN)  function fdirfile_reference (dirfile, &
  field_code)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_code

* subroutine fdirfile_parser_callback (dirfile, sehandler)
  integer, intent(in) :: dirfile
  interface
    subroutine sehandler(act, dirfile_unit, suberror, line)
      integer, intent (out) :: act
      integer, intent (in) :: dirfile_unit, suberror
      character (len=GD_MAX_LINE_LENGTH), intent (inout) :: line
    end subroutine
  end interface

* integer function fget_encoding (dirfile, fragment)
  integer, intent(in) :: dirfile, fragment

* subroutine fdirfile_alter_encoding (dirfile, encoding, fragment, recode)
  integer, intent(in) :: dirfile, encoding, fragment, recode

* integer function fget_endianness (dirfile, fragment)
  integer, intent(in) :: dirfile, fragment

* subroutine fdirfile_alter_endianness (dirfile, endianness, fragment, recode)
  integer, intent(in) :: dirfile, endianness, fragment, recode

* integer function fget_frameoffset (dirfile, fragment)
  integer, intent(in) :: dirfile, fragment

* subroutine fdirfile_alter_frameoffset (dirfile, frameoffset, fragment, recode)
  integer, intent(in) :: dirfile, frameoffset, fragment, recode

* integer function fget_protection (dirfile, fragment)
  integer, intent(in) :: dirfile, fragment

* subroutine fdirfile_protect (dirfile, protection_level, fragment)
  integer, intent(in) :: dirfile, protection_level, fragment

* integer function fget_native_type (dirfile, field_code)
  integer integer(in) :: dirfile
  character (len=*), intent(in) :: field_code

* integer function fget_parent_fragment (dirfile, fragment)
  integer, intent(in) :: dirfile, fragment

* subroutine fdirfile_uninclude (dirfile, fragment, del)
  integer, intent(in) :: dirfile, fragment, del

* character (len=GD_MAX_LINE_LENGTH) function fget_raw_filename (dirfile,
  field_code)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_code

* subroutine fdirfile_move (dirfile, field_code, new_fragment, move_data)
  integer, intent(in) :: dirfile, new_fragment, move_data
  character (len=*), intent(in) :: field_code

* subroutine fdirfile_alter_bit (dirfile, field_name, in_field, bitnum, numbits)
  integer, intent(in) :: dirfile, bitnum, numbits
  character (len=*), intent(in) :: field_name, in_field

* subroutine fdirfile_alter_const (dirfile, field_name, const_type)
  integer, intent(in) :: dirfile, const_type
  character (len=*), intent(in) :: field_name

* subroutine fdirfile_alter_lincom (dirfile, field_name, n_fields, in_field1,
  m1, b1, in_field2, m2, b2, in_field3, m3, b3)
  integer, intent(in) :: dirfile, n_fields
  character (len=*), intent(in) :: field_name, in_field1, in_field2, in_field3
  double precision, intent(in) :: m1, b1, m2, b2, m3, b3

* subroutine fdirfile_alter_linterp (dirfile, field_name, in_field, table, move)
  integer, intent(in) :: dirfile, move
  character (len=*), intent(in) :: field_name, in_field, table

* subroutine fdrifile_alter_multiply (dirfile, field_name, in_field1, in_field2)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_name, in_field1, in_field2

* subroutine fdrifile_alter_phase (dirfile, field_name, in_field, phase)
  integer, intent(in) :: dirfile, phase
  character (len=*), intent(in) :: field_name, in_field

* subroutine fdirfile_alter_raw (dirfile, field_code, data_type, spf, move)
  integer, intent(in) :: dirfile, data_type, spf, move
  character (len=*), intent(in) :: field_code

* subroutine fdirfile_alter_spec (dirfile, spec, recode)
  integer, intent(in) :: dirfile, recode
  character (len=*), intent(in) :: spec

* subroutine fdirfile_malter_spec (dirfile, spec, parent, recode)
  integer, intent(in) :: dirfile, recode
  character (len=*), intent(in) :: parent, spec

* subroutine fdirfile_rename (dirfile, field_code, new_name, move_data)
  integer, intent(in) :: dirfile, move_data
  character (len=*), intent(in) :: field_code, new_name

* subroutine fdirfile_delete (dirfile, field_code, flags)
  integer, intent(in) :: dirfile, flags
  character (len=*), intent(in) :: field_code

* integer function fget_entry_type (dirfile, field_code)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_code

* integer function fget_fragment_index (dirfile, field_code)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_code

* integer function fdirfile_validate (dirfile, field_code)
  integer integer(in) :: dirfile
  character (len=*), intent(in) :: field_code

* double precision function fget_framenum (dirfile, field_code, value)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_code
  double precision, intent(in) :: value

* double precision function fget_framenum_subset (dirfile, field_code, value,
  frame_start, frame_end)
  integer, intent(in) :: dirfile, frame_start, frame_end
  character (len=*), intent(in) :: field_code
  double precision, intent(in) :: value 

* subroutine fdirfilename (dirfilename, diriflename_l, dirfile, fragment_index)
  character (len=*), intent(out) :: dirfilename
  integer, intent(in) :: dirfile, fragment_index
  integer, intent(inout) :: diriflename_l

In order to respect type safety, the getdata and putdata analogues encode
the datatype of their array in their function name, rather than as a parameter.
Otherwise, they behave the same as their C counterparts.

* integer function fgetdata_n (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples)
  integer, intent(in) :: dirfile_unit
  integer, intent(in) :: first_frame, first_sample, num_frames, num_samples
  character (len=*), intent(in) :: field_code

  This calls getdata(3) with return_type = GD_NULL (i.e. return no data).  As a
  result, no data_out parameter is required.

* integer function fgetdata_i1 (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples, data_out)
* integer function fgetdata_i2 (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples, data_out)
* integer function fgetdata_i4 (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples, data_out)
* integer function fgetdata_i8 (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples, data_out)
* integer function fgetdata_r4 (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples, data_out)
* integer function fgetdata_r8 (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples, data_out)
* integer function fgetdata_c8 (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples, data_out)
* integer function fgetdata_c16 (dirfile_unit, field_code, first_frame,
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

* integer function fputdata_i1 (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples, data_in)
* integer function fputdata_i2 (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples, data_in)
* integer function fputdata_i4 (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples, data_in)
* integer function fputdata_i8 (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples, data_in)
* integer function fputdata_r4 (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples, data_in)
* integer function fputdata_r8 (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples, data_in)
* integer function fputdata_c8 (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples, data_out)
* integer function fputdata_c16 (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples, data_out)
  integer, intent(in) :: dirfile_unit
  integer, intent(in) :: first_frame, first_sample, num_frames, num_samples
  character (len=*), intent(in) :: field_code
  <datatype>, dimension(:), intent(in) :: data_in

  No corresponding fputdata_n function exists, since GD_NULL is not an
  acceptable input data_type for putdata(3).

  Analogously for get_constant and put_constant, for which only the _i1
  versions are shown here:

* integer function fget_constant_i1 (dirfile, field_code, data_out)
  integer, intent(in) :: dirfile_unit
  character (len=*), intent(in) :: field_code
  <datatype>, intent(out) :: data_out
* integer function fput_string_i1 (dirfile, field_code, data_in)
  integer, intent(in) :: dirfile_unit
  character (len=*), intent(in) :: field_code
  <datatype>, intent(in) :: data_in

Other procedures in the Fortran 95 bindings are:

* integer fget_field_name_max (dirfile_unit)
  integer, intent(in) :: dirfile_unit

  This function returns the length of the longest field name defined in the
  dirfile.

* integer fget_mfield_name_max (dirfile_unit, parent)
  integer, intent(in) :: dirfile_unit
  character (len=*), intent(in) :: parent

  This function returns the length of the longest field name defined in the
  dirfile for META fields of the supplied parent field.

* subroutine fget_field_list (field_list, dirfile, field_len)
  character (len=<field_len>) dimension(:), intent(out) :: field_list
  integer, intent(in) :: dirfile_unit
  integer, intent(inout) :: field_len

  This subroutine behaves analogously to get_field_list(3), except that it
  requires a third argument, field_len, which is the longest field name which
  will fit in the supplied field_list array.  If the longest field name in the
  dirfile is longer than field_len, field_len will be set to this value (which
  is equivalent to the return value of fget_field_name_max) and field_list will
  remain untouched.  The character strings returned are Fortran strings.

  Analogously:

* subroutine fget_field_list_by_type (field_list, dirfile, entype, field_len)
  character (len=<field_len>) dimension(:), intent(out) :: field_list
  integer, intent(in) :: dirfile_unit, entype
  integer, intent(inout) :: field_len
* subroutine fget_vector_list (field_list, dirfile, field_len)
  character (len=<field_len>) dimension(:), intent(out) :: field_list
  integer, intent(in) :: dirfile_unit
  integer, intent(inout) :: field_len

  Also analogously, except that field_len will be equivalent to the return
  value of fget_mfield_name_max, if too small:

* subroutine fget_mfield_list (field_list, dirfile, field_len)
  character (len=<field_len>) dimension(:), intent(out) :: field_list
  integer, intent(in) :: dirfile_unit
  integer, intent(inout) :: field_len
  character (len=*), intent(in) :: parent
* subroutine fget_mfield_list_by_type (field_list, dirfile, entype,
  field_len)
  character (len=<field_len>) dimension(:), intent(out) :: field_list
  integer, intent(in) :: dirfile_unit, entype
  integer, intent(inout) :: field_len
  character (len=*), intent(in) :: parent
* subroutine fget_mvector_list (field_list, dirfile, field_len)
  character (len=<field_len>) dimension(:), intent(out) :: field_list
  integer, intent(in) :: dirfile_unit
  integer, intent(inout) :: field_len
  character (len=*), intent(in) :: parent

* integer function fget_error (dirfile_unit)
  integer, intent(in) :: dirfile_unit
 
  This returns the DIRFILE error associated with the supplied dirfile unit.

* integer function fget_entry (dirfile_unit, field_code, ent)
  integer, intent(in) :: dirfile_unit
  character (len=*), intent(in) :: field_code
  type(gd_entry), intent(out) :: ent

  This fills ent with the metadata for field_code obtained by calling
  get_entry(3).  It returns the field type, or GD_NO_ENTRY if an error occurred
  in the get_entry() call.  The gd_entry type is defined in the getdata module
  to be:

  type gd_entry
    integer :: field_type, n_fields, spf, data_type, bitnum, numbits, shift
    integer :: format_file
    character(len=GD_FIELD_LEN), dimension(3) :: field
    double precision, dimension(3) :: m, b
  end type

  where GD_FIELD_LEN is a constant parameter equal to 4096.  Character strings
  are all stored in field(:) according to the following table:

    TYPE      FIELD(1)    FIELD(2)    FIELD(3)
    --------  ----------  ----------  ----------
    RAW        --          --          --
    LINCOM    infield[0]  infield[1]  infield[2]
    LINTERP   infield[0]  table        --
    BIT       infield[0]   --          --
    MULTIPLY  infield[0]  infield[1]   --
    PHASE     infield[0]   --          --

  Furthermore, data_type does double duty as the data_type parameter for RAW
  fields, and the const_type parameter for CONST fields.

* subroutine fdirfile_add (dirfile_unit, field_code, ent)
  integer, intent(in) :: dirfile_unit
  character (len=*), intent(in) :: field_code
  type(gd_entry), intent(in) :: ent

  This subroutine adds the field indicated by field_code and described by ent
  into specified dirfile.

  Analogously, for adding meta fields:

* subroutine fdirfile_madd (dirfile_unit, parent, field_code, ent)
  integer, intent(in) :: dirfile_unit
  character (len=*), intent(in) :: parent, field_code
  type(gd_entry), intent(in) :: ent

* subroutine fdirfile_alter_entry (dirfile, field_code, ent, recode)
  integer, intent(in) :: dirfile, recode
  character (len=*), intent(in) :: field_code
  type(gd_entry), intent(in) :: ent

  This subroutine alters the field or metafield specified.
