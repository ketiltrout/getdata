FORTRAN 95 BINDINGS FOR GETDATA
===============================

This README describes the Fortran 95 bindings for the GetData library.  These
bindings consist of a Fortran 95 compatibility library `libf95getdata' (written
in Fortran 95) and a Fortran 95 module file `getdata.mod' or `getdata.f95'
which defines the interface.  The Fortran 95 bindings require the Fortran 77
comatibility library `libfgetdata' for operation.

For programs which can use Fortran 95 modules, these bindings should be
preferred over the Fortran 77 bindings, since these bindings provide type
safety and more legible symbols.

As in the Fortran 77 bindings, dirfiles are referred to by "dirfile unit"
numbers, which are internally converted to the C API's DIRFILE pointers
internally.  Space is available in the compatibility library for only 1023
dirfile units.  If an application attempts to open more than 1023 dirfiles
simultaneously, the compatibility library will emit an error message and raise
SIGABRT.  Passing an invalid dirfile unit number to a procedure which requires
one as input (other than fdirfile_close, which will simply ignore it) will
result in the call failing with error code GD_E_BAD_DIRFILE.

The "getdata" module, which these bindings define, is described in `getdata.mod'
(which will be installed in the same directory as getdata.h).  The getdata
module defines the same error codes (GD_E_OK, GD_E_OPEN, &c.), the same
open flags (GD_RDONLY, GD_CREAT, &c.) and the same data type specifiers
(GD_INT8, GD_FLOAT32) as the C API (although these last are rarely needed
in the Fortran 95 bindings, see below.

Available Procedures
====================

Notably, unlinke the Fortran 77 bindings, the Fortran 95 bindings do not
require passing character string lengths along with the string itself to
procedures.

Procedures which are essentially equivalent to their C API counterparts, with
the exception of including an initial `f' in their names, and using dirfile
unit numbers in place of C's dirfile pointers are:

* integer function fdrifile_open(dirfilename, flags)
  character (len=*), intent(in) :: dirfilename
  integer, intent(in) :: flags

* subroutine fdirfile_close (dirfile_unit)
  integer, intent(in) :: dirfile

* subroutine fdirfile_flush (dirfile_unit, field_code)
  integer, intent(in) :: dirifle
  character (len=*), intent(in) :: field_cde

  (If field_code is the empty string, the entire dirfile will be flushed.)

* integer function fget_nfields (dirfile_unit)
  integer, intent(in) :: dirfile_unit

* integer function fget_nframes (dirfile_unit)
  integer, intent(in) :: dirfile_unit

* integer function fget_spf (dirfile_unit, field_code)
  integer, intent(in) :: dirfile
  character (len=*), intent(in) :: field_code

* integer function fget_error_string (dirfile, buffer, len)
  integer, intent(in) :: dirfile, len
  character (len=<len>), intent(out) :: buffer

In order to respect type safety, the getdata and putdata analogues endcode
the datatype of their array in their function name, rather than as a parameter.
Otherwise, they behave the same as their C counterparts.

* integer function fgetdata_n (dirfile_unit, field_code, first_frame,
  first_sample, num_frames, num_samples)
  integer, intent(in) :: dirfile_unit
  integer, intent(in) :: first_frame, first_sample, num_frames, num_samples
  character (len=*), intent(in) :: field_code

  This calls getdata(3) with return_type = GD_NULL (ie. return no data).  As a
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
  integer, intent(in) :: dirfile_unit
  integer, intent(in) :: first_frame, first_sample, num_frames, num_samples
  character (len=*), intent(in) :: field_code
  <datatype>, dimension(:), intent(out) :: data_out

  These call getdata(3) with return_type = GD_INT8, GD_INT16, GD_INT32,
  GD_INT64, GD_FLOAT32, and GD_FLOAT64, respectively.  Here <datatype> is an
  integer or real type of the appropriate width.

  Analagously, for putdata:

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
  integer, intent(in) :: dirfile_unit
  integer, intent(in) :: first_frame, first_sample, num_frames, num_samples
  character (len=*), intent(in) :: field_code
  <datatype>, dimension(:), intent(in) :: data_in

  No corresponding fputdata_n function exists, since GD_NULL is not an
  acceptable input data_type for putdata(3).

Other procedures in the Fortran 95 bindings are:

* integer fget_field_name_max (dirfile_unit)
  integer, intent(in) :: dirfile_unit

  This function returns the length of the longest field name defined in the
  dirfile.

* subroutine fget_field_list (field_list, dirfile, field_len)
  character (len=<field_len>) dimension(:), intent(out) :: field_list
  integer, intent(in) :: dirfile_unit
  integer, intent(inout) :: field_len

  This subroutine behaves analagously to get_field_list(3), except that it
  requires a third argument, field_len, which is the longest field name which
  will fit in the supplied field_list array.  If the longest field name in the
  dirfile is longer than field_len, field_len will be set to this value (which
  is equivalent to the return value of fget_field_name_max) and field_list will
  remain untouched.  The character strings returned are Fortran strings.

* integer function fget_error (dirfile_unit)
  integer, intent(in) :: dirfile_unit
 
  This returns the DIRFILE error associated with the supplied dirfile unit.

* integer function fget_entry (dirfile_unit, field_code, ent)
  integer, intent(in) :: dirfile_unit
  character (len=*), intent(in) :: field_code
  type(gd_entry), intent(out) :: ent

  This fill ent with the metadata for field_code obtained by calling
  get_entry(3).  It returns the field type, or GD_NO_ENTRY if an error occurred
  in the get_entry() call.  The gd_enty type is defined in the getdata module
  to be:

  type gd_entry
    integer :: field_type, n_fields, spf, data_type, bitnum, numbits, shift
    character(len=GD_FIELD_LEN), dimension(3) :: field
    real*8, dimension(3) :: m, b
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
