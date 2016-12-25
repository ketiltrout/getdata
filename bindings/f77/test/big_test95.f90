! Copyright (C) 2009-2015 D. V. Wiebe
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! This file is part of the GetData project.
!
! GetData is free software; you can redistribute it and/or modify it under
! the terms of the GNU Lesser General Public License as published by the
! Free Software Foundation; either version 2.1 of the License, or (at your
! option) any later version.
!
! GetData is distributed in the hope that it will be useful, but WITHOUT
! ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
! FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
! License for more details.
!
! You should have received a copy of the GNU Lesser General Public License
! along with GetData; if not, write to the Free Software Foundation, Inc.,
! 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
!
! -*- F95 -*-

! General test
!
! This very large test checks almost every procedure defined by the F95
! bindings.  Procedures not tested include: fgd_cbopen fgd_metaflush
! fgd_flush fgd_close fgd_callback fgd_discard
! (although this last one is used)

! check functions
subroutine check_err2(ne, t, i, d, v)
  use getdata
  integer, intent(inout) :: ne
  integer, intent(in) :: t, i, d, v
  integer :: e

  e = fgd_error(d)

  if (e .ne. v) then
    ne = ne + 1
    write(*, 9006), i, t, e, v
  end if
9006 format('e(', i0, ')[', i0, '] = ', i0, ', expected ', i0)
end subroutine 

subroutine check_err(ne, t, d, v)
  integer, intent(inout) :: ne
  integer, intent(in) :: t, d, v
  call check_err2(ne, t, 0, d, v)
end subroutine 

subroutine check_int2(ne, t, i, n, v)
  integer, intent(inout) :: ne
  integer, intent(in) :: t, i, n, v

  if (n .ne. v) then
    ne = ne + 1
    write(*, 9007), i, t, n, v
  end if
9007 format('n(', i0, ')[', i0, '] = ', i0, ', expected ', i0)
end subroutine 

subroutine check_int(ne, t, n, v)
  integer, intent(inout) :: ne
  integer, intent(in) :: t, n, v
  call check_int2(ne, t, 0, n, v)
end subroutine 

subroutine check_str2(ne, t, i, n, v)
  integer, intent(inout) :: ne
  integer, intent(in) :: t, i
  character (len=*), intent(in) :: n, v

  if (n .ne. v) then
    ne = ne + 1
    write(*, 9008), i, t, n, v
  end if
9008 format('s(', i0, ')[', i0, '] = "', a, '", expected "', a, '"')
end subroutine 

subroutine check_str(ne, t, n, v)
  integer, intent(inout) :: ne
  integer, intent(in) :: t
  character (len=*), intent(in) :: n, v
  call check_str2(ne, t, 0, n, v)
end subroutine 

subroutine check_dbl2(ne, t, i, n, v)
  integer, intent(inout) :: ne
  integer, intent(in) :: t, i
  double precision, intent(in) :: n, v

  ! this is good to single precision
  if (abs(n - v) .gt. 1e-7) then
    ne = ne + 1
    write(*, 9010), i, t, n, v
  end if
9010 format('r(', i0, ')[', i0, '] = ', d16.10, ', expected ', d16.10)
end subroutine 

subroutine check_dbl(ne, t, n, v)
  integer, intent(inout) :: ne
  integer, intent(in) :: t
  double precision, intent(in) :: n, v
  call check_dbl2(ne, t, 0, n, v)
end subroutine 

subroutine check_cpx2(ne, t, i, n, v)
  integer, intent(inout) :: ne
  integer, intent(in) :: t, i
  double complex, intent(in) :: n, v

  ! this is good to single precision
  if (abs(n - v) .gt. 1e-7) then
    ne = ne + 1
    write(*, 9011), i, t, real(real(n)), real(aimag(n)), &
    real(real(v)), real(aimag(v))
  end if
9011 format('x(', i0, ')[', i0, '] = ', d16.10, ';', d16.10, ', expected ', &
d16.10, ';', d16.10)
end subroutine 

subroutine check_cpx(ne, t, n, v)
  integer, intent(inout) :: ne
  integer, intent(in) :: t
  double complex, intent(in) :: n, v
  call check_cpx2(ne, t, 0, n, v)
end subroutine 

subroutine check_ok2(ne, t, i, d)
  use getdata
  integer, intent(inout) :: ne
  integer, intent(in) :: t, d, i
  call check_err2(ne, t, i, d, GD_E_OK)
end subroutine

subroutine check_ok(ne, t, d)
  integer, intent(inout) :: ne
  integer, intent(in) :: t, d
  call check_ok2(ne, t, 0, d)
end subroutine

subroutine check_eos(ne, t, n, v)
  integer, intent(inout) :: ne
  integer, intent(in) :: t
  character (len=*), intent(in) :: n, v
  integer :: f

  f = index(n, v)
  if (f .eq. 0) then
    f = 1
  end if

  call check_str(ne, t, n(f:), v)
end subroutine
  



program big_test
  use getdata
  implicit none
  include "test_getdata.f"
  character (len=*), parameter :: fildir = 'test95_dirfile'
  character (len=*), parameter :: frmat = 'test95_dirfile/format'
  character (len=*), parameter :: frm2 = 'test95_dirfile/form2'
  character (len=*), parameter :: dat = 'test95_dirfile/data'
  integer, parameter :: flen = 11
  integer, parameter :: nfields = 20
  integer, parameter :: slen = 26

  character (len=slen), dimension(3) :: strings
  character (len=slen), dimension(3) :: st
  character (len=flen), dimension(nfields + 9) :: fields
  character (len=flen), dimension(nfields + 9) :: flist
  character (len=GD_FIELD_LEN) :: str
  character (len=4096) :: path
  integer(1), dimension(80) :: datadata
  integer :: i, d, n, l, ne
  real :: fl
  double precision :: dp
  double complex :: dc
  integer(8), dimension(6) :: iq
  double precision, dimension(6) :: q
  double complex, dimension(6) :: cq
  type(gd_entry) :: ent

  integer(1), dimension(8) :: ci1
  integer(2), dimension(8) :: ci2
  integer(4), dimension(8) :: ci4
  integer(8), dimension(8) :: ci8
  real, dimension(8) :: cr4
  double precision, dimension(8) :: cr8
  complex, dimension(8) :: cc8
  double complex, dimension(8) :: cc16

  do i=1,80
    datadata(i) = int(i,1)
  end do

  call system ( 'rm -rf ' // fildir )
  call system ( 'mkdir ' // fildir )

  fields = (/      'bit        ', 'div        ', 'data       ', 'mult       ', &
    'sbit       ', 'INDEX      ', 'alias      ', 'const      ', 'indir      ', &
    'mplex      ', 'phase      ', 'recip      ', 'carray     ', 'lincom     ', &
    'sarray     ', 'sindir     ', 'string     ', 'window     ', 'linterp    ', &
    'polynom    ', '           ', '           ', '           ', '           ', &
    '           ', '           ', '           ', '           ', '           ' /)

  open(1, file=frmat, status='new')
  write(1, *) '/ENDIAN little'
  write(1, *) 'data RAW INT8 8'
  write(1, *) 'lincom LINCOM data 1.1 2.2 INDEX 2.2 3.3;4.4 linterp const const'
  write(1, *) '/META data mstr STRING "This is a string constant."'
  write(1, *) '/META data mconst CONST COMPLEX128 3.3;4.4'
  write(1, *) '/META data mlut LINTERP DATA ./lut'
  write(1, *) 'const CONST FLOAT64 5.5'
  write(1, *) 'carray CARRAY FLOAT64 1.1 2.2 3.3 4.4 5.5 6.6'
  write(1, *) 'linterp LINTERP data ./lut'
  write(1, *) 'polynom POLYNOM data 1.1 2.2 2.2 3.3;4.4 const const'
  write(1, *) 'bit BIT data 3 4'
  write(1, *) 'sbit SBIT data 5 6'
  write(1, *) 'mplex MPLEX data sbit 1 10'
  write(1, *) 'mult MULTIPLY data sbit'
  write(1, *) 'phase PHASE data 11'
  write(1, *) 'div DIVIDE mult bit'
  write(1, *) 'recip RECIP div 6.5;4.3'
  write(1, *) 'window WINDOW linterp mult LT 4.1'
  write(1, *) '/ALIAS alias data'
  write(1, *) 'string STRING "Zaphod Beeblebrox"'
  write(1, *) 'sarray SARRAY one two three four five six seven'
  write(1, *) 'data/msarray SARRAY eight nine ten eleven twelve'
  write(1, *) 'indir INDIR data carray'
  write(1, *) 'sindir SINDIR data sarray'
  close(1, status='keep')

  open(1, file=frm2, status='new')
  write(1, *) 'const2 CONST INT8 -19'
  close(1, status='keep')

  open(1, file=dat, form='unformatted', access='direct', recl=80, &
  status='new')
  write(1, rec=1) datadata
  close(1, status='keep')

  ne = 0

! 1: fgd_error check
  d = fgd_open('x', GD_RDONLY)
  call check_err(ne, 1, d, GD_E_IO)
  call fgd_discard(d)

! 2: fgd_open check
  d = fgd_open(fildir, GD_RDWR)
  call check_ok(ne, 2, d)

! 3: fgd_getdata_i1 check
  n = fgd_getdata_i1(d, 'data', 5, 0, 1, 0, ci1)
  call check_ok(ne, 3, d)
  call check_int(ne, 3, n, 8)

  do i=1,8
  call check_int2(ne, 3, i, int(ci1(i)), 40 + i)
  end do 

! 4: fgd_getdata_i2 check
  n = fgd_getdata_i2(d, 'data', 5, 0, 1, 0, ci2)
  call check_ok(ne, 4, d)
  call check_int(ne, 4, n, 8)

  do i=1,8
  call check_int2(ne, 4, i, int(ci2(i)), 40 + i)
  end do 

! 5: fgd_getdata_i4 check
  n = fgd_getdata_i4(d, 'data', 5, 0, 1, 0, ci4)
  call check_ok(ne, 5, d)
  call check_int(ne, 5, n, 8)

  do i=1,8
  call check_int2(ne, 5, i, int(ci4(i)), 40 + i)
  end do 

! 6: fgd_getdata_i8 check
  n = fgd_getdata_i8(d, 'data', 5, 0, 1, 0, ci8)
  call check_ok(ne, 6, d)
  call check_int(ne, 6, n, 8)

  do i=1,8
  call check_int2(ne, 6, i, int(ci8(i)), 40 + i)
  end do 

! 7: fgd_getdata_r4 check
  n = fgd_getdata_r4(d, 'data', 5, 0, 1, 0, cr4)
  call check_ok(ne, 7, d)
  call check_int(ne, 7, n, 8)

  do i=1,8
  call check_dbl2(ne, 7, i, 1d0 * cr4(i), 40d0 + i)
  end do 

! 8: fgd_getdata_r8 check
  n = fgd_getdata_r8(d, 'data', 5, 0, 1, 0, cr8)
  call check_ok(ne, 8, d)
  call check_int(ne, 8, n, 8)

  do i=1,8
  call check_dbl2(ne, 8, i, cr8(i), 40d0 + i)
  end do 

! 9: fgd_getdata_c8 check
  n = fgd_getdata_c8(d, 'data', 5, 0, 1, 0, cc8)
  call check_ok(ne, 9, d)
  call check_int(ne, 9, n, 8)

  do i=1,8
  call check_cpx2(ne, 9, i, 1d0 * cc8(i), dcmplx(40 + i, 0))
  end do 

! 10: fgd_getdata_c16 check
  n = fgd_getdata_c16(d, 'data', 5, 0, 1, 0, cc16)
  call check_ok(ne, 10, d)
  call check_int(ne, 10, n, 8)

  do i=1,8
  call check_cpx2(ne, 10, i, cc16(i), dcmplx(40 + i, 0))
  end do 

! 11: fgd_getdata_n check
  n = fgd_getdata_n(d, 'data', 5, 0, 1, 0)
  call check_ok(ne, 11, d)
  call check_int(ne, 11, n, 8)

! 12: fgd_get_constant_i1 check
  call fgd_get_constant_i1(d, 'const', ci1(1))
  call check_ok(ne, 12, d)
  call check_int(ne, 12, int(ci1(1)), 5)

! 13: fgd_get_constant_i2 check
  call fgd_get_constant_i2(d, 'const', ci2(1))
  call check_ok(ne, 13, d)
  call check_int(ne, 13, int(ci2(1)), 5)

! 14: fgd_get_constant_i4 check
  call fgd_get_constant_i4(d, 'const', ci4(1))
  call check_ok(ne, 14, d)
  call check_int(ne, 14, ci4(1), 5)

! 15: fgd_get_constant_i8 check
  call fgd_get_constant_i8(d, 'const', ci8(1))
  call check_ok(ne, 15, d)
  call check_int(ne, 15, int(ci8(1)), 5)

! 16: fgd_get_constant_r4 check
  call fgd_get_constant_r4(d, 'const', cr4(1))
  call check_ok(ne, 16, d)
  call check_dbl(ne, 16, 1d0 * cr4(1), 5.5d0)

! 17: fgd_get_constant_r8 check
  call fgd_get_constant_r8(d, 'const', cr8(1))
  call check_ok(ne, 17, d)
  call check_dbl(ne, 17, cr8(1), 5.5d0)

! 18: fgd_get_constant_c8 check
  call fgd_get_constant_c8(d, 'const', cc8(1))
  call check_ok(ne, 18, d)
  call check_cpx(ne, 18, 1d0 * cc8(1), dcmplx(5.5, 0))

! 19: fgd_get_constant_c16 check
  call fgd_get_constant_c16(d, 'const', cc16(1))
  call check_ok(ne, 19, d)
  call check_cpx(ne, 19, cc16(1), dcmplx(5.5, 0))

! 20: fgd_get_constant_n check
  call fgd_get_constant_n(d, 'const')
  call check_ok(ne, 20, d)

! 21: fgd_field_name_max check
  i = fgd_field_name_max(d)
  call check_ok(ne, 21, d)
  call check_int(ne, 21, i, 7)

! 22: fgd_mfield_name_max check
  i = fgd_mfield_name_max(d, 'data')
  call check_ok(ne, 22, d)
  call check_int(ne, 22, i, 7)

! 23: fgd_nfields check
  n = fgd_nfields(d)
  call check_ok(ne, 23, d)
  call check_int(ne, 23, n, nfields)

! 25: fgd_field_list check
  l = flen
  call fgd_field_list(flist, d, l)
  call check_ok(ne, 25, d)
  call check_int(ne, 25, l, flen)

  do i = 1, n
  call check_str2(ne, 25, i, flist(i), fields(i))
  end do

! 26: fgd_nmfields check
  n = fgd_nmfields(d, 'data')
  call check_ok(ne, 26, d)
  call check_int(ne, 26, n, 4)

! 27: fgd_mfield_list check
  fields(1) = 'mstr'
  fields(2) = 'mconst'
  fields(3) = 'mlut'
  fields(4) = 'msarray'

  l = flen
  call fgd_mfield_list(flist, d, 'data', l)
  call check_ok2(ne, 27, i, d)
  call check_int2(ne, 27, i, l, flen)

  DO i = 1, n
  call check_str2(ne, 27, i, flist(i), fields(i))
  end do

! 28: fgd_nframes check
  n = fgd_nframes(d)
  call check_ok(ne, 28, d)
  call check_int(ne, 28, n, 10)

! 29: fgd_spf check
  n = fgd_spf(d, 'data')
  call check_ok(ne, 29, d)
  call check_int(ne, 29, n, 8)

! 30: fgd_putdata_i1 check
  ci1 = (/ 13_1, 14_1, 15_1, 16_1, 17_1, 18_1, 19_1, 90_1 /)
  n = fgd_putdata_i1(d, 'data', 5, 1, 0, 4, ci1)
  call check_ok(ne, 30, d)
  call check_int(ne, 30, n, 4)

  n = fgd_getdata_i1(d, 'data', 5, 0, 1, 0, ci1)

  DO i = 1, 8
  if (i .eq. 1 .or. i .gt. 5) then
    n = 40 + i
  else
    n = 11 + i
  endif
  call check_int2(ne, 30, i, int(ci1(i)), n)
  end do

! 31: fgd_putdata_i2 check
  ci2 = (/ 23_2, 24_2, 25_2, 26_2, 27_2, 28_2, 29_2, 30_2 /)
  n = fgd_putdata_i2(d, 'data', 5, 1, 0, 4, ci2)
  call check_ok(ne, 31, d)
  call check_int(ne, 31, n, 4)

  n = fgd_getdata_i2(d, 'data', 5, 0, 1, 0, ci2)

  DO i = 1, 8
  if (i .eq. 1 .or. i .gt. 5) then
    n = 40 + i
  else
    n = 21 + i
  endif
  call check_int2(ne, 31, i, int(ci2(i)), n)
  end do

! 32: fgd_putdata_i4 check
  ci4 = (/ 33, 34, 35, 36, 37, 38, 39, 40 /)
  n = fgd_putdata_i4(d, 'data', 5, 1, 0, 4, ci4)
  call check_ok(ne, 32, d)
  call check_int(ne, 32, n, 4)

  n = fgd_getdata_i4(d, 'data', 5, 0, 1, 0, ci4)

  DO i = 1, 8
  if (i .eq. 1 .or. i .gt. 5) then
    n = 40 + i
  else
    n = 31 + i
  endif
  call check_int2(ne, 32, i, int(ci4(i)), n)
  end do

! 33: fgd_putdata_i8 check
  ci8 = (/ 43, 44, 45, 46, 47, 48, 49, 50 /)
  n = fgd_putdata_i8(d, 'data', 5, 1, 0, 4, ci8)
  call check_ok(ne, 33, d)
  call check_int(ne, 33, n, 4)

  n = fgd_getdata_i8(d, 'data', 5, 0, 1, 0, ci8)

  DO i = 1, 8
  if (i .eq. 1 .or. i .gt. 5) then
    n = 40 + i
  else
    n = 41 + i
  endif
  call check_int2(ne, 33, i, int(ci8(i)), n)
  end do

! 34: fgd_putdata_r4 check
  cr4 = (/ 33, 34, 35, 36, 37, 38, 39, 40 /)
  n = fgd_putdata_r4(d, 'data', 5, 1, 0, 4, cr4)
  call check_ok(ne, 34, d)
  call check_int(ne, 34, n, 4)

  n = fgd_getdata_r4(d, 'data', 5, 0, 1, 0, cr4)

  DO i = 1, 8
  if (i .eq. 1 .or. i .gt. 5) then
    dp = 40. + i
  else
    dp = 31. + i
  end if
  call check_dbl2(ne, 34, i, 1d0 * cr4(i), dp)
  end do

! 35: fgd_putdata_r8 check
  cr8 = (/ 43, 44, 45, 46, 47, 48, 49, 50 /)
  n = fgd_putdata_r8(d, 'data', 5, 1, 0, 4, cr8)
  call check_ok(ne, 35, d)
  call check_int(ne, 35, n, 4)

  n = fgd_getdata_r8(d, 'data', 5, 0, 1, 0, cr8)

  DO i = 1, 8
  if (i .eq. 1 .or. i .gt. 5) then
    dp = 40. + i
  else
    dp = 41. + i
  end if
  call check_dbl2(ne, 35, i, cr8(i), dp)
  end do

! 36: fgd_putdata_c8 check
  cc8 = (/ 53, 54, 55, 56, 57, 58, 59, 60 /)
  n = fgd_putdata_c8(d, 'data', 5, 1, 0, 4, cc8)
  call check_ok(ne, 36, d)
  call check_int(ne, 36, n, 4)

  n = fgd_getdata_c8(d, 'data', 5, 0, 1, 0, cc8)

  DO i = 1, 8
  if (i .eq. 1 .or. i .gt. 5) then
    dc = dcmplx(40d0 + i, 0d0)
  else
    dc = dcmplx(51d0 + i, 0d0)
  end if
  call check_cpx2(ne, 36, i, 1d0 * cc8(i), dc)
  end do

! 37: fgd_putdata_c16 check
  cc16 = (/ 63, 64, 65, 66, 67, 68, 69, 70 /)
  n = fgd_putdata_c16(d, 'data', 5, 1, 0, 4, cc16)
  call check_ok(ne, 37, d)
  call check_int(ne, 37, n, 4)

  n = fgd_getdata_c16(d, 'data', 5, 0, 1, 0, cc16)

  DO i = 1, 8
  if (i .eq. 1 .or. i .gt. 5) then
    dc = dcmplx(40d0 + i, 0d0)
  else
    dc = dcmplx(61d0 + i, 0d0)
  end if
  call check_cpx2(ne, 37, i, cc16(i), dc)
  end do

! 38: fgd_error_string check
  n = fgd_getdata_n(d, 'x', 5, 0, 1, 0)
  call fgd_error_string(d, str, GD_FIELD_LEN)
  call check_str(ne, 38, str, 'Field not found: x')

! 39: fgd_entry_type check
  n = fgd_entry_type(d, 'data')
  call check_ok(ne, 39, d)
  call check_int(ne, 39, n, GD_RAW_ENTRY)

! 40: fgd_entry (raw) check
  n = fgd_entry(d, 'data', ent)
  call check_ok(ne, 40, d)
  call check_int2(ne, 40, 1, n, GD_RAW_ENTRY)
  call check_int2(ne, 40, 2, ent%fragment_index, 0)
  call check_int2(ne, 40, 3, ent%spf, 8)
  call check_int2(ne, 40, 4, ent%data_type, GD_INT8)

! 42: fgd_entry (lincom) check
  n = fgd_entry(d, 'lincom', ent)
  call check_ok(ne, 42, d)
  call check_int2(ne, 42,  1, n, GD_LINCOM_ENTRY)
  call check_int2(ne, 42,  2, ent%n_fields, 3)
  call check_int2(ne, 42,  3, ent%fragment_index, 0)
  call check_str2(ne, 42,  4, ent%field(1), 'data')
  call check_str2(ne, 42,  5 , ent%field(2), 'INDEX')
  call check_str2(ne, 42,  6, ent%field(3), 'linterp')
  call check_int2(ne, 42,  7, ent%flags, GD_EN_CALC + GD_EN_COMPSCAL)
  call check_str2(ne, 42,  8, ent%scalar(1), '')
  call check_str2(ne, 42,  9, ent%scalar(2), '')
  call check_str2(ne, 42, 10, ent%scalar(3), 'const')
  call check_str2(ne, 42, 11, ent%scalar(4), '')
  call check_str2(ne, 42, 12, ent%scalar(5), '')
  call check_str2(ne, 42, 13, ent%scalar(6), 'const')
  call check_int2(ne, 42, 14, ent%scalar_ind(1), 1)
  call check_int2(ne, 42, 15, ent%scalar_ind(2), 1)
  call check_int2(ne, 42, 16, ent%scalar_ind(3), -1)
  call check_int2(ne, 42, 17, ent%scalar_ind(4), 1)
  call check_int2(ne, 42, 18, ent%scalar_ind(5), 1)
  call check_int2(ne, 42, 19, ent%scalar_ind(6), -1)

  cq(1) = dcmplx(1.1d0, 0.0d0)
  cq(2) = dcmplx(2.2d0, 0.0d0)
  cq(3) = dcmplx(2.2d0, 0.0d0)
  cq(4) = dcmplx(3.3d0, 4.4d0)
  cq(5) = dcmplx(5.5d0, 0.0d0)
  cq(6) = dcmplx(5.5d0, 0.0d0)
  DO i=1,3
  call check_cpx2(ne, 42, i * 2 + 18, ent%cm(i), cq(i * 2 - 1))
  call check_cpx2(ne, 42, i * 2 + 19, ent%cb(i), cq(i * 2))
  end do

! 44: fgd_entry (polynom) check
  n = fgd_entry(d, 'polynom', ent)
  call check_ok(ne, 44, d)
  call check_int2(ne, 44, 1, n, GD_POLYNOM_ENTRY)
  call check_int2(ne, 44, 2, ent%poly_ord, 5)
  call check_int2(ne, 44, 3, ent%fragment_index, 0)
  call check_str2(ne, 44, 4, ent%field(1), 'data')

  cq(1) = dcmplx(1.1d0, 0.0d0)
  cq(2) = dcmplx(2.2d0, 0.0d0)
  cq(3) = dcmplx(2.2d0, 0.0d0)
  cq(4) = dcmplx(3.3d0, 4.4d0)
  cq(5) = dcmplx(5.5d0, 0.0d0)
  cq(6) = dcmplx(5.5d0, 0.0d0)
  DO i=1,6
  call check_cpx2(ne, 44, i, ent%ca(i), cq(i))
  end do

! 45: fgd_entry (linterp) check
  n = fgd_entry(d, 'linterp', ent)
  call check_ok(ne, 45, d)
  call check_int2(ne, 45, 1, n, GD_LINTERP_ENTRY)
  call check_int2(ne, 45, 2, ent%fragment_index, 0)
  call check_str2(ne, 45, 3, ent%field(1), 'data')
  call check_str2(ne, 45, 4, ent%field(2), './lut')

! 46: fgd_entry (bit) check
  n = fgd_entry(d, 'bit', ent)
  call check_ok(ne, 46, d)
  call check_int2(ne, 46, 1, n, GD_BIT_ENTRY)
  call check_int2(ne, 46, 2, ent%fragment_index, 0)
  call check_int2(ne, 46, 3, ent%bitnum, 3)
  call check_int2(ne, 46, 4, ent%numbits, 4)
  call check_str2(ne, 46, 5, ent%field(1), 'data')

! 47: fgd_entry (Sbit) check
  n = fgd_entry(d, 'sbit', ent)
  call check_ok(ne, 47, d)
  call check_int2(ne, 47, 1, n, GD_SBIT_ENTRY)
  call check_int2(ne, 47, 2, ent%fragment_index, 0)
  call check_int2(ne, 47, 3, ent%numbits, 6)
  call check_int2(ne, 47, 4, ent%bitnum, 5)
  call check_str2(ne, 47, 5, ent%field(1), 'data')

! 48: fgd_entry (multiply) check
  n = fgd_entry(d, 'mult', ent)
  call check_ok(ne, 48, d)
  call check_int2(ne, 48, 1, n, GD_MULTIPLY_ENTRY)
  call check_int2(ne, 48, 2, ent%fragment_index, 0)
  call check_str2(ne, 48, 3, ent%field(1), 'data')
  call check_str2(ne, 48, 4, ent%field(2), 'sbit')

! 49: fgd_entry (phase) check
  n = fgd_entry(d, 'phase', ent)
  call check_ok(ne, 49, d)
  call check_int2(ne, 49, 1, n, GD_PHASE_ENTRY)
  call check_int2(ne, 49, 2, ent%fragment_index, 0)
  call check_int2(ne, 49, 3, ent%shift, 11)
  call check_str2(ne, 49, 4, ent%field(1), 'data')

! 50: fgd_entry (const) check
  n = fgd_entry(d, 'const', ent)
  call check_ok(ne, 50, d)
  call check_int2(ne, 50, 1, n, GD_CONST_ENTRY)
  call check_int2(ne, 50, 2, ent%fragment_index, 0)
  call check_int2(ne, 50, 3, ent%data_type, GD_FLOAT64)

! 52: fgd_fragment_index check
  n = fgd_fragment_index(d, 'const')
  call check_ok(ne, 52, d)
  call check_int(ne, 52, n, 0)

! 53: fgd_add_raw check
  call fgd_add_raw(d, 'new1', GD_FLOAT64, 3, 0)
  call check_ok2(ne, 53, 1, d)

  n = fgd_entry(d, 'new1', ent)
  call check_ok2(ne, 53, 2, d)
  call check_int2(ne, 53, 3, ent%fragment_index, 0)
  call check_int2(ne, 53, 4, ent%spf, 3)
  call check_int2(ne, 53, 5, ent%data_type, GD_FLOAT64)
  call check_int2(ne, 53, 6, n, GD_RAW_ENTRY)

! 54: fgd_add_lincom check
  call fgd_add_lincom(d, 'new2', 2, 'in1', 9.9d0, 8.8d0, &
  'in2', 7.7d0, 6.6d0, '', 0d0, 0d0, 0)
  call check_ok2(ne, 54, 1, d)

  n = fgd_entry(d, 'new2', ent)
  call check_ok2(ne, 54, 2, d)
  call check_int2(ne, 54, 3, n, GD_LINCOM_ENTRY)
  call check_int2(ne, 54, 4, ent%n_fields, 2)
  call check_int2(ne, 54, 5, ent%fragment_index, 0)
  call check_str2(ne, 54, 6, ent%field(1), 'in1')
  call check_str2(ne, 54, 7, ent%field(2), 'in2')
  call check_int2(ne, 54, 8, ent%flags, GD_EN_CALC)

  q = (/ 9.9d0, 8.8d0, 7.7d0, 6.6d0, 5.5d0, 5.5d0 /)
  do i=1,2
  call check_dbl2(ne, 54, i * 2 - 1, ent%m(i), q(i * 2 - 1))
  call check_dbl2(ne, 54, i * 2, ent%b(i), q(i * 2))
  end do

! 55: fgd_add_clincom check
  cq(1) = dcmplx(1.1, 1.2)
  cq(2) = dcmplx(1.3, 1.4)
  cq(3) = dcmplx(1.4, 1.5)
  cq(4) = dcmplx(1.6, 1.7)
  call fgd_add_clincom(d, 'new3', 2, 'in1', cq(1), cq(2), &
  'in2', cq(3), cq(4), '', cq(5), cq(6), 0)
  call check_ok2(ne, 55, 1, d)

  n = fgd_entry(d, 'new3', ent)
  call check_ok2(ne, 55, 2, d)
  call check_int2(ne, 55, 3, ent%n_fields, 2)
  call check_int2(ne, 55, 4, n, GD_LINCOM_ENTRY)
  call check_int2(ne, 55, 5, ent%fragment_index, 0)
  call check_str2(ne, 55, 6, ent%field(1), 'in1')
  call check_str2(ne, 55, 7, ent%field(2), 'in2')
  call check_int2(ne, 55, 8, ent%flags, GD_EN_CALC + GD_EN_COMPSCAL)

  cq(1) = dcmplx(1.1, 1.2)
  cq(2) = dcmplx(1.3, 1.4)
  cq(3) = dcmplx(1.4, 1.5)
  cq(4) = dcmplx(1.6, 1.7)
  do i=1,2
  call check_cpx2(ne, 55, i * 2 + 7, ent%cm(i), cq(i * 2 - 1))
  call check_cpx2(ne, 55, i * 2 + 8, ent%cb(i), cq(i * 2))
  end do

! 56: fgd_add_polynom check
  call fgd_add_polynom(d, 'new4', 3, 'in1', 3d3, 4d4, 5d5, 6d6, 0d0, 0d0, &
  0)
  call check_ok2(ne, 56, 1, d)

  n = fgd_entry(d, 'new4', ent)
  call check_ok2(ne, 56, 2, d)
  call check_int2(ne, 56, 1, n, GD_POLYNOM_ENTRY)
  call check_int2(ne, 56, 2, ent%poly_ord, 3)
  call check_int2(ne, 56, 3, ent%fragment_index, 0)
  call check_str2(ne, 56, 4, ent%field(1), 'in1')
  call check_int2(ne, 56, 5, ent%flags, GD_EN_CALC)

  q = (/ 3d3, 4d4, 5d5, 6d6, 5.5d0, 5.5d0 /)
  DO i=1,4
  call check_dbl2(ne, 56, i, ent%a(i), q(i))
  end do

! 57: fgd_add_cpolynom check
  cq(1) = dcmplx(3.1, 7.0)
  cq(2) = dcmplx(4.2, 8.0)
  cq(3) = dcmplx(5.2, 9.0)
  cq(4) = dcmplx(6.3, 4.4)
  call fgd_add_cpolynom(d, 'new5', 3, 'in1', cq(1), cq(2), cq(3), cq(4), &
  dcmplx(0d0,0d0), dcmplx(0d0,0d0), 0)
  call check_ok2(ne, 57, 1, d)

  n = fgd_entry(d, 'new5', ent)
  call check_ok2(ne, 57, 2, d)
  call check_int2(ne, 57, 1, n, GD_POLYNOM_ENTRY)
  call check_int2(ne, 57, 2, ent%poly_ord, 3)
  call check_int2(ne, 57, 3, ent%fragment_index, 0)
  call check_str2(ne, 57, 4, ent%field(1), 'in1')
  call check_int2(ne, 57, 5, ent%flags, GD_EN_CALC + GD_EN_COMPSCAL)

  cq(1) = dcmplx(3.1, 7.0)
  cq(2) = dcmplx(4.2, 8.0)
  cq(3) = dcmplx(5.2, 9.0)
  cq(4) = dcmplx(6.3, 4.4)
  DO i=1,4
  call check_cpx2(ne, 57, i, ent%ca(i), cq(i))
  end do

! 58: fgd_add_linterp check
  call fgd_add_linterp(d, "new6", "in", "./some/table", 0)
  call check_ok2(ne, 58, 1, d)

  n = fgd_entry(d, 'new6', ent)
  call check_ok2(ne, 58, 2, d)
  call check_int2(ne, 58, 1, n, GD_LINTERP_ENTRY)
  call check_int2(ne, 58, 2, ent%fragment_index, 0)
  call check_str2(ne, 58, 3, ent%field(1), 'in')
  call check_str2(ne, 58, 4, ent%field(2), './some/table')

! 59: fgd_add_bit check
  call fgd_add_bit(d, "new7", "in", 13, 12, 0)
  call check_ok2(ne, 59, 1, d)

  n = fgd_entry(d, 'new7', ent)
  call check_ok2(ne, 59, 2, d)
  call check_int2(ne, 59, 1, n, GD_BIT_ENTRY)
  call check_int2(ne, 59, 2, ent%fragment_index, 0)
  call check_int2(ne, 59, 3, ent%numbits, 12)
  call check_int2(ne, 59, 4, ent%bitnum, 13)
  call check_str2(ne, 59, 5, ent%field(1), 'in')

! 60: fgd_add_sbit check
  call fgd_add_sbit(d, "new8", "in", 13, 12, 0)
  call check_ok2(ne, 60, 1, d)

  n = fgd_entry(d, "new8", ent)
  call check_ok2(ne, 60, 2, d)
  call check_int2(ne, 60, 1, n, GD_SBIT_ENTRY)
  call check_int2(ne, 60, 2, ent%fragment_index, 0)
  call check_int2(ne, 60, 3, ent%numbits, 12)
  call check_int2(ne, 60, 4, ent%bitnum, 13)
  call check_str2(ne, 60, 5, ent%field(1), 'in')

! 61: fgd_add_multiply check
  call fgd_add_multiply(d, 'new9', 'in1', 'in2', 0)
  call check_ok2(ne, 61, 1, d)

  n = fgd_entry(d, 'new9', ent)
  call check_ok2(ne, 61, 2, d)
  call check_int2(ne, 61, 1, n, GD_MULTIPLY_ENTRY)
  call check_int2(ne, 61, 2, ent%fragment_index, 0)
  call check_str2(ne, 61, 3, ent%field(1), 'in1')
  call check_str2(ne, 61, 4, ent%field(2), 'in2')

! 62: fgd_add_phase check
  call fgd_add_phase(d, 'new10', 'in1', 22, 0)
  call check_ok2(ne, 62, 1, d)

  n = fgd_entry(d, 'new10', ent)
  call check_ok2(ne, 62, 2, d)
  call check_int2(ne, 62, 1, n, GD_PHASE_ENTRY)
  call check_int2(ne, 62, 2, ent%fragment_index, 0)
  call check_int2(ne, 62, 3, ent%shift, 22)
  call check_str2(ne, 62, 4, ent%field(1), 'in1')

! 63: fgd_add_const check
  call fgd_add_const(d, 'new11', GD_FLOAT64, 0)
  call check_ok2(ne, 63, 1, d)

  n = fgd_entry(d, 'new11', ent)
  call check_ok2(ne, 63, 2, d)
  call check_int2(ne, 63, 1, n, GD_CONST_ENTRY)
  call check_int2(ne, 63, 2, ent%fragment_index, 0)
  call check_int2(ne, 63, 3, ent%data_type, GD_FLOAT64)

  call fgd_get_constant_r4(d, 'new11', fl)
  call check_ok2(ne, 63, 3, d)
  call check_dbl(ne, 63, 1d0 * fl, 0d0)

! 64: fgd_fragmentname check
  str = fgd_fragmentname(d, 0)
  call check_ok(ne, 64, d)
  call check_eos(ne, 64, str, fildir//DIRSEP//'format')

! 65: fgd_nfragments check
  n = fgd_nfragments(d)
  call check_ok(ne, 65, d)
  call check_int(ne, 65, n, 1)

! 66: fgd_include check
  call fgd_include(d, 'form2', 0, 0)
  call check_ok2(ne, 66, 3, d)

  call fgd_get_constant_i1(d, 'const2', ci1(1))
  call check_ok2(ne, 66, 3, d)
  call check_int2(ne, 1, 66, int(ci1(1)), -19)

! 67: fgd_nfields_by_type check
  n = fgd_nfields_by_type(d, GD_LINCOM_ENTRY)
  call check_ok(ne, 67, d)
  call check_int(ne, 67, n, 3)

! 68: fgd_field_list_by_type check
  fields(1) = 'new2'
  fields(2) = 'new3'
  fields(3) = 'lincom'
  l = flen
  call fgd_field_list_by_type(flist, d, GD_LINCOM_ENTRY, l)
  call check_ok(ne, 68, d)
  call check_int(ne, 68, l, flen)

  do i = 1, n
  call check_str2(ne, 68, i, flist(i), fields(i))
  end do

! 69: fgd_nvectors check
  n = fgd_nvectors(d)
  call check_ok(ne, 69, d)
  call check_int(ne, 69, n, 25)

! 70: fgd_vector_list check
  fields = (/      'bit        ', 'div        ', 'data       ', 'mult       ', &
    'new1       ', 'new2       ', 'new3       ', 'new4       ', 'new5       ', &
    'new6       ', 'new7       ', 'new8       ', 'new9       ', 'sbit       ', &
    'INDEX      ', 'alias      ', 'indir      ', 'mplex      ', 'new10      ', &
    'phase      ', 'recip      ', 'lincom     ', 'window     ', 'linterp    ', &
    'polynom    ', '           ', '           ', '           ', '           ' /)
  l = flen
  call fgd_vector_list(flist, d, l)
  call check_ok(ne, 70, d)
  call check_int(ne, 70, l, flen)
 
  do i=1,n
  call check_str2(ne, 70, i, flist(i), fields(i))
  end do

! 71: fgd_madd_lincom check
  call fgd_madd_lincom(d, 'data', 'mnew1', 2, 'in1', 9.9d0, 8.8d0, &
  'in2', 7.7d0, 6.6d0, '', 0d0, 0d0)
  call check_ok2(ne, 71, 1, d)

  n = fgd_entry(d, 'data/mnew1', ent)
  call check_ok2(ne, 71, 2, d)
  call check_int2(ne, 71, 3, n, GD_LINCOM_ENTRY)
  call check_int2(ne, 71, 4, ent%n_fields, 2)
  call check_int2(ne, 71, 5, ent%fragment_index, 0)
  call check_str2(ne, 71, 6, ent%field(1), 'in1')
  call check_str2(ne, 71, 7, ent%field(2), 'in2')
  call check_int2(ne, 71, 8, ent%flags, GD_EN_CALC)

  q = (/ 9.9d0, 8.8d0, 7.7d0, 6.6d0, 5.5d0, 5.5d0 /)
  DO i=1,2
  call check_dbl2(ne, 71, i * 2 - 1, ent%m(i), q(i *  2 - 1))
  call check_dbl2(ne, 71, i * 2, ent%b(i), q(i * 2))
  end do

! 72: fgd_madd_clincom check
  cq(1) = dcmplx(1.1, 1.2)
  cq(2) = dcmplx(1.3, 1.4)
  cq(3) = dcmplx(1.4, 1.5)
  cq(4) = dcmplx(1.6, 1.7)
  call fgd_madd_clincom(d, 'data', 'mnew2', 2, 'in1', cq(1), cq(2), &
  'in2', cq(3), cq(4), '', cq(5), cq(6))
  call check_ok2(ne, 72, 1, d)

  n = fgd_entry(d, 'data/mnew2', ent)
  call check_ok(ne, 72, d)
  call check_int2(ne, 72, 1, n, GD_LINCOM_ENTRY)
  call check_int2(ne, 72, 2, ent%n_fields, 2)
  call check_int2(ne, 72, 3, ent%fragment_index, 0)
  call check_str2(ne, 72, 4, ent%field(1), 'in1')
  call check_str2(ne, 72, 5, ent%field(2), 'in2')
  call check_int2(ne, 72, 6, ent%flags, GD_EN_CALC + GD_EN_COMPSCAL)

  cq(1) = dcmplx(1.1, 1.2)
  cq(2) = dcmplx(1.3, 1.4)
  cq(3) = dcmplx(1.4, 1.5)
  cq(4) = dcmplx(1.6, 1.7)
  DO i=1,2
  call check_cpx2(ne, 72, i * 2 - 1, ent%cm(i), cq(i * 2 - 1))
  call check_cpx2(ne, 72, i * 2, ent%cb(i), cq(i * 2))
  end do

! 73: fgd_madd_polynom check
  call fgd_madd_polynom(d, 'data', 'mnew3', 3, 'in1', 3d3, 4d4, 5d5, &
     6d6, 0d0, 0d0)
  call check_ok2(ne, 73, 1, d)

  n = fgd_entry(d, 'data/mnew3', ent)
  call check_ok2(ne, 73, 2, d)
  call check_int2(ne, 73, 1, n, GD_POLYNOM_ENTRY)
  call check_int2(ne, 73, 2, ent%poly_ord, 3)
  call check_int2(ne, 73, 3, ent%fragment_index, 0)
  call check_str2(ne, 73, 4, ent%field(1), 'in1')

  q = (/ 3d3, 4d4, 5d5, 6d6, 5.5d0, 5.5d0 /)
  DO i=1,4
  call check_dbl2(ne, 73, i, ent%a(i), q(i))
  end do

! 74: fgd_madd_cpolynom check
  cq(1) = dcmplx(1.1, 0.0)
  cq(2) = dcmplx(2.2, 0.0)
  cq(3) = dcmplx(2.2, 0.0)
  cq(4) = dcmplx(3.3, 4.4)
  call fgd_madd_cpolynom(d, 'data', 'mnew5', 3, 'in1', cq(1), cq(2), &
     cq(3), cq(4), dcmplx(0d0,0d0), dcmplx(0d0,0d0))
  call check_ok2(ne, 74, 1, d)

  n = fgd_entry(d, 'data/mnew5', ent)
  call check_ok2(ne, 74, 2, d)
  call check_int2(ne, 74, 1, n, GD_POLYNOM_ENTRY)
  call check_int2(ne, 74, 2, ent%poly_ord, 3)
  call check_int2(ne, 74, 3, ent%fragment_index, 0)
  call check_str2(ne, 74, 4, ent%field(1), 'in1')

  cq(1) = dcmplx(1.1, 0.0)
  cq(2) = dcmplx(2.2, 0.0)
  cq(3) = dcmplx(2.2, 0.0)
  cq(4) = dcmplx(3.3, 4.4)
  DO i=1,4
  call check_cpx2(ne, 74, i, ent%ca(i), cq(i))
  end do

! 75: fgd_madd_linterp check
  call fgd_madd_linterp(d, "data", "mnew6", "in", "./more/table")
  call check_ok2(ne, 75, 1, d)

  n = fgd_entry(d, 'data/mnew6', ent)
  call check_ok2(ne, 75, 2, d)
  call check_int2(ne, 75, 1, n, GD_LINTERP_ENTRY)
  call check_int2(ne, 75, 2, ent%fragment_index, 0)
  call check_str2(ne, 75, 3, ent%field(1), 'in')
  call check_str2(ne, 75, 4, ent%field(2), './more/table')

! 76: fgd_madd_bit check
  call fgd_madd_bit(d, "data", "mnew7", "in", 13, 12)
  call check_ok2(ne, 76, 1, d)

  n = fgd_entry(d, 'data/mnew7', ent)
  call check_ok2(ne, 76, 2, d)
  call check_int2(ne, 76, 1, n, GD_BIT_ENTRY)
  call check_int2(ne, 76, 2, ent%fragment_index, 0)
  call check_int2(ne, 76, 3, ent%numbits, 12)
  call check_int2(ne, 76, 4, ent%bitnum, 13)
  call check_str2(ne, 76, 5, ent%field(1), 'in')

! 77: fgd_madd_sbit check
  call fgd_madd_sbit(d, "data", "mnew8", "in", 13, 12)
  call check_ok2(ne, 77, 1, d)

  n = fgd_entry(d, 'data/mnew8', ent)
  call check_ok2(ne, 77, 2, d)
  call check_int2(ne, 77, 1, n, GD_SBIT_ENTRY)
  call check_int2(ne, 77, 2, ent%fragment_index, 0)
  call check_int2(ne, 77, 3, ent%numbits, 12)
  call check_int2(ne, 77, 4, ent%bitnum, 13)
  call check_str2(ne, 77, 5, ent%field(1), 'in')

! 78: fgd_madd_multiply check
  call fgd_madd_multiply(d, 'data', 'mnew9', 'in1', 'in2')
  call check_ok2(ne, 78, 1, d)

  n = fgd_entry(d, 'data/mnew9', ent)
  call check_ok2(ne, 78, 2, d)
  call check_int2(ne, 78, 1, n, GD_MULTIPLY_ENTRY)
  call check_int2(ne, 78, 2, ent%fragment_index, 0)
  call check_str2(ne, 78, 3, ent%field(1), 'in1')
  call check_str2(ne, 78, 4, ent%field(2), 'in2')

! 79: fgd_madd_phase check
  call fgd_madd_phase(d, 'data', 'mnew10', 'in1', 22)
  call check_ok2(ne, 79, 1, d)

  n = fgd_entry(d, 'data/mnew10', ent)
  call check_ok2(ne, 79, 2, d)
  call check_int2(ne, 79, 1, n, GD_PHASE_ENTRY)
  call check_int2(ne, 79, 2, ent%fragment_index, 0)
  call check_int2(ne, 79, 3, ent%shift, 22)
  call check_str2(ne, 79, 4, ent%field(1), 'in1')

! 80: fgd_madd_const check
  call fgd_madd_const(d, 'data', 'mnew11', GD_FLOAT64)
  call check_ok2(ne, 80, 1, d)

  n = fgd_entry(d, 'data/mnew11', ent)
  call check_ok2(ne, 80, 2, d)
  call check_int2(ne, 80, 1, n, GD_CONST_ENTRY)
  call check_int2(ne, 80, 2, ent%fragment_index, 0)
  call check_int2(ne, 80, 3, ent%data_type, GD_FLOAT64)

  call fgd_get_constant_r4(d, 'data/mnew11', fl)
  call check_ok2(ne, 80, 3, d)
  call check_dbl(ne, 80, 1d0 * fl, 0d0)

! 81: fgd_get_string check
  n = fgd_get_string(d, 'string', GD_FIELD_LEN, str)
  call check_ok(ne, 81, d)
  call check_int(ne, 81, n, 17)
  call check_str(ne, 81, str, "Zaphod Beeblebrox")

! 82: fgd_add_string check
  call fgd_add_string(d, 'new12', 0)
  call check_ok2(ne, 82, 1, d)

  n = fgd_get_string(d, 'new12', GD_FIELD_LEN, str)
  call check_ok2(ne, 82, 2, d)
  call check_str(ne, 82, str, "")

! 83: fgd_madd_string check
  call fgd_madd_string(d, "data", 'mnew12')
  call check_ok2(ne, 83, 1, d)

  n = fgd_get_string(d, 'data/mnew12', GD_FIELD_LEN, str)
  call check_ok2(ne, 83, 2, d)
  call check_str(ne, 83, str, "")

! 84: fgd_add_spec check
  call fgd_add_spec(d, 'lorem STRING "Lorem ipsum"', 0)
  call check_ok2(ne, 84, 1, d)

  n = fgd_get_string(d, 'lorem', GD_FIELD_LEN, str)
  call check_ok2(ne, 84, 2, d)
  call check_str(ne, 84, str, "Lorem ipsum")

! 85: fgd_madd_spec check
  call fgd_madd_spec(d, 'ipsum STRING "dolor sit amet."', 'lorem')
  call check_ok2(ne, 85, 1, d)

  n = fgd_get_string(d, 'lorem/ipsum', GD_FIELD_LEN, str)
  call check_ok2(ne, 85, 2, d)
  call check_str(ne, 85, str, "dolor sit amet.")

! 86: fgd_put_constant_i1 check
  ci1(1) = 86
  call fgd_put_constant_i1(d, 'const', ci1(1))
  call check_ok2(ne, 86, 1, d)

  call fgd_get_constant_r4(d, 'const', fl)
  call check_ok2(ne, 86, 2, d)
  call check_dbl(ne, 86, 1d0 * fl, 86d0)

! 87: fgd_put_constant_i2 check
  ci2(1) = 87
  call fgd_put_constant_i2(d, 'const', ci2(1))
  call check_ok2(ne, 87, 1, d)

  call fgd_get_constant_r4(d, 'const', fl)
  call check_ok2(ne, 87, 2, d)
  call check_dbl(ne, 87, 1d0 * fl, 87d0)

! 88: fgd_put_constant_i4 check
  ci4(1) = 88
  call fgd_put_constant_i4(d, 'const', ci4(1))
  call check_ok2(ne, 88, 1, d)

  call fgd_get_constant_r4(d, 'const', fl)
  call check_ok2(ne, 88, 2, d)
  call check_dbl(ne, 88, 1d0 * fl, 88d0)

! 89: fgd_put_constant_i8 check
  ci8(1) = 89
  call fgd_put_constant_i8(d, 'const', ci8(1))
  call check_ok2(ne, 89, 1, d)

  call fgd_get_constant_r4(d, 'const', fl)
  call check_ok2(ne, 89, 2, d)
  call check_dbl(ne, 89, 1d0 * fl, 89d0)

! 90: fgd_put_constant_r4 check
  cr4(1) = -8.1
  call fgd_put_constant_r4(d, 'new11', cr4(1))
  call check_ok2(ne, 90, 1, d)

  call fgd_get_constant_r4(d, 'new11', fl)
  call check_ok2(ne, 90, 2, d)
  call check_dbl(ne, 90, 1d0 * fl, 1d0 * (-8.1))

! 91: fgd_put_constant_r8 check
  cr8(1) = 91
  call fgd_put_constant_r8(d, 'const', cr8(1))
  call check_ok2(ne, 91, 1, d)

  call fgd_get_constant_r4(d, 'const', fl)
  call check_ok2(ne, 91, 2, d)
  call check_dbl(ne, 91, 1d0 * fl, 1d0 * 91.)

! 92: fgd_put_constant_c8 check
  cc8(1) = 92
  call fgd_put_constant_c8(d, 'const', cc8(1))
  call check_ok2(ne, 92, 1, d)

  call fgd_get_constant_r4(d, 'const', fl)
  call check_ok2(ne, 92, 2, d)
  call check_dbl(ne, 92, 1d0 * fl, 92d0)

! 93: fgd_put_constant_c16 check
  cc16(1) = 93
  call fgd_put_constant_c16(d, 'const', cc16(1))
  call check_ok2(ne, 93, 1, d)

  call fgd_get_constant_r4(d, 'const', fl)
  call check_ok2(ne, 93, 2, d)
  call check_dbl(ne, 93, 1d0 * fl, 93d0)

! 94: fgd_put_string check
  call fgd_put_string(d, 'string', "Arthur Dent")
  call check_ok2(ne, 94, 1, d)

  n = fgd_get_string(d, 'string', GD_FIELD_LEN, str)
  call check_ok2(ne, 94, 2, d)
  call check_str(ne, 94, str, "Arthur Dent")

! 95: fgd_nmfields_by_type check
  n = fgd_nmfields_by_type(d, "data", GD_LINCOM_ENTRY)
  call check_ok(ne, 95, d)
  call check_int(ne, 95, n, 2)

! 96: fgd_mfield_list_by_type check
  fields(1) = 'mnew1'
  fields(2) = 'mnew2'
  l = flen
  call fgd_mfield_list_by_type(flist, d, "data", GD_LINCOM_ENTRY, l)
  call check_ok2(ne, 96, i, d)
  call check_int2(ne, 96, i, l, flen)

  do i = 1, n
  call check_str2(ne, 96, i, flist(i), fields(i))
  end do

! 97: fgd_nmvectors check
  n = fgd_nmvectors(d, "data")
  call check_ok(ne, 97, d)
  call check_int(ne, 97, n, 10)

! 98: fgd_mvector_list check
  fields = (/    'mlut       ', 'mnew1      ', 'mnew2      ', 'mnew3      ', &
  'mnew5      ', 'mnew6      ', 'mnew7      ', 'mnew8      ', 'mnew9      ', &
  'mnew10     ', '           ', '           ', '           ', '           ', &
  '           ', '           ', '           ', '           ', '           ', &
  '           ', '           ', '           ', '           ', '           ', &
  '           ', '           ', '           ', '           ', '           ' /)
  l = flen
  call fgd_mvector_list(flist, d, "data", l)
  call check_ok2(ne, 98, i, d)
  call check_int2(ne, 98, i, l, flen)

  do i=1,n
  call check_str2(ne, 98, i, flist(i), fields(i))
  end do

! 99: fgd_alter_raw check
  call fgd_alter_raw(d, 'new1', GD_INT32, 4, 0)
  call check_ok2(ne, 99, 1, d)

  n = fgd_entry(d, 'new1', ent)
  call check_ok2(ne, 99, 2, d)
  call check_int2(ne, 99, 3, ent%fragment_index, 0)
  call check_int2(ne, 99, 4, ent%spf, 4)
  call check_int2(ne, 99, 5, ent%data_type, GD_INT32)
  call check_int2(ne, 99, 6, n, GD_RAW_ENTRY)

! 100: fgd_alter_lincom check
  call fgd_alter_lincom(d, 'new2', 3, 'in4', 9.9d-1, 7.8d0, 'in5', &
     1.1d1, 2.2d-2, 'in6', 1.96d0, 0d0)
  call check_ok2(ne, 100, 1, d)

  n = fgd_entry(d, 'new2', ent)
  call check_ok2(ne, 100, 2, d)
  call check_int2(ne, 100, 3, n, GD_LINCOM_ENTRY)
  call check_int2(ne, 100, 4, ent%n_fields, 3)
  call check_int2(ne, 100, 5, ent%fragment_index, 0)
  call check_str2(ne, 100, 6, ent%field(1), 'in4')
  call check_str2(ne, 100, 7, ent%field(2), 'in5')
  call check_str2(ne, 100, 8, ent%field(3), 'in6')
  call check_int2(ne, 100, 5, ent%flags, GD_EN_CALC)

  q = (/ 9.9d-1, 7.8d0, 1.1d1, 2.2d-2, 1.96d0, 0d0 /)
  DO i=1,3
  call check_dbl2(ne, 100, i * 2 - 1, ent%m(i), q(i * 2 - 1))
  call check_dbl2(ne, 100, i * 2, ent%b(i), q(i * 2))
  end do

! 101: fgd_alter_clincom check
  cq(1) = dcmplx(0.1, 0.2)
  cq(2) = dcmplx(0.3, 0.4)
  cq(3) = dcmplx(0.4, 0.5)
  cq(4) = dcmplx(0.6, 0.7)
  call fgd_alter_clincom(d, 'new3', 2, 'in4', cq(1), cq(2), 'in3', &
     cq(3), cq(4), '', cq(5), cq(6))
  call check_ok2(ne, 101, 1, d)

  n = fgd_entry(d, 'new3', ent)
  call check_ok(ne, 101, d)
  call check_int2(ne, 101, 1, n, GD_LINCOM_ENTRY)
  call check_int2(ne, 101, 2, ent%n_fields, 2)
  call check_int2(ne, 101, 3, ent%fragment_index, 0)
  call check_str2(ne, 101, 4, ent%field(1), 'in4')
  call check_str2(ne, 101, 5, ent%field(2), 'in3')
  call check_int2(ne, 101, 6, ent%flags, GD_EN_CALC + GD_EN_COMPSCAL)

  cq(1) = dcmplx(0.1, 0.2)
  cq(2) = dcmplx(0.3, 0.4)
  cq(3) = dcmplx(0.4, 0.5)
  cq(4) = dcmplx(0.6, 0.7)
  DO i=1,2
  call check_cpx2(ne, 101, i * 2 - 1, ent%cm(i), cq(i * 2 - 1))
  call check_cpx2(ne, 101, i * 2, ent%cb(i), cq(i * 2))
  end do

! 102: fgd_alter_polynom check
  call fgd_alter_polynom(d, 'new4', 4, 'in1', 3d0, 4d0, 5d0, 6d0, 7d0, 0d0)
  call check_ok2(ne, 102, 1, d)

  n = fgd_entry(d, 'new4', ent)
  call check_ok2(ne, 102, 2, d)
  call check_int2(ne, 102, 1, n, GD_POLYNOM_ENTRY)
  call check_int2(ne, 102, 2, ent%poly_ord, 4)
  call check_int2(ne, 102, 3, ent%fragment_index, 0)
  call check_str2(ne, 102, 4, ent%field(1), 'in1')

  q = (/ 3d0, 4d0, 5d0, 6d0, 7d0, 0d0 /)
  DO i=1,5
  call check_dbl2(ne, 102, i, ent%a(i), q(i))
  end do

! 103: fgd_alter_cpolynom check
  cq(1) = dcmplx(1.1, 5.0)
  cq(2) = dcmplx(1.2, 4.0)
  cq(3) = dcmplx(1.2, 3.0)
  cq(4) = dcmplx(1.3, 2.4)
  call fgd_alter_cpolynom(d, 'new5', 3, 'in1', cq(1), cq(2), cq(3), &
  cq(4), dcmplx(0d0,0d0), dcmplx(0d0,0d0))
  call check_ok2(ne, 103, 1, d)

  n = fgd_entry(d, 'new5', ent)
  call check_ok2(ne, 103, 2, d)
  call check_int2(ne, 103, 1, n, GD_POLYNOM_ENTRY)
  call check_int2(ne, 103, 2, ent%poly_ord, 3)
  call check_int2(ne, 103, 3, ent%fragment_index, 0)
  call check_str2(ne, 103, 4, ent%field(1), 'in1')
  call check_int2(ne, 103, 5, ent%flags, GD_EN_CALC + GD_EN_COMPSCAL)

  cq(1) = dcmplx(1.1, 5.0)
  cq(2) = dcmplx(1.2, 4.0)
  cq(3) = dcmplx(1.2, 3.0)
  cq(4) = dcmplx(1.3, 2.4)
  do i=1,4
  call check_cpx2(ne, 103, i, ent%ca(i), cq(i))
  end do

! 104: fgd_alter_linterp check
  call fgd_alter_linterp(d, "new6", "in3", "./other/table", 0)
  call check_ok2(ne, 104, 1, d)

  n = fgd_entry(d, 'new6', ent)
  call check_ok2(ne, 104, 2, d)
  call check_int2(ne, 104, 1, n, GD_LINTERP_ENTRY)
  call check_int2(ne, 104, 2, ent%fragment_index, 0)
  call check_str2(ne, 104, 3, ent%field(1), 'in3')
  call check_str2(ne, 104, 4, ent%field(2), './other/table')

! 105: fgd_alter_bit check
  call fgd_alter_bit(d, "new7", "in3", 3, 2)
  call check_ok2(ne, 105, 1, d)

  n = fgd_entry(d, 'new7', ent)
  call check_ok2(ne, 105, 2, d)
  call check_int2(ne, 105, 1, n, GD_BIT_ENTRY)
  call check_int2(ne, 105, 2, ent%fragment_index, 0)
  call check_int2(ne, 105, 3, ent%numbits, 2)
  call check_int2(ne, 105, 4, ent%bitnum, 3)
  call check_str2(ne, 105, 5, ent%field(1), 'in3')

! 106: fgd_alter_sbit check
  call fgd_alter_sbit(d, "new8", "out", 1, 22)
  call check_ok2(ne, 106, 1, d)

  n = fgd_entry(d, 'new8', ent)
  call check_ok2(ne, 106, 2, d)
  call check_int2(ne, 106, 1, n, GD_SBIT_ENTRY)
  call check_int2(ne, 106, 2, ent%fragment_index, 0)
  call check_int2(ne, 106, 3, ent%numbits, 22)
  call check_int2(ne, 106, 4, ent%bitnum, 1)
  call check_str2(ne, 106, 5, ent%field(1), 'out')

! 107: fgd_alter_multiply check
  call fgd_alter_multiply(d, 'new9', 'in6', 'in4')
  call check_ok2(ne, 107, 1, d)

  n = fgd_entry(d, 'new9', ent)
  call check_ok2(ne, 107, 2, d)
  call check_int2(ne, 107, 1, n, GD_MULTIPLY_ENTRY)
  call check_int2(ne, 107, 2, ent%fragment_index, 0)
  call check_str2(ne, 107, 3, ent%field(1), 'in6')
  call check_str2(ne, 107, 4, ent%field(2), 'in4')

! 108: fgd_alter_phase check
  call fgd_alter_phase(d, 'new10', 'in2', 8)
  call check_ok2(ne, 108, 1, d)

  n = fgd_entry(d, 'new10', ent)
  call check_ok2(ne, 108, 2, d)
  call check_int2(ne, 108, 1, n, GD_PHASE_ENTRY)
  call check_int2(ne, 108, 2, ent%fragment_index, 0)
  call check_int2(ne, 108, 3, ent%shift, 8)
  call check_str2(ne, 108, 4, ent%field(1), 'in2')

! 109: fgd_alter_const check
  call fgd_alter_const(d, 'new11', GD_FLOAT32)
  call check_ok2(ne, 109, 1, d)

  n = fgd_entry(d, 'new11', ent)
  call check_ok2(ne, 109, 2, d)
  call check_int2(ne, 109, 1, n, GD_CONST_ENTRY)
  call check_int2(ne, 109, 2, ent%fragment_index, 0)
  call check_int2(ne, 109, 3, ent%data_type, GD_FLOAT32)

  call fgd_get_constant_r4(d, 'new11', fl)
  call check_ok2(ne, 109, 3, d)
  call check_dbl(ne, 109, 1d0 * fl, 1d0 * (-8.1))

! 110: fgd_encoding check
  n = fgd_encoding(d, 0)
  call check_ok(ne, 110, d)
  call check_int(ne, 110, n, GD_UNENCODED)

! 111: fgd_endianness check
  n = fgd_endianness(d, 0)
  call check_ok(ne, 111, d)
  call check_int(ne, 111, n, (GD_LITTLE_ENDIAN + GD_NOT_ARM_ENDIAN))

! 112: fgd_dirfilename check
  l = 4096
  call fgd_dirfilename(path, l, d, 0)
  call check_ok(ne, 112, d)
  call check_int(ne, 112, l, 4096)
  call check_eos(ne, 112, path, fildir)

! 113: fgd_parent_fragment check
  n = fgd_parent_fragment(d, 1)
  call check_ok(ne, 113, d)
  call check_int(ne, 113, n, 0)

! 114: fgd_alter_protection check
  call fgd_alter_protection(d, GD_PROTECT_DATA, 1)
  call check_ok(ne, 114, d)

! 115: fgd_protection check
  n = fgd_protection(d, 1)
  call check_ok(ne, 115, d)
  call check_int(ne, 115, n, GD_PROTECT_DATA)

! 116: fgd_raw_filename check
  str = fgd_raw_filename(d, "data")
  call check_ok(ne, 116, d)
  call check_eos(ne, 116, str, fildir//DIRSEP//'data')

! 117: fgd_reference check
  str = fgd_reference(d, "new1")
  call check_ok(ne, 117, d)
  call check_str(ne, 117, str, 'new1')

! 118: fgd_eof check
  n = fgd_eof(d, 'lincom')
  call check_ok(ne, 118, d)
  call check_int(ne, 118, n, 80)

! 119: fgd_alter_encoding check
  call fgd_alter_encoding(d, GD_SLIM_ENCODED, 1, 0)
  call check_ok2(ne, 119, 1, d)

  n = fgd_encoding(d, 1)
  call check_ok2(ne, 119, 2, d)
  call check_int(ne, 119, n, GD_SLIM_ENCODED)

! 120: fgd_alter_endianness check
  call fgd_alter_endianness(d, GD_BIG_ENDIAN, 1, 0)
  call check_ok2(ne, 120, 1, d)

  n = fgd_endianness(d, 1)
  call check_ok2(ne, 120, 2, d)
  call check_int(ne, 120, n, GD_BIG_ENDIAN)

! 121: fgd_alter_spec check
  call fgd_alter_spec(d, 'new10 PHASE in1 3', 0)
  call check_ok2(ne, 121, 1, d)

  n = fgd_entry(d, 'new10', ent)
  call check_ok2(ne, 121, 2, d)
  call check_int2(ne, 121, 1, n, GD_PHASE_ENTRY)
  call check_int2(ne, 121, 2, ent%fragment_index, 0)
  call check_int2(ne, 121, 3, ent%shift, 3)
  call check_str2(ne, 121, 4, ent%field(1), 'in1')

! 122: fgd_delete check
  call fgd_delete(d, 'new10', 0)
  call check_ok2(ne, 122, 1, d)

  n = fgd_entry(d, 'new10', ent)
  call check_err2(ne, 122, 2, d, GD_E_BAD_CODE)

! 123: fgd_malter_spec check
  call fgd_malter_spec(d, 'mnew10 PHASE in4 11', 'data', 0)
  call check_ok2(ne, 123, 1, d)

  n = fgd_entry(d, 'data/mnew10', ent)
  call check_ok2(ne, 123, 2, d)
  call check_int2(ne, 123, 1, n, GD_PHASE_ENTRY)
  call check_int2(ne, 123, 2, ent%fragment_index, 0)
  call check_int2(ne, 123, 3, ent%shift, 11)
  call check_str2(ne, 123, 4, ent%field(1), 'in4')

! 124: fgd_move check
  call fgd_move(d, 'new9', 1, 0)
  call check_ok2(ne, 124, 1, d)

  n = fgd_entry(d, 'new9', ent)
  call check_ok2(ne, 124, 2, d)
  call check_int2(ne, 124, 1, n, GD_MULTIPLY_ENTRY)
  call check_int2(ne, 124, 2, ent%fragment_index, 1)
  call check_str2(ne, 124, 3, ent%field(1), 'in6')
  call check_str2(ne, 124, 4, ent%field(2), 'in4')

! 125: fgd_rename check
  call fgd_rename(d, 'new9', 'newer', 0)
  call check_ok2(ne, 125, 1, d)

  n = fgd_entry(d, 'new9', ent)
  call check_err2(ne, 125, 2, d, GD_E_BAD_CODE)

  n = fgd_entry(d, 'newer', ent)
  call check_ok2(ne, 125, 3, d)
  call check_int2(ne, 125, 1, n, GD_MULTIPLY_ENTRY)
  call check_int2(ne, 125, 2, ent%fragment_index, 1)
  call check_str2(ne, 125, 3, ent%field(1), 'in6')
  call check_str2(ne, 125, 4, ent%field(2), 'in4')

! 126: fgd_uninclude check
  call fgd_uninclude(d, 1, 0)
  call check_ok2(ne, 126, 1, d)

  n = fgd_entry(d, 'newer', ent)
  call check_err2(ne, 126, 2, d, GD_E_BAD_CODE)

! 127: fgd_frameoffset check
  n = fgd_frameoffset(d, 0)
  call check_ok(ne, 127, d)
  call check_int(ne, 127, n, 0)

! 128: fgd_alter_frameoffset check
  call fgd_alter_frameoffset(d, 33, 0, 0)
  call check_ok2(ne, 128, 1, d)

  n = fgd_frameoffset(d, 0)
  call check_ok2(ne, 128, 2, d)
  call check_int(ne, 128, n, 33)

! 129: fgd_native_type check
  n = fgd_native_type(d, 'data')
  call check_ok(ne, 129, d)
  call check_int(ne, 129, n, GD_INT8)

! 131: fgd_validate check
  n = fgd_validate(d, 'new7')
  call check_err(ne, 131, d, GD_E_BAD_CODE)
  call check_int(ne, 131, n, GD_E_BAD_CODE)

! 132: fgd_framenum check
  str = fgd_reference(d, "data")
  dp = fgd_framenum(d, 'INDEX', 33.3d0)
  call check_ok(ne, 132, d)
  call check_dbl(ne, 132, dp, 33.3d0)

! 133: fgd_framenum_subset check
  dp = fgd_framenum_subset(d, 'data', 33.3d0, 6, 0)
  call check_ok(ne, 133, d)
  call check_dbl(ne, 133, dp, 37.0375d0)

! 135: fgd_add raw
  ent%data_type = GD_FLOAT32
  ent%fragment_index = 0
  ent%spf = 0
  ent%field_type = GD_RAW_ENTRY
  ent%scalar(1) = 'carray'
  ent%scalar_ind(1) = 2
  call fgd_add(d, 'new135', ent)
  call check_ok2(ne, 135, 1, d)

  n = fgd_entry(d, 'new135', ent)
  call check_ok2(ne, 135, 2, d)
  call check_int2(ne, 135, 3, n, GD_RAW_ENTRY)
  call check_int2(ne, 135, 4, ent%fragment_index, 0)
  call check_int2(ne, 135, 5, ent%spf, 2)
  call check_int2(ne, 135, 6, ent%data_type, GD_FLOAT32)

! 136: fgd_madd check
  ent%shift = 33
  ent%field(1) = 'data/mnew4'
  ent%fragment_index = 0
  ent%field_type = GD_PHASE_ENTRY
  call fgd_madd(d, 'data', 'mnew136', ent)
  call check_ok2(ne, 136, 1, d)

  n = fgd_entry(d, 'data/mnew136', ent)
  call check_ok2(ne, 136, 2, d)
  call check_int2(ne, 136, 1, n, GD_PHASE_ENTRY)
  call check_int2(ne, 136, 2, ent%fragment_index, 0)
  call check_int2(ne, 136, 3, ent%shift, 33)
  call check_str2(ne, 136, 4, ent%field(1), 'data/mnew4')

! 141: fgd_alter_entry RAW
  ent%field_type = GD_RAW_ENTRY
  ent%data_type = GD_FLOAT64
  ent%scalar(1) = 'const'
  ent%scalar_ind(1) = -1
  call fgd_alter_entry(d, 'new135', ent, 0, 0)
  call check_ok2(ne, 141, 1, d)

  n = fgd_entry(d, 'new135', ent)
  call check_ok2(ne, 141, 2, d)
  call check_int2(ne, 141, 3, n, GD_RAW_ENTRY)
  call check_int2(ne, 141, 4, ent%fragment_index, 0)
  call check_int2(ne, 141, 5, ent%spf, 93)
  call check_int2(ne, 141, 6, ent%data_type, GD_FLOAT64)
  call check_str2(ne, 141, 7, ent%scalar(1), 'const')
  call check_int2(ne, 141, 8, ent%scalar_ind(1), -1)

! 142: fgd_bof check
  n = fgd_bof(d, 'lincom')
  call check_ok(ne, 142, d)
  call check_int(ne, 142, n, 264)

! 143: fgd_entry (divide) check
  n = fgd_entry(d, 'div', ent)
  call check_ok(ne, 143, d)
  call check_int2(ne, 143, 1, n, GD_DIVIDE_ENTRY)
  call check_int2(ne, 143, 2, ent%fragment_index, 0)
  call check_str2(ne, 143, 3, ent%field(1), 'mult')
  call check_str2(ne, 143, 4, ent%field(2), 'bit')

! 145: fgd_entry (recip) check
  n = fgd_entry(d, 'recip', ent)
  call check_ok(ne, 145, d)
  call check_int2(ne, 145, 1, n, GD_RECIP_ENTRY)
  call check_int2(ne, 145, 2, ent%fragment_index, 0)
  call check_int2(ne, 145, 3, ent%flags, GD_EN_CALC + GD_EN_COMPSCAL)
  call check_str2(ne, 145, 4, ent%field(1), 'div')
  call check_cpx2(ne, 145, 5, ent%cdividend, dcmplx(6.5d0, 4.3d0))

! 146: fgd_add_divide check
  call fgd_add_divide(d, 'new14', 'in1', 'in2', 0)
  call check_ok2(ne, 146, 1, d)

  n = fgd_entry(d, 'new14', ent)
  call check_ok2(ne, 146, 2, d)
  call check_int2(ne, 146, 1, n, GD_DIVIDE_ENTRY)
  call check_int2(ne, 146, 2, ent%fragment_index, 0)
  call check_str2(ne, 146, 3, ent%field(1), 'in1')
  call check_str2(ne, 146, 4, ent%field(2), 'in2')

! 147: fgd_add_recip check
  call fgd_add_recip(d, 'new15', 'in1', 31.9d0, 0)
  call check_ok2(ne, 147, 1, d)

  n = fgd_entry(d, 'new15', ent)
  call check_ok2(ne, 147, 2, d)
  call check_int2(ne, 147, 1, n, GD_RECIP_ENTRY)
  call check_int2(ne, 147, 2, ent%fragment_index, 0)
  call check_str2(ne, 147, 3, ent%field(1), 'in1')
  call check_int2(ne, 147, 4, ent%flags, GD_EN_CALC)
  call check_dbl2(ne, 147, 5, ent%dividend, 31.9d0)

! 148: fgd_add_recip check
  call fgd_add_crecip(d, 'new16', 'in1', dcmplx(31.9d0, 38.2d0), 0)
  call check_ok2(ne, 148, 1, d)

  n = fgd_entry(d, 'new16', ent)
  call check_ok2(ne, 148, 2, d)
  call check_int2(ne, 148, 1, n, GD_RECIP_ENTRY)
  call check_int2(ne, 148, 2, ent%fragment_index, 0)
  call check_str2(ne, 148, 3, ent%field(1), 'in1')
  call check_int2(ne, 148, 4, ent%flags, GD_EN_CALC + GD_EN_COMPSCAL)
  call check_cpx2(ne, 148, 5, ent%cdividend, dcmplx(31.9d0, 38.2d0))

! 149: fgd_madd_divide check
  call fgd_madd_divide(d, 'data', 'new14', 'in3', 'in4')
  call check_ok2(ne, 149, 1, d)

  n = fgd_entry(d, 'data/new14', ent)
  call check_ok2(ne, 149, 2, d)
  call check_int2(ne, 149, 1, n, GD_DIVIDE_ENTRY)
  call check_int2(ne, 149, 2, ent%fragment_index, 0)
  call check_str2(ne, 149, 3, ent%field(1), 'in3')
  call check_str2(ne, 149, 4, ent%field(2), 'in4')

! 150: fgd_madd_recip check
  call fgd_madd_recip(d, 'data', 'new15', 'in0', 95.5d0)
  call check_ok2(ne, 150, 1, d)

  n = fgd_entry(d, 'data/new15', ent)
  call check_ok2(ne, 150, 2, d)
  call check_int2(ne, 150, 1, n, GD_RECIP_ENTRY)
  call check_int2(ne, 150, 2, ent%fragment_index, 0)
  call check_str2(ne, 150, 3, ent%field(1), 'in0')
  call check_int2(ne, 150, 4, ent%flags, GD_EN_CALC)
  call check_dbl2(ne, 150, 5, ent%dividend, 95.5d0)

! 151: fgd_madd_recip check
  call fgd_madd_crecip(d, 'data', 'new16', 'in3', dcmplx(8.47d0, 6.22d0))
  call check_ok2(ne, 151, 1, d)

  n = fgd_entry(d, 'data/new16', ent)
  call check_ok2(ne, 151, 2, d)
  call check_int2(ne, 151, 1, n, GD_RECIP_ENTRY)
  call check_int2(ne, 151, 2, ent%fragment_index, 0)
  call check_str2(ne, 151, 3, ent%field(1), 'in3')
  call check_int2(ne, 151, 4, ent%flags, GD_EN_CALC + GD_EN_COMPSCAL)
  call check_cpx2(ne, 151, 5, ent%cdividend, dcmplx(8.47d0, 6.22d0))

! 152: fgd_alter_divide check
  call fgd_alter_divide(d, 'new14', 'in6', 'in4')
  call check_ok2(ne, 152, 1, d)

  n = fgd_entry(d, 'new14', ent)
  call check_ok2(ne, 152, 2, d)
  call check_int2(ne, 152, 1, n, GD_DIVIDE_ENTRY)
  call check_int2(ne, 152, 2, ent%fragment_index, 0)
  call check_str2(ne, 152, 3, ent%field(1), 'in6')
  call check_str2(ne, 152, 4, ent%field(2), 'in4')

! 153: fgd_alter_recip check
  call fgd_alter_recip(d, 'new15', 'in5', 0.187d0)
  call check_ok2(ne, 153, 1, d)

  n = fgd_entry(d, 'new15', ent)
  call check_ok2(ne, 153, 2, d)
  call check_int2(ne, 153, 1, n, GD_RECIP_ENTRY)
  call check_int2(ne, 153, 2, ent%fragment_index, 0)
  call check_str2(ne, 153, 3, ent%field(1), 'in5')
  call check_dbl2(ne, 153, 4, ent%dividend, 0.187d0)

! 154: fgd_alter_crecip check
  call fgd_alter_crecip(d, 'new16', 'in2', dcmplx(4.3d0, 81.81d0))
  call check_ok2(ne, 154, 1, d)

  n = fgd_entry(d, 'new16', ent)
  call check_ok2(ne, 154, 2, d)
  call check_int2(ne, 154, 1, n, GD_RECIP_ENTRY)
  call check_int2(ne, 154, 2, ent%fragment_index, 0)
  call check_str2(ne, 154, 3, ent%field(1), 'in2')
  call check_cpx2(ne, 154, 4, ent%cdividend, dcmplx(4.3d0, 81.81d0))

! 155: fgd_rewrite_fragment check
  call fgd_rewrite_fragment(d, 0)
  call check_ok(ne, 155, d)

! 156: fgd_invalid_dirfile check
  l = fgd_invalid_dirfile()
  call check_ok2(ne, 98, 1, l)

  n = fgd_nfragments(l)
  call check_err2(ne, 98, 2, l, GD_E_BAD_DIRFILE)

  call fgd_close(l)

! 157: fgd_dirfile_standards
  n = fgd_dirfile_standards(d, GD_VERSION_CURRENT)
  call check_ok2(ne, 157, 1, d)
  call check_int(ne, 157, n, GD_DIRFILE_STANDARDS_VERSION)

  n = fgd_dirfile_standards(d, 0)
  call check_err2(ne, 157, 2, d, GD_E_ARGUMENT)

! 158: gd_get_carray_slice (INT8)
  call fgd_get_carray_i1(d, "carray", 0, 0, ci1)
  call check_ok(ne, 158, d)

  do i=1,6
    call check_int2(ne, 158, i, int(ci1(i)), i)
  end do

! 159: gd_get_carray_slice (INT8)
  call fgd_get_carray_i1(d, "carray", 3, 2, ci1)
  call check_ok(ne, 159, d)

  do i=1,2
    call check_int2(ne, 159, i, int(ci1(i)), i + 2)
  end do

! 160: gd_get_carray_slice (INT16)
  call fgd_get_carray_i2(d, "carray", 3, 2, ci2)
  call check_ok(ne, 160, d)

  do i=1,2
    call check_int2(ne, 160, i, int(ci2(i)), i + 2)
  end do

! 161: gd_get_carray_slice (INT32)
  call fgd_get_carray_i4(d, "carray", 3, 2, ci4)
  call check_ok(ne, 161, d)

  do i=1,2
    call check_int2(ne, 161, i, ci4(i), i + 2)
  end do

! 162: gd_get_carray_slice (INT64)
  call fgd_get_carray_i8(d, "carray", 3, 2, ci8)
  call check_ok(ne, 162, d)

  do i=1,2
    call check_int2(ne, 162, i, int(ci8(i)), i + 2)
  end do

! 163: gd_get_carray_slice (FLOAT32)
  call fgd_get_carray_r4(d, "carray", 3, 2, cr4)
  call check_ok(ne, 163, d)

  do i=1,2
  call check_dbl2(ne, 163, i, 1d0 * cr4(i), (2 + i) * 1.1d0)
  end do

! 164: gd_get_carray_slice (FLOAT64)
  call fgd_get_carray_r8(d, "carray", 3, 2, cr8)
  call check_ok(ne, 164, d)

  do i=1,2
  call check_dbl2(ne, 164, i, cr8(i), (2 + i) * 1.1d0)
  end do

! 165: gd_get_carray_slice (COMPLEX64)
  call fgd_get_carray_c8(d, "carray", 3, 2, cc8)
  call check_ok(ne, 165, d)

  do i=1,2
  call check_cpx2(ne, 165, i, 1d0 * cc8(i), dcmplx((2 + i) * 1.1d0, 0))
  end do

! 166: gd_get_carray_slice (COMPLEX128)
  call fgd_get_carray_c16(d, "carray", 3, 2, cc16)
  call check_ok(ne, 166, d)

  do i=1,2
  call check_cpx2(ne, 166, i, cc16(i), dcmplx((2 + i) * 1.1d0, 0))
  end do

! 168: gd_put_carray
  ci1 = (/ 11_1, 12_1, 13_1, 14_1, 15_1, 16_1, 0_1, 0_1 /)
  call fgd_put_carray_i1(d, "carray", 0, 0, ci1)
  call check_ok2(ne, 168, 1, d)

  call fgd_get_carray_i1(d, "carray", 0, 0, ci1)
  call check_ok2(ne, 168, 2, d)

  do i=1,6
    call check_int2(ne, 168, i, int(ci1(i)), i + 10)
  end do

! 169: gd_put_carray_slice (INT8)
  ci1 = (/ 72_1, 73_1, 0_1, 0_1, 0_1, 0_1, 0_1, 0_1 /)
  call fgd_put_carray_i1(d, "carray", 3, 2, ci1)
  call check_ok2(ne, 169, 1, d)

  call fgd_get_carray_i1(d, "carray", 0, 0, ci1)
  call check_ok2(ne, 169, 2, d)

  do i=1,6
    if (i .eq. 3 .or. i .eq. 4) then
      n = 69 + i
    else
      n = 10 + i
    end if
    call check_int2(ne, 169, i, int(ci1(i)), n)
  end do

! 170: gd_put_carray_slice (INT16)
  ci2 = (/ 173_2, 174_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2 /)
  call fgd_put_carray_i2(d, "carray", 3, 2, ci2)
  call check_ok2(ne, 170, 1, d)

  call fgd_get_carray_i2(d, "carray", 0, 0, ci2)
  call check_ok2(ne, 170, 2, d)

  do i=1,6
    if (i .eq. 3 .or. i .eq. 4) then
      n = 170 + i
    else
      n = 10 + i
    end if
    call check_int2(ne, 170, i, int(ci2(i)), n)
  end do

! 171: gd_put_carray_slice (INT32)
  ci4 = (/ 174, 175, 0, 0, 0, 0, 0, 0 /)
  call fgd_put_carray_i4(d, "carray", 3, 2, ci4)
  call check_ok2(ne, 171, 1, d)

  call fgd_get_carray_i4(d, "carray", 0, 0, ci4)
  call check_ok2(ne, 171, 2, d)

  do i=1,6
    if (i .eq. 3 .or. i .eq. 4) then
      n = 171 + i
    else
      n = 10 + i
    end if
    call check_int2(ne, 171, i, ci4(i), n)
  end do

! 172: gd_put_carray_slice (INT64)
  ci8 = (/ 175, 176, 0, 0, 0, 0, 0, 0 /)
  call fgd_put_carray_i8(d, "carray", 3, 2, ci8)
  call check_ok2(ne, 172, 1, d)

  call fgd_get_carray_i8(d, "carray", 0, 0, ci8)
  call check_ok2(ne, 172, 2, d)

  do i=1,6
    if (i .eq. 3 .or. i .eq. 4) then
      n = 172 + i
    else
      n = 10 + i
    end if
    call check_int2(ne, 172, i, int(ci8(i)), n)
  end do

! 173: gd_put_carray_slice (FLOAT32)
  cr4 = (/ 176., 177., 0., 0., 0., 0., 0., 0. /)
  call fgd_put_carray_r4(d, "carray", 3, 2, cr4)
  call check_ok2(ne, 173, 1, d)

  call fgd_get_carray_r4(d, "carray", 0, 0, cr4)
  call check_ok2(ne, 173, 2, d)

  do i=1,6
    if (i .eq. 3 .or. i .eq. 4) then
      dp = 173. + i
    else
      dp = 10 + i
    end if
    call check_dbl2(ne, 173, i, 1d0 * cr4(i), dp)
  end do

! 174: gd_put_carray_slice (FLOAT64)
  cr8 = (/ 177., 178., 0., 0., 0., 0., 0., 0. /)
  call fgd_put_carray_r8(d, "carray", 3, 2, cr8)
  call check_ok2(ne, 174, 1, d)

  call fgd_get_carray_r8(d, "carray", 0, 0, cr8)
  call check_ok2(ne, 174, 2, d)

  do i=1,6
    if (i .eq. 3 .or. i .eq. 4) then
      dp = 174. + i
    else
      dp = 10 + i
    end if
    call check_dbl2(ne, 174, i, cr8(i), dp)
  end do

! 175: gd_put_carray_slice (COMPLEX64)
  cc8 = (/ 178., 179., 0., 0., 0., 0., 0., 0. /)
  call fgd_put_carray_c8(d, "carray", 3, 2, cc8)
  call check_ok2(ne, 175, 1, d)

  call fgd_get_carray_c8(d, "carray", 0, 0, cc8)
  call check_ok2(ne, 175, 2, d)

  do i=1,6
    if (i .eq. 3 .or. i .eq. 4) then
      dc = dcmplx(175d0 + i, 0d0)
    else
      dc = dcmplx(10d0 + i, 0d0)
    end if
    call check_cpx2(ne, 175, i, 1d0 * cc8(i), dc)
  end do

! 176: gd_put_carray_slice (COMPLEX128)
  cc16 = (/ 179., 180., 0., 0., 0., 0., 0., 0. /)
  call fgd_put_carray_c16(d, "carray", 3, 2, cc16)
  call check_ok2(ne, 176, 1, d)

  call fgd_get_carray_c16(d, "carray", 0, 0, cc16)
  call check_ok2(ne, 176, 2, d)

  do i=1,6
    if (i .eq. 3 .or. i .eq. 4) then
      dc = dcmplx(176d0 + i, 0d0)
    else
      dc = dcmplx(10d0 + i, 0d0)
    end if
    call check_cpx2(ne, 175, i, cc16(i), dc)
  end do

! 177: gd_array_len
  n = fgd_array_len(d, 'carray')
  call check_ok(ne, 177, d)
  call check_int(ne, 177, n, 6)

! 178: gd_entry (CARRAY)
  n = fgd_entry(d, 'carray', ent)
  call check_ok(ne, 178, d)
  call check_int2(ne, 178, 1, n, GD_CARRAY_ENTRY)
  call check_int2(ne, 178, 2, ent%fragment_index, 0)
  call check_int2(ne, 178, 3, ent%array_len, 6)
  call check_int2(ne, 178, 4, ent%data_type, GD_FLOAT64)

! 179: gd_add_carray
  call fgd_add_carray(d, 'new17', GD_FLOAT64, 4, 0)
  call check_ok2(ne, 179, 1, d)

  n = fgd_entry(d, 'new17', ent)
  call check_ok2(ne, 179, 2, d)
  call check_int2(ne, 179, 1, n, GD_CARRAY_ENTRY)
  call check_int2(ne, 179, 2, ent%fragment_index, 0)
  call check_int2(ne, 179, 3, ent%array_len, 4)
  call check_int2(ne, 179, 4, ent%data_type, GD_FLOAT64)

  call fgd_get_carray_r4(d, 'new17', 0, 0, cr4)
  call check_ok2(ne, 179, 3, d)

  do i=1,4
  call check_dbl2(ne, 179, i, 1d0 * cr4(i), 0d0)
  end do

! 180: gd_madd_carray
  call fgd_madd_carray(d, 'data', 'new17', GD_FLOAT64, 4)
  call check_ok2(ne, 180, 1, d)

  n = fgd_entry(d, 'data/new17', ent)
  call check_ok2(ne, 180, 2, d)
  call check_int2(ne, 180, 1, n, GD_CARRAY_ENTRY)
  call check_int2(ne, 180, 2, ent%fragment_index, 0)
  call check_int2(ne, 180, 3, ent%array_len, 4)
  call check_int2(ne, 180, 4, ent%data_type, GD_FLOAT64)

  call fgd_get_carray_r4(d, 'data/new17', 0, 0, cr4)
  call check_ok2(ne, 180, 3, d)

  do i=1,4
  call check_dbl2(ne, 180, i, 1d0 * cr4(i), 0d0)
  end do

! 181: gd_alter_carray
  call fgd_alter_carray(d, 'new17', GD_FLOAT32, 3)
  call check_ok2(ne, 181, 1, d)

  n = fgd_entry(d, 'new17', ent)
  call check_ok2(ne, 181, 2, d)
  call check_int2(ne, 181, 1, n, GD_CARRAY_ENTRY)
  call check_int2(ne, 181, 2, ent%fragment_index, 0)
  call check_int2(ne, 181, 3, ent%data_type, GD_FLOAT32)
  call check_int2(ne, 181, 4, ent%array_len, 3)

! 183: fgd_constants_i1 check
  iq(1) = 93
  iq(2) = -8
  n = fgd_nfields_by_type(d, GD_CONST_ENTRY)
  call fgd_constants_i1(ci1, d)
  call check_ok(ne, 183, d)

  do i = 1, n
  call check_int2(ne, 183, i, int(ci1(i)), int(iq(i)))
  end do

! 184: fgd_constants_i2 check
  iq(1) = 93
  call fgd_constants_i2(ci2, d)
  call check_ok(ne, 184, d)

  do i = 1, n
  call check_int2(ne, 184, i, int(ci2(i)), int(iq(i)))
  end do

! 185: fgd_constants_i4 check
  call fgd_constants_i4(ci4, d)
  call check_ok(ne, 185, d)

  do i = 1, n
  call check_int2(ne, 185, i, ci4(i), int(iq(i)))
  end do

! 186: fgd_constants_i8 check
  call fgd_constants_i8(ci8, d)
  call check_ok(ne, 186, d)

  do i = 1, n
  call check_int2(ne, 186, i, int(ci8(i)), int(iq(i)))
  end do

! 187: fgd_constants_r4 check
  q(1) = 93.
  q(2) = -8.1
  call fgd_constants_r4(cr4, d)
  call check_ok(ne, 187, d)

  do i = 1, n
  call check_dbl2(ne, 187, i, 1d0 * cr4(i), q(i))
  end do

! 188: fgd_constants_r8 check
  call fgd_constants_r8(cr8, d)
  call check_ok(ne, 188, d)

  do i = 1, n
  call check_dbl2(ne, 188, i, cr8(i), q(i))
  end do


! 189: fgd_constants_c8 check
  cq(1) = 93.
  cq(2) = -8.1
  call fgd_constants_c8(cc8, d)
  call check_ok(ne, 189, d)

  do i = 1, n
  call check_cpx2(ne, 189, i, 1d0 * cc8(i), cq(i))
  end do

! 190: fgd_constants_c16 check
  call fgd_constants_c16(cc16, d)
  call check_ok(ne, 190, d)

  do i = 1, n
  call check_cpx2(ne, 190, i, cc16(i), cq(i))
  end do

! 191: fgd_mconstants_i1 check
  iq(1) = 3
  iq(2) = 0
  n = fgd_nmfields_by_type(d, 'data', GD_CONST_ENTRY)
  call fgd_mconstants_i1(ci1, d, 'data')
  call check_ok(ne, 191, d)

  do i = 1, n
  call check_int2(ne, 191, i, int(ci1(i)), int(iq(i)))
  end do

! 192: fgd_mconstants_i2 check
  call fgd_mconstants_i2(ci2, d, 'data')
  call check_ok(ne, 192, d)

  do i = 1, n
  call check_int2(ne, 192, i, int(ci2(i)), int(iq(i)))
  end do

! 193: fgd_mconstants_i4 check
  call fgd_mconstants_i4(ci4, d, 'data')
  call check_ok(ne, 193, d)

  do i = 1, n
  call check_int2(ne, 193, i, ci4(i), int(iq(i)))
  end do

! 194: fgd_mconstants_i8 check
  call fgd_mconstants_i8(ci8, d, 'data')
  call check_ok(ne, 194, d)

  do i = 1, n
  call check_int2(ne, 194, i, int(ci8(i)), int(iq(i)))
  end do

! 195: fgd_mconstants_r4 check
  q(1) = 3.3
  q(2) = 0.
  call fgd_mconstants_r4(cr4, d, 'data')
  call check_ok(ne, 195, d)

  do i = 1, n
  call check_dbl2(ne, 195, i, 1d0 * cr4(i), q(i))
  end do

! 196: fgd_mconstants_r8 check
  call fgd_mconstants_r8(cr8, d, 'data')
  call check_ok(ne, 196, d)

  do i = 1, n
  call check_dbl2(ne, 196, i, cr8(i), q(i))
  end do


! 197: fgd_mconstants_c8 check
  cq(1) = dcmplx(3.3, 4.4)
  cq(2) = 0.
  call fgd_mconstants_c8(cc8, d, 'data')
  call check_ok(ne, 197, d)

  do i = 1, n
  call check_cpx2(ne, 197, i, 1d0 * cc8(i), cq(i))
  end do

! 198: fgd_mconstants_c16 check
  call fgd_mconstants_c16(cc16, d, 'data')
  call check_ok(ne, 198, d)

  cq(1) = dcmplx(3.3d0, 4.4d0)
  do i = 1, n
  call check_cpx2(ne, 198, i, cc16(i), cq(i))
  end do

! 199: fgd_strings check
  strings(1) = "Lorem ipsum"
  strings(2) = ""
  strings(3) = "Arthur Dent"
  n = fgd_nfields_by_type(d, GD_STRING_ENTRY)
  l = slen
  call fgd_strings(st, d, l)
  call check_ok(ne, 199, d)
  call check_int(ne, 199, l, slen)

  do i = 1, n
  call check_str2(ne, 199, i, st(i), strings(i))
  end do

! 200: fgd_strings check
  strings(1) = "This is a string constant."
  strings(2) = ""
  n = fgd_nmfields_by_type(d, 'data', GD_STRING_ENTRY)
  l = slen
  call fgd_mstrings(st, d, 'data', l)
  call check_ok(ne, 200, d)
  call check_int(ne, 200, l, slen)

  do i = 1, n
  call check_str2(ne, 200, i, st(i), strings(i))
  end do

! 201: fgd_string_value_max
  n = fgd_string_value_max(d)
  call check_int(ne, 201, n, 11)

! 202: fgd_mstring_value_max
  n = fgd_mstring_value_max(d, 'data')
  call check_int(ne, 202, n, 26)

! 203: fgd_seek check
  n = fgd_seek(d, 'data', 35, 0, GD_SEEK_SET)
  call check_ok(ne, 203, d)
  call check_int2(ne, 203, 1, n, 280)

  n = fgd_getdata_r8(d, 'data', GD_HERE, 0, 1, 0, cr8)
  call check_int2(ne, 203, 2, n, 8)

  do i=1,8
  call check_dbl2(ne, 203, i, cr8(i), 16d0 + i)
  end do

! 204: fgd_tell check
  n = fgd_tell(d, 'data')
  call check_ok(ne, 204, d)
  call check_int(ne, 204, n, 288)

! 205: fgd_hide check
  call fgd_hide(d, 'data')
  call check_ok(ne, 205, d)

! 206: fgd_hidden check
  n = fgd_hidden(d, 'data')
  call check_ok2(ne, 206, 1, d)
  call check_int2(ne, 206, 1, n, 1)

  n = fgd_hidden(d, 'lincom')
  call check_ok2(ne, 206, 2, d)
  call check_int2(ne, 206, 2, n, 0)

! 207: fgd_unhide check
  call fgd_unhide(d, 'data')
  call check_ok2(ne, 206, 1, d)
  n = fgd_hidden(d, 'data')
  call check_ok2(ne, 206, 2, d)
  call check_int(ne, 206, n, 0)

! 208: fgd_sync check
  call fgd_sync(d, 'data')
  call check_ok(ne, 208, d)

! 209: fgd_flush check
  call fgd_flush(d, 'data')
  call check_ok(ne, 209, d)

! 210: fgd_metaflush check
  call fgd_metaflush(d)
  call check_ok(ne, 210, d)

! 211: fgd_entry (WINDOW) check
  n = fgd_entry(d, 'window', ent)
  call check_ok(ne, 211, d)
  call check_int2(ne, 211, 1, n, GD_WINDOW_ENTRY)
  call check_int2(ne, 211, 2, ent%fragment_index, 0)
  call check_int2(ne, 211, 3, ent%windop, GD_WINDOP_LT)
  call check_str2(ne, 211, 4, ent%field(1), 'linterp')
  call check_str2(ne, 211, 5, ent%field(2), 'mult')
  call check_dbl2(ne, 211, 6, ent%rthreshold, 4.1D0)

! 212: fgd_add_window_i check
  call fgd_add_window_i(d, 'new18', 'in1', 'in2', GD_WINDOP_NE, 32, 0)
  call check_ok2(ne, 212, 1, d)

  n = fgd_entry(d, 'new18', ent)
  call check_ok2(ne, 212, 2, d)
  call check_int2(ne, 212, 1, n, GD_WINDOW_ENTRY)
  call check_int2(ne, 212, 2, ent%fragment_index, 0)
  call check_int2(ne, 212, 3, ent%windop, GD_WINDOP_NE)
  call check_str2(ne, 212, 4, ent%field(1), 'in1')
  call check_str2(ne, 212, 5, ent%field(2), 'in2')
  call check_int2(ne, 212, 6, ent%ithreshold, 32)

! 213: fgd_add_window_r check
  call fgd_add_window_r(d, 'new19', 'in5', 'in4', GD_WINDOP_LE, 1d-4, 0)
  call check_ok2(ne, 213, 1, d)

  n = fgd_entry(d, 'new19', ent)
  call check_ok2(ne, 213, 2, d)
  call check_int2(ne, 213, 1, n, GD_WINDOW_ENTRY)
  call check_int2(ne, 213, 2, ent%fragment_index, 0)
  call check_int2(ne, 213, 3, ent%windop, GD_WINDOP_LE)
  call check_str2(ne, 213, 4, ent%field(1), 'in5')
  call check_str2(ne, 213, 5, ent%field(2), 'in4')
  call check_dbl2(ne, 213, 6, ent%rthreshold, 1d-4)

! 214: fgd_madd_window_i check
  call fgd_madd_window_i(d, 'data', 'mnew18', 'in2', 'in3', GD_WINDOP_SET, &
  128)
  call check_ok2(ne, 214, 1, d)

  n = fgd_entry(d, 'data/mnew18', ent)
  call check_ok2(ne, 214, 2, d)
  call check_int2(ne, 214, 1, n, GD_WINDOW_ENTRY)
  call check_int2(ne, 214, 2, ent%fragment_index, 0)
  call check_int2(ne, 214, 3, ent%windop, GD_WINDOP_SET)
  call check_str2(ne, 214, 4, ent%field(1), 'in2')
  call check_str2(ne, 214, 5, ent%field(2), 'in3')
  call check_int2(ne, 214, 6, ent%ithreshold, 128)

! 215: fgd_madd_window_r check
  call fgd_madd_window_r(d, 'data', 'mnew19', 'in4', 'in1', GD_WINDOP_GT, &
  0d0)
  call check_ok2(ne, 215, 1, d)

  n = fgd_entry(d, 'data/mnew19', ent)
  call check_ok2(ne, 215, 2, d)
  call check_int2(ne, 215, 1, n, GD_WINDOW_ENTRY)
  call check_int2(ne, 215, 2, ent%fragment_index, 0)
  call check_int2(ne, 215, 3, ent%windop, GD_WINDOP_GT)
  call check_str2(ne, 215, 4, ent%field(1), 'in4')
  call check_str2(ne, 215, 5, ent%field(2), 'in1')
  call check_dbl2(ne, 215, 6, ent%rthreshold, 0d0)

! 216: fgd_alter_window_i check
  call fgd_alter_window_i(d, 'new19', 'in3', 'in5', GD_WINDOP_CLR, 32)
  call check_ok2(ne, 216, 1, d)

  n = fgd_entry(d, 'new19', ent)
  call check_ok2(ne, 216, 2, d)
  call check_int2(ne, 216, 1, n, GD_WINDOW_ENTRY)
  call check_int2(ne, 216, 2, ent%fragment_index, 0)
  call check_int2(ne, 216, 3, ent%windop, GD_WINDOP_CLR)
  call check_str2(ne, 216, 4, ent%field(1), 'in3')
  call check_str2(ne, 216, 5, ent%field(2), 'in5')
  call check_int2(ne, 216, 6, ent%ithreshold, 32)

! 217: fgd_alter_window_r check
  call fgd_alter_window_r(d, 'new18', 'in3', 'in4', GD_WINDOP_GE, 32d3)
  call check_ok2(ne, 217, 1, d)

  n = fgd_entry(d, 'new18', ent)
  call check_ok2(ne, 217, 2, d)
  call check_int2(ne, 217, 1, n, GD_WINDOW_ENTRY)
  call check_int2(ne, 217, 2, ent%fragment_index, 0)
  call check_int2(ne, 217, 3, ent%windop, GD_WINDOP_GE)
  call check_str2(ne, 217, 4, ent%field(1), 'in3')
  call check_str2(ne, 217, 5, ent%field(2), 'in4')
  call check_dbl2(ne, 217, 6, ent%rthreshold, 32d3)

! 218: fgd_alias_target check
  str = fgd_alias_target(d, 'alias')
  call check_ok(ne, 218, d)
  call check_str(ne, 218, str, 'data')

! 219: fgd_add_alias check
  call fgd_add_alias(d, 'new20', 'data', 0)
  call check_ok2(ne, 219, 1, d)

  str = fgd_alias_target(d, 'new20')
  call check_ok2(ne, 219, 2, d)
  call check_str(ne, 219, str, 'data')

! 220: fgd_madd_alias check
  call fgd_madd_alias(d, 'data', 'mnew20', 'data')
  call check_ok2(ne, 220, 1, d)

  str = fgd_alias_target(d, 'data/mnew20')
  call check_ok2(ne, 220, 2, d)
  call check_str(ne, 220, str, 'data')

! 221: fgd_naliases check
  n = fgd_naliases(d, 'data')
  call check_ok(ne, 221, d)
  call check_int(ne, 221, n, 4)

! 222: GDALSS check
  fields(1) = 'data'
  fields(2) = 'alias'
  fields(3) = 'new20'
  fields(4) = 'data/mnew20'
  l = flen
  call fgd_aliases(flist, d, 'data', l)
  call check_ok(ne, 222, d)
  call check_int(ne, 222, l, flen)

  do i = 1, n
  call check_str2(ne, 222, i, flist(i), fields(i))
  end do

! 223: fgd_include_affix check
  call fgd_include_affix(d, 'format1', 0, 'A', 'Z', GD_CREAT + GD_EXCL)
  call check_ok(ne, 223, d)

! 226: fgd_fragment_affixes check
  l = flen
  n = flen
  call fgd_fragment_affixes(fields(1), l, fields(2), n, d, 1)
  call check_ok(ne, 226, d)
  call check_int2(ne, 226, 1, l, flen)
  call check_str2(ne, 226, 2, fields(1), 'A')
  call check_int2(ne, 226, 3, n, flen)
  call check_str2(ne, 226, 4, fields(2), 'Z')

! 227: fgd_alter_affixes check
  call fgd_alter_affixes(d, 1, 'B', '')
  call check_ok2(ne, 227, 1, d)

  l = flen
  n = flen
  call fgd_fragment_affixes(fields(1), l, fields(2), n, d, 1)
  call check_ok2(ne, 227, 2, d)
  call check_int2(ne, 226, 3, l, flen)
  call check_str2(ne, 227, 4, fields(1), 'B')
  call check_int2(ne, 226, 5, n, flen)
  call check_str2(ne, 227, 6, fields(2), '')

! 228: fgd_entry (MPLEX) check
  n = fgd_entry(d, 'mplex', ent)
  call check_ok(ne, 228, d)
  call check_int2(ne, 228, 1, n, GD_MPLEX_ENTRY)
  call check_int2(ne, 228, 2, ent%fragment_index, 0)
  call check_int2(ne, 228, 3, ent%count_val, 1)
  call check_str2(ne, 228, 4, ent%field(1), 'data')
  call check_str2(ne, 228, 5, ent%field(2), 'sbit')
  call check_int2(ne, 228, 6, ent%period, 10)

! 229: fgd_add_mplex check
  call fgd_add_mplex(d, 'new21', 'in1', 'in2', 5, 6, 0)
  call check_ok2(ne, 229, 1, d)

  n = fgd_entry(d, 'new21', ent)
  call check_ok2(ne, 229, 2, d)
  call check_int2(ne, 229, 1, n, GD_MPLEX_ENTRY)
  call check_int2(ne, 229, 2, ent%fragment_index, 0)
  call check_int2(ne, 229, 3, ent%count_val, 5)
  call check_str2(ne, 229, 4, ent%field(1), 'in1')
  call check_str2(ne, 229, 5, ent%field(2), 'in2')
  call check_int2(ne, 229, 6, ent%period, 6)

! 230: fgd_madd_mplex check
  call fgd_madd_mplex(d, 'data', 'mnew21', 'in2', 'in3', 0, 12)
  call check_ok2(ne, 230, 1, d)

  n = fgd_entry(d, 'data/mnew21', ent)
  call check_ok2(ne, 230, 2, d)
  call check_int2(ne, 230, 1, n, GD_MPLEX_ENTRY)
  call check_int2(ne, 230, 2, ent%fragment_index, 0)
  call check_int2(ne, 230, 3, ent%count_val, 0)
  call check_str2(ne, 230, 4, ent%field(1), 'in2')
  call check_str2(ne, 230, 5, ent%field(2), 'in3')
  call check_int2(ne, 230, 6, ent%period, 12)

! 231: fgd_alter_mplex check
  call fgd_alter_mplex(d, 'new21', 'in3', 'in4', 7, -1)
  call check_ok2(ne, 231, 1, d)

  n = fgd_entry(d, 'new21', ent)
  call check_ok2(ne, 231, 2, d)
  call check_int2(ne, 231, 1, n, GD_MPLEX_ENTRY)
  call check_int2(ne, 231, 2, ent%fragment_index, 0)
  call check_int2(ne, 231, 3, ent%count_val, 7)
  call check_str2(ne, 231, 4, ent%field(1), 'in3')
  call check_str2(ne, 231, 5, ent%field(2), 'in4')
  call check_int2(ne, 231, 6, ent%period, 6)

! 232: fgd_strtok check
  l = slen
  call fgd_strtok(str, l, d, '"test1 test2" test3\ test4 test5')
  call check_ok2(ne, 232, 1, d)
  call check_int2(ne, 232, 2, l, slen)
  call check_str2(ne, 232, 3, str, 'test1 test2')

  l = slen
  call fgd_strtok(str, l, d, '')
  call check_ok2(ne, 232, 4, d)
  call check_int2(ne, 232, 5, l, slen)
  call check_str2(ne, 232, 6, str, 'test3 test4')

! 233: fgd_raw_close check
  call fgd_raw_close(d, 'data')
  call check_ok(ne, 233, d)

! 234: fgd_desync check
  n = fgd_desync(d, 0)
  call check_ok(ne, 234, d)
  call check_int(ne, 234, n, 0)

! 235: fgd_flags check
  n = fgd_flags(d, GD_PRETTY_PRINT, 0)
  call check_ok(ne, 235, d)
  call check_int(ne, 235, n, GD_PRETTY_PRINT)

! 236: fgd_verbose_prefix check
  call fgd_verbose_prefix(d, "big_test95")
  call check_ok(ne, 236, d)

! 237: fgd_nentries check
  n = fgd_nentries(d, "data", GD_SCALAR_ENTRIES, &
    IOR(GD_ENTRIES_HIDDEN, GD_ENTRIES_NOALIAS))
  call check_ok2(ne, 237, 1, d)
  call check_int2(ne, 237, 1, n, 6)
  n = fgd_nentries(d, "", GD_VECTOR_ENTRIES, &
    IOR(GD_ENTRIES_HIDDEN, GD_ENTRIES_NOALIAS))
  call check_ok2(ne, 237, 2, d)
  call check_int2(ne, 237, 2, n, 29)

! 238: fgd_field_name_max check
  i = fgd_entry_name_max(d, "", GD_VECTOR_ENTRIES, &
    IOR(GD_ENTRIES_HIDDEN, GD_ENTRIES_NOALIAS))
  call check_ok(ne, 238, d)
  call check_int(ne, 238, i, 7)

! 239: fgd_field_list check
  fields = (/      'bit        ', 'div        ', 'data       ', 'mult       ', &
    'new1       ', 'new2       ', 'new3       ', 'new4       ', 'new5       ', &
    'new6       ', 'new7       ', 'new8       ', 'sbit       ', 'INDEX      ', &
    'indir      ', 'mplex      ', 'new14      ', 'new15      ', 'new16      ', &
    'new18      ', 'new19      ', 'new21      ', 'phase      ', 'recip      ', &
    'lincom     ', 'new135     ', 'window     ', 'linterp    ', 'polynom    ' /)
  l = flen
  call fgd_entry_list(flist, d, "", GD_VECTOR_ENTRIES, &
    IOR(GD_ENTRIES_HIDDEN, GD_ENTRIES_NOALIAS), l)
  call check_ok(ne, 239, d)
  call check_int(ne, 239, l, flen)

  do i = 1, n
  call check_str2(ne, 239, i, flist(i), fields(i))
  end do

! 240: fgd_mplex_lookback check
  call fgd_mplex_lookback(d, GD_LOOKBACK_ALL)
  call check_ok(ne, 240, d)

! 241: fgd_raw_filename check
  str = fgd_linterp_tablename(d, "linterp")
  call check_ok(ne, 241, d)
  call check_eos(ne, 241, str, fildir//DIRSEP//'lut')

! 243: fgd_add lincom
  ent%field_type = GD_LINCOM_ENTRY
  ent%fragment_index = 0
  ent%n_fields = 3
  ent%flags = 0
  ent%field(1) = 'in1'
  ent%field(2) = 'in2'
  ent%field(3) = 'in3'
  ent%m(1) = 1.1d0
  ent%m(3) = 1.4d0
  ent%scalar(1) = ''
  ent%scalar(2) = 'const'
  ent%scalar_ind(2) = -1
  ent%scalar(3) = ''
  ent%scalar(4) = 'carray'
  ent%scalar_ind(4) = 3
  ent%scalar(5) = 'carray'
  ent%scalar_ind(5) = 4
  ent%scalar(6) = 'carray'
  ent%scalar_ind(6) = 5
  call fgd_add(d, 'new243', ent)
  call check_ok2(ne, 243, 1, d)
  
  n = fgd_entry(d, 'new243', ent)
  call check_ok2(ne, 243, 2, d)
  call check_int2(ne, 243,  3, n, GD_LINCOM_ENTRY)
  call check_int2(ne, 243,  4, ent%n_fields, 3)
  call check_int2(ne, 243,  5, ent%fragment_index, 0)
  call check_str2(ne, 243,  6, ent%field(1), 'in1')
  call check_str2(ne, 243,  7, ent%field(2), 'in2')
  call check_str2(ne, 243,  8, ent%field(3), 'in3')
  call check_int2(ne, 243,  9, ent%flags, GD_EN_CALC)
  call check_str2(ne, 243, 10, ent%scalar(1), '')
  call check_str2(ne, 243, 11, ent%scalar(2), 'const')
  call check_str2(ne, 243, 12, ent%scalar(3), '')
  call check_str2(ne, 243, 13, ent%scalar(4), 'carray')
  call check_str2(ne, 243, 14, ent%scalar(5), 'carray')
  call check_str2(ne, 243, 15, ent%scalar(6), 'carray')
  call check_int2(ne, 243, 16, ent%scalar_ind(2), -1)
  call check_int2(ne, 243, 17, ent%scalar_ind(4), 3)
  call check_int2(ne, 243, 18, ent%scalar_ind(5), 4)
  call check_int2(ne, 243, 19, ent%scalar_ind(6), 5)
  call check_dbl2(ne, 243, 20, ent%m(1), 1.1d0)
  call check_dbl2(ne, 243, 21, ent%m(2), 93d0)
  call check_dbl2(ne, 243, 22, ent%m(3), 1.4d0)
  call check_dbl2(ne, 243, 23, ent%b(1), 179d0)
  call check_dbl2(ne, 243, 24, ent%b(2), 180d0)
  call check_dbl2(ne, 243, 25, ent%b(3), 15d0)

! 244: fgd_add polynom
  ent%field_type = GD_POLYNOM_ENTRY
  ent%fragment_index = 0
  ent%field(1) = 'in1'
  ent%flags = 0
  ent%a(1) = 33d0
  ent%a(2) = 44d0
  ent%a(3) = 66d0
  ent%poly_ord = 3
  ent%scalar(1) = ''
  ent%scalar(2) = ''
  ent%scalar(3) = ''
  ent%scalar(4) = 'carray'
  ent%scalar_ind(4) = 1
  call fgd_add(d, 'new244', ent)
  call check_ok2(ne, 244, 1, d)
  
  n = fgd_entry(d, 'new244', ent)
  call check_ok2(ne,  244,  2, d)
  call check_int2(ne, 244,  3, n, GD_POLYNOM_ENTRY)
  call check_int2(ne, 244,  4, ent%poly_ord, 3)
  call check_int2(ne, 244,  5, ent%fragment_index, 0)
  call check_str2(ne, 244,  6, ent%field(1), 'in1')
  call check_int2(ne, 244,  7, ent%flags, GD_EN_CALC)
  call check_dbl2(ne, 244,  8, ent%a(1), 33d0)
  call check_dbl2(ne, 244,  9, ent%a(2), 44d0)
  call check_dbl2(ne, 244, 10, ent%a(3), 66d0)
  call check_dbl2(ne, 244, 11, ent%a(4), 11d0)
  call check_str2(ne, 244, 12, ent%scalar(1), '')
  call check_str2(ne, 244, 13, ent%scalar(2), '')
  call check_str2(ne, 244, 14, ent%scalar(3), '')
  call check_str2(ne, 244, 15, ent%scalar(4), 'carray')
  call check_int2(ne, 244, 16, ent%scalar_ind(4), 1)

! 246: fgd_add bit
  ent%field_type = GD_BIT_ENTRY
  ent%fragment_index = 0
  ent%field(1) = 'in1'
  ent%bitnum = 11
  ent%scalar(1) = ''
  ent%scalar(2) = 'const'
  ent%scalar_ind(2) = 0
  call fgd_add(d, 'new246', ent)
  call check_ok2(ne, 246, 1, d)

  n = fgd_entry(d, 'new246', ent)
  call check_ok2(ne, 246, 2, d)
  call check_int2(ne, 246,  3, n, GD_BIT_ENTRY)
  call check_int2(ne, 246,  4, ent%fragment_index, 0)
  call check_int2(ne, 246,  5, ent%numbits, 93)
  call check_int2(ne, 246,  6, ent%bitnum, 11)
  call check_str2(ne, 246,  7, ent%field(1), 'in1')
  call check_str2(ne, 246,  8, ent%scalar(1), '')
  call check_str2(ne, 246,  9, ent%scalar(2), 'const')
  call check_int2(ne, 246, 10, ent%scalar_ind(2), -1)

! 248: fgd_add phase
  ent%field(1) = 'new9'
  ent%fragment_index = 0
  ent%field_type = GD_PHASE_ENTRY
  ent%scalar(1) = 'carray'
  ent%scalar_ind(1) = 2
  call fgd_add(d, 'new248', ent)
  call check_ok2(ne, 248, 1, d)

  n = fgd_entry(d, 'new248', ent)
  call check_ok2(ne, 248, 2, d)
  call check_int2(ne, 248, 1, n, GD_PHASE_ENTRY)
  call check_int2(ne, 248, 2, ent%fragment_index, 0)
  call check_int2(ne, 248, 3, ent%shift, 12)
  call check_str2(ne, 248, 4, ent%field(1), 'new9')
  call check_str2(ne, 248, 5, ent%scalar(1), 'carray')
  call check_int2(ne, 248, 6, ent%scalar_ind(1), 2)

! 251: fgd_add recip
  ent%field(1) = 'in1'
  ent%field_type = GD_RECIP_ENTRY
  ent%fragment_index = 0
  ent%scalar(1) = 'carray'
  ent%flags = 0
  ent%scalar_ind = 4
  call fgd_add(d, 'new251', ent)
  call check_ok2(ne, 251, 1, d)

  n = fgd_entry(d, 'new251', ent)
  call check_ok2(ne, 251, 2, d)
  call check_int2(ne, 251, 3, n, GD_RECIP_ENTRY)
  call check_int2(ne, 251, 4, ent%fragment_index, 0)
  call check_int2(ne, 251, 5, ent%flags, GD_EN_CALC)
  call check_str2(ne, 251, 6, ent%field(1), 'in1')
  call check_dbl2(ne, 251, 7, ent%dividend, 180d0)
  call check_str2(ne, 251, 8, ent%scalar(1), 'carray')
  call check_int2(ne, 251, 9, ent%scalar_ind(1), 4)

! 253: fgd_add window
  ent%field(1) = 'in2'
  ent%field(2) = 'in3'
  ent%field_type = GD_WINDOW_ENTRY
  ent%windop = GD_WINDOP_NE
  ent%fragment_index = 0
  ent%scalar(1) = 'const'
  ent%scalar_ind(1) = -1
  call fgd_add(d, 'new253', ent)
  call check_ok2(ne, 253, 1, d)

  n = fgd_entry(d, 'new253', ent)
  call check_ok2(ne, 253, 2, d)
  call check_int2(ne, 253, 1, n, GD_WINDOW_ENTRY)
  call check_int2(ne, 253, 2, ent%fragment_index, 0)
  call check_int2(ne, 253, 3, ent%windop, GD_WINDOP_NE)
  call check_str2(ne, 253, 4, ent%field(1), 'in2')
  call check_str2(ne, 253, 5, ent%field(2), 'in3')
  call check_int2(ne, 253, 6, ent%ithreshold, 93)
  call check_str2(ne, 253, 7, ent%scalar(1), 'const')
  call check_int2(ne, 253, 8, ent%scalar_ind(1), -1)

! 254: fgd_add mplex
  ent%field(1) = 'in1'
  ent%field(2) = 'in2'
  ent%field_type = GD_MPLEX_ENTRY
  ent%scalar(1) = 'carray'
  ent%scalar(2) = 'carray'
  ent%scalar_ind(1) = 3
  ent%scalar_ind(2) = 4
  call fgd_add(d, 'new254', ent)
  call check_ok2(ne, 254, 1, d)

  n = fgd_entry(d, 'new254', ent)
  call check_ok2(ne,  254,  1, d)
  call check_int2(ne, 254,  1, n, GD_MPLEX_ENTRY)
  call check_int2(ne, 254,  2, ent%fragment_index, 0)
  call check_int2(ne, 254,  3, ent%count_val, 179)
  call check_int2(ne, 254,  4, ent%period, 180)
  call check_str2(ne, 254,  5, ent%field(1), 'in1')
  call check_str2(ne, 254,  6, ent%field(2), 'in2')
  call check_str2(ne, 254,  7, ent%scalar(1), 'carray')
  call check_int2(ne, 254,  8, ent%scalar_ind(1), 3)
  call check_str2(ne, 254,  9, ent%scalar(2), 'carray')
  call check_int2(ne, 254, 10, ent%scalar_ind(2), 4)

! 255: fgd_add complex lincom
  ent%field_type = GD_LINCOM_ENTRY
  ent%fragment_index = 0
  ent%n_fields = 3
  ent%flags = GD_EN_COMPSCAL
  ent%field(1) = 'in1'
  ent%field(2) = 'in2'
  ent%field(3) = 'in3'
  ent%cm(1) = dcmplx(1.1d0, 1.2d0)
  ent%cm(3) = dcmplx(1.3d0, 1.4d0)
  ent%scalar(1) = ''
  ent%scalar(2) = 'const'
  ent%scalar_ind(2) = -1
  ent%scalar(3) = ''
  ent%scalar(4) = 'carray'
  ent%scalar_ind(4) = 3
  ent%scalar(5) = 'carray'
  ent%scalar_ind(5) = 4
  ent%scalar(6) = 'carray'
  ent%scalar_ind(6) = 5
  call fgd_add(d, 'new255', ent)
  call check_ok2(ne, 255, 1, d)
  
  n = fgd_entry(d, 'new255', ent)
  call check_ok2(ne, 255, 2, d)
  call check_int2(ne, 255,  3, n, GD_LINCOM_ENTRY)
  call check_int2(ne, 255,  4, ent%n_fields, 3)
  call check_int2(ne, 255,  5, ent%fragment_index, 0)
  call check_str2(ne, 255,  6, ent%field(1), 'in1')
  call check_str2(ne, 255,  7, ent%field(2), 'in2')
  call check_str2(ne, 255,  8, ent%field(3), 'in3')
  call check_int2(ne, 255,  9, ent%flags, GD_EN_CALC + GD_EN_COMPSCAL)
  call check_str2(ne, 255, 10, ent%scalar(1), '')
  call check_str2(ne, 255, 11, ent%scalar(2), 'const')
  call check_str2(ne, 255, 12, ent%scalar(3), '')
  call check_str2(ne, 255, 13, ent%scalar(4), 'carray')
  call check_str2(ne, 255, 14, ent%scalar(5), 'carray')
  call check_str2(ne, 255, 15, ent%scalar(6), 'carray')
  call check_int2(ne, 255, 16, ent%scalar_ind(2), -1)
  call check_int2(ne, 255, 17, ent%scalar_ind(4), 3)
  call check_int2(ne, 255, 18, ent%scalar_ind(5), 4)
  call check_int2(ne, 255, 19, ent%scalar_ind(6), 5)
  call check_cpx2(ne, 255, 20, ent%cm(1), dcmplx(1.1d0, 1.2d0))
  call check_cpx2(ne, 255, 21, ent%cm(2), dcmplx(93d0, 0))
  call check_cpx2(ne, 255, 22, ent%cm(3), dcmplx(1.3d0, 1.4d0))
  call check_cpx2(ne, 255, 23, ent%cb(1), dcmplx(179d0, 0))
  call check_cpx2(ne, 255, 24, ent%cb(2), dcmplx(180d0, 0))

! 256: fgd_add polynom
  ent%field_type = GD_POLYNOM_ENTRY
  ent%fragment_index = 0
  ent%field(1) = 'in1'
  ent%flags = GD_EN_COMPSCAL
  ent%ca(1) = dcmplx(22d0, 33d0)
  ent%ca(2) = dcmplx(44d0, 55d0)
  ent%ca(3) = dcmplx(66d0, 77d0)
  ent%poly_ord = 3
  ent%scalar(1) = ''
  ent%scalar(2) = ''
  ent%scalar(3) = ''
  ent%scalar(4) = 'carray'
  ent%scalar_ind(4) = 1
  call fgd_add(d, 'new256', ent)
  call check_ok2(ne, 256, 1, d)
  
  n = fgd_entry(d, 'new256', ent)
  call check_ok2(ne,  256,  2, d)
  call check_int2(ne, 256,  3, n, GD_POLYNOM_ENTRY)
  call check_int2(ne, 256,  4, ent%poly_ord, 3)
  call check_int2(ne, 256,  5, ent%fragment_index, 0)
  call check_str2(ne, 256,  6, ent%field(1), 'in1')
  call check_int2(ne, 256,  7, ent%flags, GD_EN_CALC + GD_EN_COMPSCAL)
  call check_cpx2(ne, 256,  8, ent%ca(1), dcmplx(22d0, 33d0))
  call check_cpx2(ne, 256,  9, ent%ca(2), dcmplx(44d0, 55d0))
  call check_cpx2(ne, 256, 10, ent%ca(3), dcmplx(66d0, 77d0))
  call check_cpx2(ne, 256, 11, ent%ca(4), dcmplx(11d0, 0))
  call check_str2(ne, 256, 12, ent%scalar(1), '')
  call check_str2(ne, 256, 13, ent%scalar(2), '')
  call check_str2(ne, 256, 14, ent%scalar(3), '')
  call check_str2(ne, 256, 15, ent%scalar(4), 'carray')
  call check_int2(ne, 256, 16, ent%scalar_ind(4), 1)

! 257: fgd_add recip
  ent%field(1) = 'in1'
  ent%field_type = GD_RECIP_ENTRY
  ent%fragment_index = 0
  ent%scalar(1) = 'carray'
  ent%flags = GD_EN_COMPSCAL
  ent%scalar_ind = 4
  call fgd_add(d, 'new257', ent)
  call check_ok2(ne, 257, 1, d)

  n = fgd_entry(d, 'new257', ent)
  call check_ok2(ne, 257, 2, d)
  call check_int2(ne, 257, 3, n, GD_RECIP_ENTRY)
  call check_int2(ne, 257, 4, ent%fragment_index, 0)
  call check_int2(ne, 257, 5, ent%flags, GD_EN_CALC)
  call check_str2(ne, 257, 6, ent%field(1), 'in1')
  call check_dbl2(ne, 257, 7, ent%dividend, 180d0)
  call check_str2(ne, 257, 8, ent%scalar(1), 'carray')

! 258: fgd_add sbit
  ent%field_type = GD_SBIT_ENTRY
  ent%fragment_index = 0
  ent%field(1) = 'in1'
  ent%bitnum = 11
  ent%scalar(1) = ''
  ent%scalar(2) = 'const'
  ent%scalar_ind(2) = 0
  call fgd_add(d, 'new258', ent)
  call check_ok2(ne, 258, 1, d)

  n = fgd_entry(d, 'new258', ent)
  call check_ok2(ne, 258, 2, d)
  call check_int2(ne, 258,  3, n, GD_SBIT_ENTRY)
  call check_int2(ne, 258,  4, ent%fragment_index, 0)
  call check_int2(ne, 258,  5, ent%numbits, 93)
  call check_int2(ne, 258,  6, ent%bitnum, 11)
  call check_str2(ne, 258,  7, ent%field(1), 'in1')
  call check_str2(ne, 258,  8, ent%scalar(1), '')
  call check_str2(ne, 258,  9, ent%scalar(2), 'const')

! 259: fgd_alter_entry lincom
  ent%field_type = GD_LINCOM_ENTRY
  ent%field(2) = 'in4'
  ent%flags = 0
  ent%m(1) = 2.2d0
  ent%scalar(2) = ''
  ent%scalar(3) = 'const'
  ent%scalar(4) = 'carray'
  ent%scalar(6) = 'const'
  ent%scalar_ind(3) = -1
  ent%scalar_ind(4) =  4
  call fgd_alter_entry(d, 'new243', ent, 17, 0)
  call check_ok2(ne, 259, 1, d)
  
  n = fgd_entry(d, 'new243', ent)
  call check_ok2(ne, 259, 2, d)
  call check_int2(ne, 259,  3, n, GD_LINCOM_ENTRY)
  call check_int2(ne, 259,  4, ent%n_fields, 3)
  call check_int2(ne, 259,  5, ent%fragment_index, 0)
  call check_str2(ne, 259,  6, ent%field(1), 'in1')
  call check_str2(ne, 259,  7, ent%field(2), 'in4')
  call check_str2(ne, 259,  8, ent%field(3), 'in3')
  call check_int2(ne, 259,  9, ent%flags, GD_EN_CALC)
  call check_str2(ne, 259, 10, ent%scalar(1), '')
  call check_str2(ne, 259, 11, ent%scalar(2), 'const')
  call check_str2(ne, 259, 12, ent%scalar(3), 'const')
  call check_str2(ne, 259, 13, ent%scalar(4), 'carray')
  call check_str2(ne, 259, 14, ent%scalar(5), '')
  call check_str2(ne, 259, 15, ent%scalar(6), 'const')
  call check_int2(ne, 259, 16, ent%scalar_ind(2), -1)
  call check_int2(ne, 259, 16, ent%scalar_ind(3), -1)
  call check_int2(ne, 259, 17, ent%scalar_ind(4), 4)
  call check_int2(ne, 259, 19, ent%scalar_ind(6), -1)
  call check_dbl2(ne, 259, 20, ent%m(1), 2.2d0)
  call check_dbl2(ne, 259, 21, ent%m(2), 93d0)
  call check_dbl2(ne, 259, 22, ent%m(3), 93d0)
  call check_dbl2(ne, 259, 23, ent%b(1), 180d0)
  call check_dbl2(ne, 259, 24, ent%b(2), 180d0)
  call check_dbl2(ne, 259, 25, ent%b(3), 93d0)

! 260: fgd_alter_entry CLINCOM
  ent%field(1) = 'in1'
  ent%field(2) = 'in4'
  ent%field(3) = 'in3'
  ent%field_type = GD_LINCOM_ENTRY
  ent%flags = GD_EN_COMPSCAL
  ent%cm(1) = dcmplx(9d0, 8d0)
  ent%scalar(2) = ''
  ent%scalar(3) = ''
  ent%scalar(4) = 'carray'
  ent%scalar(6) = ''
  ent%scalar_ind(4) =  3
  call fgd_alter_entry(d, 'new243', ent, 17, 0)
  call check_ok2(ne, 260, 1, d)
  
  n = fgd_entry(d, 'new243', ent)
  call check_ok2(ne, 260, 2, d)
  call check_int2(ne, 260,  3, n, GD_LINCOM_ENTRY)
  call check_int2(ne, 260,  4, ent%n_fields, 3)
  call check_int2(ne, 260,  5, ent%fragment_index, 0)
  call check_str2(ne, 260,  6, ent%field(1), 'in1')
  call check_str2(ne, 260,  7, ent%field(2), 'in4')
  call check_str2(ne, 260,  8, ent%field(3), 'in3')
  call check_int2(ne, 260,  9, ent%flags, GD_EN_CALC + GD_EN_COMPSCAL)
  call check_str2(ne, 260, 10, ent%scalar(1), '')
  call check_str2(ne, 260, 11, ent%scalar(2), 'const')
  call check_str2(ne, 260, 12, ent%scalar(3), 'const')
  call check_str2(ne, 260, 13, ent%scalar(4), 'carray')
  call check_str2(ne, 260, 14, ent%scalar(5), '')
  call check_str2(ne, 260, 15, ent%scalar(6), 'const')
  call check_int2(ne, 260, 16, ent%scalar_ind(2), -1)
  call check_int2(ne, 260, 16, ent%scalar_ind(3), -1)
  call check_int2(ne, 260, 17, ent%scalar_ind(4), 3)
  call check_int2(ne, 260, 19, ent%scalar_ind(6), -1)
  call check_cpx2(ne, 260, 20, ent%cm(1), dcmplx(9d0, 8d0))
  call check_cpx2(ne, 260, 21, ent%cm(2), dcmplx(93d0, 0))
  call check_cpx2(ne, 260, 22, ent%cm(3), dcmplx(93d0, 0))
  call check_cpx2(ne, 260, 23, ent%cb(1), dcmplx(179d0, 0))
  call check_cpx2(ne, 260, 24, ent%cb(2), dcmplx(180d0, 0))
  call check_cpx2(ne, 260, 25, ent%cb(3), dcmplx(93d0, 0))

! 261: fgd_alter_entry POLYNOM
  ent%field(1) = 'in3'
  ent%field_type = GD_POLYNOM_ENTRY
  ent%flags = 0
  ent%a(1) = 2d0
  ent%a(2) = 6d0
  ent%scalar(1) = ''
  ent%scalar(2) = ''
  ent%scalar(3) = 'carray'
  ent%scalar_ind(3) = 5
  call fgd_alter_entry(d, 'new244', ent, 8, 0)
  call check_ok2(ne, 261, 1, d)

  n = fgd_entry(d, 'new244', ent)
  call check_ok2(ne,  261,  2, d)
  call check_int2(ne, 261,  3, n, GD_POLYNOM_ENTRY)
  call check_int2(ne, 261,  4, ent%poly_ord, 3)
  call check_int2(ne, 261,  5, ent%fragment_index, 0)
  call check_str2(ne, 261,  6, ent%field(1), 'in3')
  call check_int2(ne, 261,  7, ent%flags, GD_EN_CALC)
  call check_dbl2(ne, 261,  8, ent%a(1), 2d0)
  call check_dbl2(ne, 261,  9, ent%a(2), 6d0)
  call check_dbl2(ne, 261, 10, ent%a(3), 15d0)
  call check_dbl2(ne, 261, 11, ent%a(4), 11d0)
  call check_str2(ne, 261, 12, ent%scalar(1), '')
  call check_str2(ne, 261, 13, ent%scalar(2), '')
  call check_str2(ne, 261, 14, ent%scalar(3), 'carray')
  call check_str2(ne, 261, 15, ent%scalar(4), '')
  call check_int2(ne, 261, 16, ent%scalar_ind(3), 5)

! 262: fgd_alter_entry CPOLYNOM
  ent%field_type = GD_POLYNOM_ENTRY
  ent%flags = GD_EN_COMPSCAL
  ent%ca(3) = dcmplx(26d0, 2d0)
  ent%scalar(1) = 'const'
  ent%scalar(2) = 'const'
  ent%scalar_ind(1) = -1
  ent%scalar_ind(2) = -1
  call fgd_alter_entry(d, 'new244', ent, 4, 0)
  call check_ok2(ne, 262, 1, d)

  n = fgd_entry(d, 'new244', ent)
  call check_ok2(ne,  262,  2, d)
  call check_int2(ne, 262,  3, n, GD_POLYNOM_ENTRY)
  call check_int2(ne, 262,  4, ent%poly_ord, 3)
  call check_int2(ne, 262,  5, ent%fragment_index, 0)
  call check_str2(ne, 262,  6, ent%field(1), 'in3')
  call check_int2(ne, 262,  7, ent%flags, GD_EN_CALC + GD_EN_COMPSCAL)
  call check_cpx2(ne, 262,  8, ent%ca(1), dcmplx(93d0, 0))
  call check_cpx2(ne, 262,  9, ent%ca(2), dcmplx(93d0, 0))
  call check_cpx2(ne, 262, 10, ent%ca(3), dcmplx(26d0, 2d0))
  call check_cpx2(ne, 262, 11, ent%ca(4), dcmplx(11d0, 0))
  call check_str2(ne, 262, 12, ent%scalar(1), 'const')
  call check_str2(ne, 262, 13, ent%scalar(2), 'const')
  call check_str2(ne, 262, 14, ent%scalar(3), '')
  call check_str2(ne, 262, 15, ent%scalar(4), '')
  call check_int2(ne, 262, 16, ent%scalar_ind(1), -1)

! 263: fgd_alter_entry BIT
  ent%field_type = GD_BIT_ENTRY
  ent%scalar(1) = 'carray'
  ent%scalar_ind(1) = 6
  call fgd_alter_entry(d, 'new246', ent, 2, 0)
  call check_ok2(ne, 263, 1, d)

  n = fgd_entry(d, 'new246', ent)
  call check_ok2(ne, 263, 2, d)
  call check_int2(ne, 263,  3, n, GD_BIT_ENTRY)
  call check_int2(ne, 263,  4, ent%fragment_index, 0)
  call check_int2(ne, 263,  5, ent%numbits, 93)
  call check_int2(ne, 263,  6, ent%bitnum, 16)
  call check_str2(ne, 263,  7, ent%field(1), 'in3')
  call check_str2(ne, 263,  8, ent%scalar(1), 'carray')
  call check_str2(ne, 263,  9, ent%scalar(2), '')

! 264: fgd_alter_entry SBIT
  ent%field_type = GD_SBIT_ENTRY
  ent%scalar(1) = 'carray'
  ent%scalar_ind(1) = 5
  ent%numbits = 0
  call fgd_alter_entry(d, 'new258', ent, 1, 0)
  call check_ok2(ne, 264, 1, d)

  n = fgd_entry(d, 'new258', ent)
  call check_ok2(ne, 264, 2, d)
  call check_int2(ne, 264,  3, n, GD_SBIT_ENTRY)
  call check_int2(ne, 264,  4, ent%fragment_index, 0)
  call check_int2(ne, 264,  5, ent%numbits, 93)
  call check_int2(ne, 264,  6, ent%bitnum, 16)
  call check_str2(ne, 264,  7, ent%field(1), 'in3')
  call check_str2(ne, 264,  8, ent%scalar(1), '')
  call check_str2(ne, 264,  9, ent%scalar(2), 'const')
  call check_int2(ne, 264, 10, ent%scalar_ind(2), -1)

! 265: fgd_alter_entry PHASE
  ent%field_type = GD_PHASE_ENTRY
  ent%field(1) = 'in2'
  ent%shift = -265
  call fgd_alter_entry(d, 'new248', ent, 1, 0)
  call check_ok2(ne,  265, 1, d)

  n = fgd_entry(d, 'new248', ent)
  call check_ok2(ne,  265, 2, d)
  call check_int2(ne, 265, 1, n, GD_PHASE_ENTRY)
  call check_int2(ne, 265, 2, ent%fragment_index, 0)
  call check_int2(ne, 265, 3, ent%shift, -265)
  call check_str2(ne, 265, 4, ent%field(1), 'in2')
  call check_str2(ne, 265, 5, ent%scalar(1), '')
 
! 266: fgd_alter_entry RECIP
  ent%field_type = GD_RECIP_ENTRY
  ent%flags = 0
  ent%field(1) = 'in5'
  ent%scalar(1) = 'carray'
  ent%scalar_ind(1) = 2
  call fgd_alter_entry(d, 'new251', ent, 0, 0)
  call check_ok2(ne, 266, 1, d)

  n = fgd_entry(d, 'new251', ent)
  call check_ok2(ne, 266, 2, d)
  call check_int2(ne, 266, 3, n, GD_RECIP_ENTRY)
  call check_int2(ne, 266, 4, ent%fragment_index, 0)
  call check_int2(ne, 266, 5, ent%flags, GD_EN_CALC)
  call check_str2(ne, 266, 6, ent%field(1), 'in5')
  call check_dbl2(ne, 266, 7, ent%dividend, 12d0)
  call check_str2(ne, 266, 8, ent%scalar(1), 'carray')
  call check_int2(ne, 266, 9, ent%scalar_ind(1), 2)

! 267: fgd_alter_entry CRECIP
  ent%field_type = GD_RECIP_ENTRY
  ent%field(1) = 'in4'
  ent%flags = GD_EN_COMPSCAL
  ent%cdividend = dcmplx(12d0, 14d0)
  call fgd_alter_entry(d, 'new251', ent, 15, 0)
  call check_ok2(ne, 267, 1, d)

  n = fgd_entry(d, 'new251', ent)
  call check_ok2(ne, 267, 2, d)
  call check_int2(ne, 267, 3, n, GD_RECIP_ENTRY)
  call check_int2(ne, 267, 4, ent%fragment_index, 0)
  call check_int2(ne, 267, 5, ent%flags, GD_EN_CALC + GD_EN_COMPSCAL)
  call check_str2(ne, 267, 6, ent%field(1), 'in4')
  call check_cpx2(ne, 267, 7, ent%cdividend, dcmplx(12d0, 14d0))
  call check_str2(ne, 267, 8, ent%scalar(1), '')

! 268: fgd_alter_entry WINDOW
  ent%field_type = GD_WINDOW_ENTRY
  ent%field(1) = ''
  ent%field(2) = 'in4'
  ent%windop = GD_WINDOP_LT
  ent%scalar(1) = 'carray'
  ent%scalar_ind(1) = 3
  call fgd_alter_entry(d, 'new253', ent, 0, 0)
  call check_ok2(ne, 268, 1, d)

  n = fgd_entry(d, 'new253', ent)
  call check_ok2(ne,  268,  2, d)
  call check_int2(ne, 268,  3, n, GD_WINDOW_ENTRY)
  call check_int2(ne, 268,  4, ent%fragment_index, 0)
  call check_int2(ne, 268,  5, ent%windop, GD_WINDOP_LT)
  call check_str2(ne, 268,  6, ent%field(1), 'in2')
  call check_str2(ne, 268,  7, ent%field(2), 'in4')
  call check_dbl2(ne, 268,  8, ent%rthreshold, 179d0)
  call check_str2(ne, 268,  9, ent%scalar(1), 'carray')
  call check_int2(ne, 268, 10, ent%scalar_ind(1), 3)

! 269: fgd_alter MPLEX
  ent%field_type = GD_MPLEX_ENTRY
  ent%field(1) = 'in0'
  ent%field(2) = ''
  ent%scalar(1) = ''
  ent%period = -1
  call fgd_alter_entry(d, 'new254', ent, 2, 0)
  call check_ok2(ne, 269, 1, d)

  n = fgd_entry(d, 'new254', ent)
  call check_ok2(ne,  269,  2, d)
  call check_int2(ne, 269,  3, n, GD_MPLEX_ENTRY)
  call check_int2(ne, 269,  4, ent%fragment_index, 0)
  call check_int2(ne, 269,  5, ent%count_val, 179)
  call check_int2(ne, 269,  6, ent%period, 180)
  call check_str2(ne, 269,  7, ent%field(1), 'in0')
  call check_str2(ne, 269,  8, ent%field(2), 'in2')
  call check_str2(ne, 269,  9, ent%scalar(1), 'carray')
  call check_int2(ne, 269, 10, ent%scalar_ind(1), 3)
  call check_str2(ne, 269, 11, ent%scalar(2), '')

! 271: fgd_encoding_support
  n = fgd_encoding_support(GD_SIE_ENCODED)
  call check_int(ne, 271, n, GD_RDWR)

! 272: NULL return from gd_reference
  l = fgd_open(fildir//DIRSEP//'empty', GD_RDWR + GD_CREAT + GD_EXCL)
  call check_ok2(ne, 272, 1, l)

  str = fgd_reference(l, "")
  call check_ok2(ne, 272, 2, l)
  call check_str(ne, 272, str, '')

  call fgd_discard(l)

! 275: fgd_sarray_value_max
  n = fgd_sarray_value_max(d)
  call check_int(ne, 275, n, 5)

! 276: fgd_msarray_value_max
  n = fgd_msarray_value_max(d, 'data')
  call check_int(ne, 276, n, 6)

! 277: gd_entry (SARRAY)
  n = fgd_entry(d, 'sarray', ent)
  call check_ok(ne, 277, d)
  call check_int2(ne, 277, 1, n, GD_SARRAY_ENTRY)
  call check_int2(ne, 277, 2, ent%fragment_index, 0)
  call check_int2(ne, 277, 3, ent%array_len, 7)

! 278: gd_get_sarray
  fields(1) = 'one'
  fields(2) = 'two'
  fields(3) = 'three'
  fields(4) = 'four'
  fields(5) = 'five'
  fields(6) = 'six'
  fields(7) = 'seven'
  l = flen
  call fgd_get_sarray(flist, l, d, 'sarray')
  call check_ok(ne, 278, d)
  call check_int(ne, 278, l, flen)
  do i=1,7
  call check_str2(ne, 278, i, flist(i), fields(i))
  end do 

! 279: gd_get_sarray_slice
  l = flen
  call fgd_get_sarray_slice(flist, l, d, 'sarray', 4, 3)
  call check_ok(ne, 279, d)
  call check_int(ne, 279, l, flen)
  do i=1,3
  call check_str2(ne, 279, i, flist(i), fields(i + 3))
  end do 

! 281: gd_put_sarray
  fields(1) = 'eka'
  fields(2) = 'dvi'
  fields(3) = 'tri'
  fields(4) = 'catur'
  fields(5) = 'panca'
  fields(6) = 'sas'
  fields(7) = 'sapta'
  call fgd_put_sarray(d, 'sarray', fields)
  call check_ok2(ne, 281, 1, d)

  l = flen
  call fgd_get_sarray(flist, l, d, 'sarray')
  call check_ok2(ne, 281, 2, d)
  call check_int(ne, 281, l, flen)
  do i=1,7
  call check_str2(ne, 281, i, flist(i), fields(i))
  end do 

! 282: gd_put_sarray_slice
  fields(4) = 'asta'
  fields(5) = 'nava'
  call fgd_put_sarray_slice(d, 'sarray', 4, 2, fields(4:5))
  call check_ok2(ne, 282, 1, d)

  l = flen
  call fgd_get_sarray(flist, l, d, 'sarray')
  call check_ok2(ne, 282, 2, d)
  call check_int(ne, 282, l, flen)
  do i=1,7
  call check_str2(ne, 282, i, flist(i), fields(i))
  end do 

! 283: gd_add_sarray
  call fgd_add_sarray(d, 'new283', 4, 0)
  call check_ok2(ne, 283, 1, d)

  n = fgd_entry(d, 'new283', ent)
  call check_ok2(ne, 283, 2, d)
  call check_int2(ne, 283, 1, n, GD_SARRAY_ENTRY)
  call check_int2(ne, 283, 2, ent%fragment_index, 0)
  call check_int2(ne, 283, 3, ent%array_len, 4)

  call fgd_get_sarray(flist, l, d, 'new283')
  call check_ok2(ne, 283, 3, d)

  do i=1,4
  call check_str2(ne, 283, i, flist(i), "");
  end do

! 285: gd_madd_sarray
  call fgd_madd_sarray(d, 'data', 'new285', 4)
  call check_ok2(ne, 285, 1, d)

  n = fgd_entry(d, 'data/new285', ent)
  call check_ok2(ne, 285, 2, d)
  call check_int2(ne, 285, 1, n, GD_SARRAY_ENTRY)
  call check_int2(ne, 285, 2, ent%fragment_index, 0)
  call check_int2(ne, 285, 3, ent%array_len, 4)

  call fgd_get_sarray(flist, l, d, 'data/new285')
  call check_ok2(ne, 285, 3, d)

  do i=1,4
  call check_str2(ne, 285, i, flist(i), "")
  end do

! 286: gd_alter_carray
  call fgd_alter_sarray(d, 'new283', 3)
  call check_ok2(ne, 286, 1, d)

  n = fgd_entry(d, 'new283', ent)
  call check_ok2(ne, 286, 2, d)
  call check_int2(ne, 286, 1, n, GD_SARRAY_ENTRY)
  call check_int2(ne, 286, 2, ent%fragment_index, 0)
  call check_int2(ne, 286, 3, ent%array_len, 3)

! 288: fgd_entry (INDIR)
  n = fgd_entry(d, 'indir', ent)
  call check_ok(ne, 288, d)
  call check_int2(ne, 288, 1, n, GD_INDIR_ENTRY)
  call check_int2(ne, 288, 2, ent%fragment_index, 0)
  call check_str2(ne, 288, 3, ent%field(1), 'data')
  call check_str2(ne, 288, 4, ent%field(2), 'carray')

! 289: fgd_add_indir check
  call fgd_add_indir(d, 'new289', 'in1', 'in2', 0)
  call check_ok2(ne, 289, 1, d)

  n = fgd_entry(d, 'new289', ent)
  call check_ok2(ne, 289, 2, d)
  call check_int2(ne, 289, 1, n, GD_INDIR_ENTRY)
  call check_int2(ne, 289, 2, ent%fragment_index, 0)
  call check_str2(ne, 289, 3, ent%field(1), 'in1')
  call check_str2(ne, 289, 4, ent%field(2), 'in2')

! 290: fgd_madd_indir check
  call fgd_madd_indir(d, 'data', 'new290', 'in3', 'in4')
  call check_ok2(ne, 290, 1, d)

  n = fgd_entry(d, 'data/new290', ent)
  call check_ok2(ne, 290, 2, d)
  call check_int2(ne, 290, 1, n, GD_INDIR_ENTRY)
  call check_int2(ne, 290, 2, ent%fragment_index, 0)
  call check_str2(ne, 290, 3, ent%field(1), 'in3')
  call check_str2(ne, 290, 4, ent%field(2), 'in4')

! 291: fgd_alter_indir check
  call fgd_alter_indir(d, 'new289', 'in6', 'in4')
  call check_ok2(ne, 291, 1, d)

  n = fgd_entry(d, 'new289', ent)
  call check_ok2(ne, 291, 2, d)
  call check_int2(ne, 291, 1, n, GD_INDIR_ENTRY)
  call check_int2(ne, 291, 2, ent%fragment_index, 0)
  call check_str2(ne, 291, 3, ent%field(1), 'in6')
  call check_str2(ne, 291, 4, ent%field(2), 'in4')

! 292: fgd_entry (SINDIR)
  n = fgd_entry(d, 'sindir', ent)
  call check_ok(ne, 292, d)
  call check_int2(ne, 292, 1, n, GD_SINDIR_ENTRY)
  call check_int2(ne, 292, 2, ent%fragment_index, 0)
  call check_str2(ne, 292, 3, ent%field(1), 'data')
  call check_str2(ne, 292, 4, ent%field(2), 'sarray')

! 293: fgd_add_sindir check
  call fgd_add_sindir(d, 'new293', 'in1', 'in2', 0)
  call check_ok2(ne, 293, 1, d)

  n = fgd_entry(d, 'new293', ent)
  call check_ok2(ne, 293, 2, d)
  call check_int2(ne, 293, 1, n, GD_SINDIR_ENTRY)
  call check_int2(ne, 293, 2, ent%fragment_index, 0)
  call check_str2(ne, 293, 3, ent%field(1), 'in1')
  call check_str2(ne, 293, 4, ent%field(2), 'in2')

! 294: fgd_madd_sindir check
  call fgd_madd_sindir(d, 'data', 'new294', 'in3', 'in4')
  call check_ok2(ne, 294, 1, d)

  n = fgd_entry(d, 'data/new294', ent)
  call check_ok2(ne, 294, 2, d)
  call check_int2(ne, 294, 1, n, GD_SINDIR_ENTRY)
  call check_int2(ne, 294, 2, ent%fragment_index, 0)
  call check_str2(ne, 294, 3, ent%field(1), 'in3')
  call check_str2(ne, 294, 4, ent%field(2), 'in4')

! 295: fgd_alter_sindir check
  call fgd_alter_sindir(d, 'new293', 'in6', 'in4')
  call check_ok2(ne, 295, 1, d)

  n = fgd_entry(d, 'new293', ent)
  call check_ok2(ne, 295, 2, d)
  call check_int2(ne, 295, 1, n, GD_SINDIR_ENTRY)
  call check_int2(ne, 295, 2, ent%fragment_index, 0)
  call check_str2(ne, 295, 3, ent%field(1), 'in6')
  call check_str2(ne, 295, 4, ent%field(2), 'in4')

! 296: fgd_getstrdata check
  l = flen
  n  = fgd_getstrdata(d, 'sindir', 0, 0, 1, 0, flist, l)
  call check_ok(ne, 296, d)
  call check_int2(ne, 296, 1, l, flen)
  call check_int2(ne, 296, 2, n, 8)

  do i=1,8
  call check_str2(ne, 296, i, flist(i), 'eka')
  end do

! 302: fgd_include_ns
  call fgd_include_ns(d, 'format2', 0, 'NS', GD_CREAT + GD_EXCL)
  call check_ok(ne, 302, d)

! 303: fgd_fragment_namespace (read)
  str = fgd_fragment_namespace(d, 2)
  call check_ok(ne, 303, d)
  call check_eos(ne, 303, str, 'NS')

! 304: fgd_fragment_namespace (alter)
  str = fgd_alter_namespace(d, 2, 'NS2')
  call check_ok(ne, 304, d)
  call check_eos(ne, 304, str, 'NS2')

! 305: fgd_match_entries
  i = fgd_match_entries_max(d, "^lin", 0, 0, 0)
  call check_ok2(ne, 305, 1, d)
  call check_int2(ne, 305, 2, i, 7)
  
  l = flen
  n = fgd_match_entries(flist, d, "^lin", 0, 0, 0, l)
  call check_ok2(ne, 305, 3, d)
  call check_int2(ne, 305, 4, n, 2)
  call check_str2(ne, 305, 5, flist(1), "lincom")
  call check_str2(ne, 305, 6, flist(2), "linterp")
  call check_int2(ne, 305, 7, l, flen)




  


!================================================================
  call fgd_discard(d)

  call system ( 'rm -rf ' // fildir )

  if (ne .GT. 0) then
    write(*, 9000) ne
    call exit(1)
  end if

9000 format('ne = ', i0)
end program
