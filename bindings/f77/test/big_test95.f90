! Copyright (C) 2009-2012 D. V. Wiebe
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
subroutine check_err(ne, t, d, v)
  use getdata
  integer, intent(inout) :: ne
  integer, intent(in) :: t, d, v
  integer :: e

  e = fgd_error(d)

  if (e .ne. v) then
    ne = ne + 1
    write(*, 9001), t, e, v
  end if
9001 format('e[', i0, '] = ', i0, ', expected ', i0)
end subroutine 

subroutine check_int(ne, t, n, v)
  integer, intent(inout) :: ne
  integer, intent(in) :: t, n, v

  if (n .ne. v) then
    ne = ne + 1
    write(*, 9002), t, n, v
  end if
9002 format('n[', i0, '] = ', i0, ', expected ', i0)
end subroutine 

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

subroutine check_int2(ne, t, i, n, v)
  integer, intent(inout) :: ne
  integer, intent(in) :: t, i, n, v

  if (n .ne. v) then
    ne = ne + 1
    write(*, 9007), i, t, n, v
  end if
9007 format('n(', i0, ')[', i0, '] = ', i0, ', expected ', i0)
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

  if (n .ne. v) then
    ne = ne + 1
    write(*, 9009), t, n, v
  end if
9009 format('s[', i0, '] = "', a, '", expected "', a, '"')
end subroutine 

subroutine check_dbl2(ne, t, i, n, v)
  integer, intent(inout) :: ne
  integer, intent(in) :: t, i
  double precision, intent(in) :: n, v

  if (abs(n - v) .gt. 1e-5) then
    ne = ne + 1
    write(*, 9010), i, t, n, v
  end if
9010 format('r(', i0, ')[', i0, '] = ', d16.10, ', expected ', d16.10)
end subroutine 

subroutine check_cpx2(ne, t, i, n, v)
  integer, intent(inout) :: ne
  integer, intent(in) :: t, i
  double complex, intent(in) :: n, v

  if (abs(n - v) .gt. 1e-5) then
    ne = ne + 1
    write(*, 9011), i, t, real(real(n)), real(aimag(n)), &
    real(real(v)), real(aimag(v))
  end if
9011 format('x(', i0, ')[', i0, '] = ', d16.10, ';', d16.10, ', expected ', &
d16.10, ';', d16.10)
end subroutine 

subroutine check_dbl(ne, t, n, v)
  integer, intent(inout) :: ne
  integer, intent(in) :: t
  double precision, intent(in) :: n, v

  if (abs(n - v) .gt. 1e-5) then
    ne = ne + 1
    write(*, 9012), t, n, d
  end if
9012 format('r[', i0, '] = ', d16.10, ', expected ', d16.10)
end subroutine 

subroutine check_cpx(ne, t, n, v)
  integer, intent(inout) :: ne
  integer, intent(in) :: t
  double complex, intent(in) :: n, v

  if (abs(n - v) .gt. 1e-5) then
    ne = ne + 1
    write(*, 9013), t, n, d
  end if
9013 format('x[', i0, '] = ', d16.10, ';', d16.10, ', expected ', d16.10, &
';', d16.10)
end subroutine 

subroutine check_ok(ne, t, d)
  use getdata
  integer, intent(inout) :: ne
  integer, intent(in) :: t, d
  call check_err(ne, t, d, GD_E_OK)
end subroutine

subroutine check_ok2(ne, t, i, d)
  use getdata
  integer, intent(inout) :: ne
  integer, intent(in) :: t, d
  call check_err2(ne, t, i, d, GD_E_OK)
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
  character (len=*), parameter :: fildir = 'test95_dirfile'
  character (len=*), parameter :: frmat = 'test95_dirfile/format'
  character (len=*), parameter :: frm2 = 'test95_dirfile/form2'
  character (len=*), parameter :: dat = 'test95_dirfile/data'
  integer, parameter :: flen = 11
  integer, parameter :: nfields = 17
  integer, parameter :: slen = 26

  character (len=slen), dimension(3) :: strings
  character (len=slen), dimension(3) :: st
  character (len=flen), dimension(nfields + 8) :: fields
  character (len=flen), dimension(nfields + 8) :: flist
  character (len=GD_FIELD_LEN) :: str
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

  fields = (/    'INDEX      ', 'alias      ', 'bit        ', 'carray     ', &
  'const      ', 'data       ', 'div        ', 'lincom     ', 'linterp    ', &
  'mplex      ', 'mult       ', 'phase      ', 'polynom    ', 'recip      ', &
  'sbit       ', 'string     ', 'window     ', '           ', '           ', &
  '           ', '           ', '           ', '           ', '           ', &
  '           ' /)

  open(1, file=frmat, status='new')
  write(1, *) '/ENDIAN little'
  write(1, *) 'data RAW INT8 8'
  write(1, *) 'lincom LINCOM data 1.1 2.2 INDEX 2.2 3.3;4.4 linterp const const'
  write(1, *) '/META data mstr STRING "This is a string constant."'
  write(1, *) '/META data mconst CONST COMPLEX128 3.3;4.4'
  write(1, *) '/META data mlut LINTERP DATA ./lut'
  write(1, *) 'const CONST FLOAT64 5.5'
  write(1, *) 'carray CARRAY FLOAT64 1.1 2.2 3.3 4.4 5.5 6.6'
  write(1, *) 'linterp LINTERP data /look/up/file'
  write(1, *) 'polynom POLYNOM data 1.1 2.2 2.2 3.3;4.4 const const'
  write(1, *) 'bit BIT data 3 4'
  write(1, *) 'sbit SBIT data 5 6'
  write(1, *) 'mplex MPLEX sbit data 1 10'
  write(1, *) 'mult MULTIPLY data sbit'
  write(1, *) 'phase PHASE data 11'
  write(1, *) 'div DIVIDE mult bit'
  write(1, *) 'recip RECIP div 6.5;4.3'
  write(1, *) 'window WINDOW linterp mult LT 4.1'
  write(1, *) '/ALIAS alias data'
  write(1, *) 'string STRING "Zaphod Beeblebrox"'
  close(1, status='keep')

  open(1, file=frm2, status='new')
  write(1, *) 'const2 CONST INT8 -19'
  close(1, status='keep')

  open(1, file=dat, form='unformatted', access='direct', recl=80, &
  status='new')
  write(1, rec=1) datadata
  close(1, status='keep')

  ne = 0

! fgd_error check
  d = fgd_open('x', GD_RDONLY)
  call check_err(ne, 0, d, GD_E_OPEN)
  call fgd_discard(d)

! 1: fgd_open check
  d = fgd_open(fildir, GD_RDWR)
  call check_ok(ne, 1, d)

! 2: fgd_getdata_i1 check
  n = fgd_getdata_i1(d, 'data', 5, 0, 1, 0, ci1)
  call check_ok(ne, 2, d)
  call check_int(ne, 2, n, 8)

  do i=1,8
  call check_int2(ne, 2, i, int(ci1(i)), 40 + i)
  end do 

! 102: fgd_getdata_i2 check
  n = fgd_getdata_i2(d, 'data', 5, 0, 1, 0, ci2)
  call check_ok(ne, 102, d)
  call check_int(ne, 102, n, 8)

  do i=1,8
  call check_int2(ne, 102, i, int(ci2(i)), 40 + i)
  end do 

! 103: fgd_getdata_i4 check
  n = fgd_getdata_i4(d, 'data', 5, 0, 1, 0, ci4)
  call check_ok(ne, 103, d)
  call check_int(ne, 103, n, 8)

  do i=1,8
  call check_int2(ne, 103, i, int(ci4(i)), 40 + i)
  end do 

! 104: fgd_getdata_i8 check
  n = fgd_getdata_i8(d, 'data', 5, 0, 1, 0, ci8)
  call check_ok(ne, 104, d)
  call check_int(ne, 104, n, 8)

  do i=1,8
  call check_int2(ne, 104, i, int(ci8(i)), 40 + i)
  end do 

! 105: fgd_getdata_r4 check
  n = fgd_getdata_r4(d, 'data', 5, 0, 1, 0, cr4)
  call check_ok(ne, 105, d)
  call check_int(ne, 105, n, 8)

  do i=1,8
  call check_dbl2(ne, 105, i, 1d0 * cr4(i), 40d0 + i)
  end do 

! 106: fgd_getdata_r8 check
  n = fgd_getdata_r8(d, 'data', 5, 0, 1, 0, cr8)
  call check_ok(ne, 106, d)
  call check_int(ne, 106, n, 8)

  do i=1,8
  call check_dbl2(ne, 106, i, cr8(i), 40d0 + i)
  end do 

! 107: fgd_getdata_c8 check
  n = fgd_getdata_c8(d, 'data', 5, 0, 1, 0, cc8)
  call check_ok(ne, 107, d)
  call check_int(ne, 107, n, 8)

  do i=1,8
  call check_cpx2(ne, 107, i, 1d0 * cc8(i), dcmplx(40 + i, 0))
  end do 

! 108: fgd_getdata_c16 check
  n = fgd_getdata_c16(d, 'data', 5, 0, 1, 0, cc16)
  call check_ok(ne, 108, d)
  call check_int(ne, 108, n, 8)

  do i=1,8
  call check_cpx2(ne, 108, i, cc16(i), dcmplx(40 + i, 0))
  end do 

! 109: fgd_getdata_n check
  n = fgd_getdata_n(d, 'data', 5, 0, 1, 0)
  call check_ok(ne, 109, d)
  call check_int(ne, 109, n, 8)

! 3: fgd_get_constant_i1 check
  call fgd_get_constant_i1(d, 'const', ci1(1))
  call check_ok(ne, 3, d)
  call check_int(ne, 3, int(ci1(1)), 5)

! 110: fgd_get_constant_i2 check
  call fgd_get_constant_i2(d, 'const', ci2(1))
  call check_ok(ne, 110, d)
  call check_int(ne, 110, int(ci2(1)), 5)

! 111: fgd_get_constant_i4 check
  call fgd_get_constant_i4(d, 'const', ci4(1))
  call check_ok(ne, 111, d)
  call check_int(ne, 111, ci4(1), 5)

! 112: fgd_get_constant_i8 check
  call fgd_get_constant_i8(d, 'const', ci8(1))
  call check_ok(ne, 112, d)
  call check_int(ne, 112, int(ci8(1)), 5)

! 113: fgd_get_constant_r4 check
  call fgd_get_constant_r4(d, 'const', cr4(1))
  call check_ok(ne, 113, d)
  call check_dbl(ne, 113, 1d0 * cr4(1), 5.5d0)

! 114: fgd_get_constant_r8 check
  call fgd_get_constant_r8(d, 'const', cr8(1))
  call check_ok(ne, 114, d)
  call check_dbl(ne, 114, cr8(1), 5.5d0)

! 115: fgd_get_constant_c8 check
  call fgd_get_constant_c8(d, 'const', cc8(1))
  call check_ok(ne, 115, d)
  call check_cpx(ne, 115, 1d0 * cc8(1), dcmplx(5.5, 0))

! 116: fgd_get_constant_c16 check
  call fgd_get_constant_c16(d, 'const', cc16(1))
  call check_ok(ne, 116, d)
  call check_cpx(ne, 116, cc16(1), dcmplx(5.5, 0))

! 117: fgd_get_constant_n check
  call fgd_get_constant_n(d, 'const')
  call check_ok(ne, 117, d)

! 4: fgd_field_name_max check
  i = fgd_field_name_max(d)
  call check_ok(ne, 4, d)
  call check_int(ne, 4, i, 7)

! 5: fgd_mfield_name_max check
  i = fgd_mfield_name_max(d, 'data')
  call check_ok(ne, 5, d)
  call check_int(ne, 5, i, 6)

! 6: fgd_nfields check
  n = fgd_nfields(d)
  call check_ok(ne, 6, d)
  call check_int(ne, 6, n, nfields)

! 8: fgd_field_list check
  l = flen
  call fgd_field_list(flist, d, l)
  call check_ok(ne, 8, d)
  call check_int(ne, 8, l, flen)

  do i = 1, n
  call check_str2(ne, 8, i, flist(i), fields(i))
  end do

! 9: fgd_nmfields check
  n = fgd_nmfields(d, 'data')
  call check_ok(ne, 9, d)
  call check_int(ne, 9, n, 3)

! 10: fgd_mfield_list check
  fields(1) = 'mstr'
  fields(2) = 'mconst'
  fields(3) = 'mlut'

  l = flen
  call fgd_mfield_list(flist, d, 'data', l)
  call check_ok2(ne, 10, i, d)
  call check_int2(ne, 10, i, l, flen)

  DO i = 1, n
  call check_str2(ne, 10, i, flist(i), fields(i))
  end do

! 11: fgd_nframes check
  n = fgd_nframes(d)
  call check_ok(ne, 11, d)
  call check_int(ne, 11, n, 10)

! 12: fgd_spf check
  n = fgd_spf(d, 'data')
  call check_ok(ne, 12, d)
  call check_int(ne, 12, n, 8)

! 13: fgd_putdata_i1 check
  ci1 = (/ 13_1, 14_1, 15_1, 16_1, 17_1, 18_1, 19_1, 90_1 /)
  n = fgd_putdata_i1(d, 'data', 5, 1, 0, 4, ci1)
  call check_ok(ne, 13, d)
  call check_int(ne, 13, n, 4)

  n = fgd_getdata_i1(d, 'data', 5, 0, 1, 0, ci1)

  DO i = 1, 8
  if (i .eq. 1 .or. i .gt. 5) then
    n = 40 + i
  else
    n = 11 + i
  endif
  call check_int2(ne, 13, i, int(ci1(i)), n)
  end do

! 118: fgd_putdata_i2 check
  ci2 = (/ 23_2, 24_2, 25_2, 26_2, 27_2, 28_2, 29_2, 30_2 /)
  n = fgd_putdata_i2(d, 'data', 5, 1, 0, 4, ci2)
  call check_ok(ne, 118, d)
  call check_int(ne, 118, n, 4)

  n = fgd_getdata_i2(d, 'data', 5, 0, 1, 0, ci2)

  DO i = 1, 8
  if (i .eq. 1 .or. i .gt. 5) then
    n = 40 + i
  else
    n = 21 + i
  endif
  call check_int2(ne, 118, i, int(ci2(i)), n)
  end do

! 119: fgd_putdata_i4 check
  ci4 = (/ 33, 34, 35, 36, 37, 38, 39, 40 /)
  n = fgd_putdata_i4(d, 'data', 5, 1, 0, 4, ci4)
  call check_ok(ne, 119, d)
  call check_int(ne, 119, n, 4)

  n = fgd_getdata_i4(d, 'data', 5, 0, 1, 0, ci4)

  DO i = 1, 8
  if (i .eq. 1 .or. i .gt. 5) then
    n = 40 + i
  else
    n = 31 + i
  endif
  call check_int2(ne, 119, i, int(ci4(i)), n)
  end do

! 120: fgd_putdata_i8 check
  ci8 = (/ 43, 44, 45, 46, 47, 48, 49, 50 /)
  n = fgd_putdata_i8(d, 'data', 5, 1, 0, 4, ci8)
  call check_ok(ne, 120, d)
  call check_int(ne, 120, n, 4)

  n = fgd_getdata_i8(d, 'data', 5, 0, 1, 0, ci8)

  DO i = 1, 8
  if (i .eq. 1 .or. i .gt. 5) then
    n = 40 + i
  else
    n = 41 + i
  endif
  call check_int2(ne, 120, i, int(ci8(i)), n)
  end do

! 121: fgd_putdata_r4 check
  cr4 = (/ 33, 34, 35, 36, 37, 38, 39, 40 /)
  n = fgd_putdata_r4(d, 'data', 5, 1, 0, 4, cr4)
  call check_ok(ne, 121, d)
  call check_int(ne, 121, n, 4)

  n = fgd_getdata_r4(d, 'data', 5, 0, 1, 0, cr4)

  DO i = 1, 8
  if (i .eq. 1 .or. i .gt. 5) then
    dp = 40. + i
  else
    dp = 31. + i
  end if
  call check_dbl2(ne, 121, i, 1d0 * cr4(i), dp)
  end do

! 122: fgd_putdata_r8 check
  cr8 = (/ 43, 44, 45, 46, 47, 48, 49, 50 /)
  n = fgd_putdata_r8(d, 'data', 5, 1, 0, 4, cr8)
  call check_ok(ne, 122, d)
  call check_int(ne, 122, n, 4)

  n = fgd_getdata_r8(d, 'data', 5, 0, 1, 0, cr8)

  DO i = 1, 8
  if (i .eq. 1 .or. i .gt. 5) then
    dp = 40. + i
  else
    dp = 41. + i
  end if
  call check_dbl2(ne, 122, i, cr8(i), dp)
  end do

! 123: fgd_putdata_c8 check
  cc8 = (/ 53, 54, 55, 56, 57, 58, 59, 60 /)
  n = fgd_putdata_c8(d, 'data', 5, 1, 0, 4, cc8)
  call check_ok(ne, 123, d)
  call check_int(ne, 123, n, 4)

  n = fgd_getdata_c8(d, 'data', 5, 0, 1, 0, cc8)

  DO i = 1, 8
  if (i .eq. 1 .or. i .gt. 5) then
    dc = dcmplx(40d0 + i, 0d0)
  else
    dc = dcmplx(51d0 + i, 0d0)
  end if
  call check_cpx2(ne, 123, i, 1d0 * cc8(i), dc)
  end do

! 124: fgd_putdata_c16 check
  cc16 = (/ 63, 64, 65, 66, 67, 68, 69, 70 /)
  n = fgd_putdata_c16(d, 'data', 5, 1, 0, 4, cc16)
  call check_ok(ne, 124, d)
  call check_int(ne, 124, n, 4)

  n = fgd_getdata_c16(d, 'data', 5, 0, 1, 0, cc16)

  DO i = 1, 8
  if (i .eq. 1 .or. i .gt. 5) then
    dc = dcmplx(40d0 + i, 0d0)
  else
    dc = dcmplx(61d0 + i, 0d0)
  end if
  call check_cpx2(ne, 124, i, cc16(i), dc)
  end do

! 14: fgd_error_string check
  n = fgd_getdata_n(d, 'x', 5, 0, 1, 0)
  call fgd_error_string(d, str, GD_FIELD_LEN)
  call check_str(ne, 14, str, 'Field not found: x')

! 15: fgd_entry_type check
  n = fgd_entry_type(d, 'data')
  call check_ok(ne, 15, d)
  call check_int(ne, 15, n, GD_RAW_ENTRY)

! 16: fgd_entry (raw) check
  n = fgd_entry(d, 'data', ent)
  call check_ok(ne, 16, d)
  call check_int2(ne, 16, 1, n, GD_RAW_ENTRY)
  call check_int2(ne, 16, 2, ent%fragment_index, 0)
  call check_int2(ne, 16, 3, ent%spf, 8)
  call check_int2(ne, 16, 4, ent%data_type, GD_INT8)

! 18: fgd_entry (lincom) check
  n = fgd_entry(d, 'lincom', ent)
  call check_ok(ne, 18, d)
  call check_int2(ne, 18, 1, n, GD_LINCOM_ENTRY)
  call check_int2(ne, 18, 2, ent%n_fields, 3)
  call check_int2(ne, 18, 3, ent%fragment_index, 0)
  call check_str2(ne, 18, 4, ent%field(1), 'data')
  call check_str2(ne, 18, 5, ent%field(2), 'INDEX')
  call check_str2(ne, 18, 6, ent%field(3), 'linterp')
  call check_int2(ne, 18, 7, ent%comp_scal, 1)
  call check_str2(ne, 18, 8, ent%scalar(3), 'const')
  call check_str2(ne, 18, 9, ent%scalar(6), 'const')

  cq(1) = dcmplx(1.1, 0.0)
  cq(2) = dcmplx(2.2, 0.0)
  cq(3) = dcmplx(2.2, 0.0)
  cq(4) = dcmplx(3.3, 4.4)
  cq(5) = dcmplx(5.5, 0.0)
  cq(6) = dcmplx(5.5, 0.0)
  DO i=1,3
  call check_dbl2(ne, 18, i * 2 - 1, ent%cm(i), cq(i * 2 - 1))
  call check_dbl2(ne, 18, i * 2, ent%cb(i), cq(i * 2))
  end do

! 20: fgd_entry (polynom) check
  n = fgd_entry(d, 'polynom', ent)
  call check_ok(ne, 20, d)
  call check_int2(ne, 20, 1, n, GD_POLYNOM_ENTRY)
  call check_int2(ne, 20, 2, ent%poly_ord, 5)
  call check_int2(ne, 20, 3, ent%fragment_index, 0)
  call check_str2(ne, 20, 4, ent%field(1), 'data')

  cq(1) = dcmplx(1.1, 0.0)
  cq(2) = dcmplx(2.2, 0.0)
  cq(3) = dcmplx(2.2, 0.0)
  cq(4) = dcmplx(3.3, 4.4)
  cq(5) = dcmplx(5.5, 0.0)
  cq(6) = dcmplx(5.5, 0.0)
  DO i=1,6
  call check_cpx2(ne, 30, i, ent%ca(i), cq(i))
  end do

! 21: fgd_entry (linterp) check
  n = fgd_entry(d, 'linterp', ent)
  call check_ok(ne, 21, d)
  call check_int2(ne, 21, 1, n, GD_LINTERP_ENTRY)
  call check_int2(ne, 21, 2, ent%fragment_index, 0)
  call check_str2(ne, 21, 3, ent%field(1), 'data')
  call check_str2(ne, 21, 4, ent%field(2), '/look/up/file')

! 22: fgd_entry (bit) check
  n = fgd_entry(d, 'bit', ent)
  call check_ok(ne, 22, d)
  call check_int2(ne, 22, 1, n, GD_BIT_ENTRY)
  call check_int2(ne, 22, 2, ent%fragment_index, 0)
  call check_int2(ne, 22, 3, ent%bitnum, 3)
  call check_int2(ne, 22, 4, ent%numbits, 4)
  call check_str2(ne, 22, 5, ent%field(1), 'data')

! 23: fgd_entry (Sbit) check
  n = fgd_entry(d, 'sbit', ent)
  call check_ok(ne, 23, d)
  call check_int2(ne, 23, 1, n, GD_SBIT_ENTRY)
  call check_int2(ne, 23, 2, ent%fragment_index, 0)
  call check_int2(ne, 23, 3, ent%numbits, 6)
  call check_int2(ne, 23, 4, ent%bitnum, 5)
  call check_str2(ne, 23, 5, ent%field(1), 'data')

! 24: fgd_entry (multiply) check
  n = fgd_entry(d, 'mult', ent)
  call check_ok(ne, 24, d)
  call check_int2(ne, 24, 1, n, GD_MULTIPLY_ENTRY)
  call check_int2(ne, 24, 2, ent%fragment_index, 0)
  call check_str2(ne, 24, 3, ent%field(1), 'data')
  call check_str2(ne, 24, 4, ent%field(2), 'sbit')

! 25: fgd_entry (phase) check
  n = fgd_entry(d, 'phase', ent)
  call check_ok(ne, 25, d)
  call check_int2(ne, 25, 1, n, GD_PHASE_ENTRY)
  call check_int2(ne, 25, 2, ent%fragment_index, 0)
  call check_int2(ne, 25, 3, ent%shift, 11)
  call check_str2(ne, 25, 4, ent%field(1), 'data')

! 26: fgd_entry (const) check
  n = fgd_entry(d, 'const', ent)
  call check_ok(ne, 26, d)
  call check_int2(ne, 26, 1, n, GD_CONST_ENTRY)
  call check_int2(ne, 26, 2, ent%fragment_index, 0)
  call check_int2(ne, 26, 3, ent%data_type, GD_FLOAT64)

! 27: fgd_fragment_index check
  n = fgd_fragment_index(d, 'const')
  call check_ok(ne, 27, d)
  call check_int(ne, 27, n, 0)

! 28: fgd_add_raw check
  call fgd_add_raw(d, 'new1', GD_FLOAT64, 3, 0)
  call check_ok2(ne, 28, 1, d)

  n = fgd_entry(d, 'new1', ent)
  call check_ok2(ne, 28, 2, d)
  call check_int2(ne, 28, 3, ent%fragment_index, 0)
  call check_int2(ne, 28, 4, ent%spf, 3)
  call check_int2(ne, 28, 5, ent%data_type, GD_FLOAT64)
  call check_int2(ne, 28, 6, n, GD_RAW_ENTRY)

! 29: fgd_add_lincom check
  call fgd_add_lincom(d, 'new2', 2, 'in1', 9.9d0, 8.8d0, &
  'in2', 7.7d0, 6.6d0, '', 0d0, 0d0, 0)
  call check_ok2(ne, 29, 1, d)

  n = fgd_entry(d, 'new2', ent)
  call check_ok2(ne, 29, 2, d)
  call check_int2(ne, 29, 3, n, GD_LINCOM_ENTRY)
  call check_int2(ne, 29, 4, ent%n_fields, 2)
  call check_int2(ne, 29, 5, ent%fragment_index, 0)
  call check_str2(ne, 29, 6, ent%field(1), 'in1')
  call check_str2(ne, 29, 7, ent%field(2), 'in2')
  call check_int2(ne, 29, 8, ent%comp_scal, 0)

  q = (/ 9.9, 8.8, 7.7, 6.6, 5.5, 5.5 /)
  do i=1,2
  call check_dbl2(ne, 29, i * 2 - 1, ent%m(i), q(i * 2 - 1))
  call check_dbl2(ne, 29, i * 2, ent%b(i), q(i * 2))
  end do

! 30: fgd_add_clincom check
  cq(1) = dcmplx(1.1, 1.2)
  cq(2) = dcmplx(1.3, 1.4)
  cq(3) = dcmplx(1.4, 1.5)
  cq(4) = dcmplx(1.6, 1.7)
  call fgd_add_clincom(d, 'new3', 2, 'in1', cq(1), cq(2), &
  'in2', cq(3), cq(4), '', cq(5), cq(6), 0)
  call check_ok2(ne, 30, 1, d)

  n = fgd_entry(d, 'new3', ent)
  call check_ok2(ne, 30, 2, d)
  call check_int2(ne, 30, 1, ent%n_fields, 2)
  call check_int2(ne, 30, 2, n, GD_LINCOM_ENTRY)
  call check_int2(ne, 30, 3, ent%fragment_index, 0)
  call check_str2(ne, 30, 4, ent%field(1), 'in1')
  call check_str2(ne, 30, 5, ent%field(2), 'in2')
  call check_int2(ne, 30, 6, ent%comp_scal, 1)

  cq(1) = dcmplx(1.1, 1.2)
  cq(2) = dcmplx(1.3, 1.4)
  cq(3) = dcmplx(1.4, 1.5)
  cq(4) = dcmplx(1.6, 1.7)
  do i=1,2
  call check_cpx2(ne, 30, i * 2 - 1, ent%cm(i), cq(i * 2 - 1))
  call check_cpx2(ne, 30, i * 2, ent%cb(i), cq(i * 2))
  end do

! 31: fgd_add_polynom check
  call fgd_add_polynom(d, 'new4', 3, 'in1', 3d3, 4d4, 5d5, 6d6, 0d0, 0d0, &
  0)
  call check_ok2(ne, 31, 1, d)

  n = fgd_entry(d, 'new4', ent)
  call check_ok2(ne, 31, 2, d)
  call check_int2(ne, 31, 1, n, GD_POLYNOM_ENTRY)
  call check_int2(ne, 31, 2, ent%poly_ord, 3)
  call check_int2(ne, 31, 3, ent%fragment_index, 0)
  call check_str2(ne, 31, 4, ent%field(1), 'in1')
  call check_int2(ne, 31, 5, ent%comp_scal, 0)

  q = (/ 3d3, 4d4, 5d5, 6d6, 5.5d0, 5.5d0 /)
  DO i=1,4
  call check_dbl2(ne, 31, i, ent%a(i), q(i))
  end do

! 32: fgd_add_cpolynom check
  cq(1) = dcmplx(3.1, 7.0)
  cq(2) = dcmplx(4.2, 8.0)
  cq(3) = dcmplx(5.2, 9.0)
  cq(4) = dcmplx(6.3, 4.4)
  call fgd_add_cpolynom(d, 'new5', 3, 'in1', cq(1), cq(2), cq(3), cq(4), &
  dcmplx(0d0,0d0), dcmplx(0d0,0d0), 0)
  call check_ok2(ne, 32, 1, d)

  n = fgd_entry(d, 'new5', ent)
  call check_ok2(ne, 32, 2, d)
  call check_int2(ne, 32, 1, n, GD_POLYNOM_ENTRY)
  call check_int2(ne, 32, 2, ent%poly_ord, 3)
  call check_int2(ne, 32, 3, ent%fragment_index, 0)
  call check_str2(ne, 32, 4, ent%field(1), 'in1')
  call check_int2(ne, 31, 5, ent%comp_scal, 1)

  cq(1) = dcmplx(3.1, 7.0)
  cq(2) = dcmplx(4.2, 8.0)
  cq(3) = dcmplx(5.2, 9.0)
  cq(4) = dcmplx(6.3, 4.4)
  DO i=1,4
  call check_cpx2(ne, 32, i, ent%ca(i), cq(i))
  end do

! 33: fgd_add_linterp check
  call fgd_add_linterp(d, "new6", "in", "./some/table", 0)
  call check_ok2(ne, 33, 1, d)

  n = fgd_entry(d, 'new6', ent)
  call check_ok2(ne, 33, 2, d)
  call check_int2(ne, 33, 1, n, GD_LINTERP_ENTRY)
  call check_int2(ne, 33, 2, ent%fragment_index, 0)
  call check_str2(ne, 33, 3, ent%field(1), 'in')
  call check_str2(ne, 33, 4, ent%field(2), './some/table')

! 34: fgd_add_bit check
  call fgd_add_bit(d, "new7", "in", 13, 12, 0)
  call check_ok2(ne, 34, 1, d)

  n = fgd_entry(d, 'new7', ent)
  call check_ok2(ne, 34, 2, d)
  call check_int2(ne, 34, 1, n, GD_BIT_ENTRY)
  call check_int2(ne, 34, 2, ent%fragment_index, 0)
  call check_int2(ne, 34, 3, ent%numbits, 12)
  call check_int2(ne, 34, 4, ent%bitnum, 13)
  call check_str2(ne, 34, 5, ent%field(1), 'in')

! 35: fgd_add_sbit check
  call fgd_add_sbit(d, "new8", "in", 13, 12, 0)
  call check_ok2(ne, 35, 1, d)

  n = fgd_entry(d, "new8", ent)
  call check_ok2(ne, 35, 2, d)
  call check_int2(ne, 35, 1, n, GD_SBIT_ENTRY)
  call check_int2(ne, 35, 2, ent%fragment_index, 0)
  call check_int2(ne, 35, 3, ent%numbits, 12)
  call check_int2(ne, 35, 4, ent%bitnum, 13)
  call check_str2(ne, 35, 5, ent%field(1), 'in')

! 36: fgd_add_multiply check
  call fgd_add_multiply(d, 'new9', 'in1', 'in2', 0)
  call check_ok2(ne, 36, 1, d)

  n = fgd_entry(d, 'new9', ent)
  call check_ok2(ne, 36, 2, d)
  call check_int2(ne, 36, 1, n, GD_MULTIPLY_ENTRY)
  call check_int2(ne, 36, 2, ent%fragment_index, 0)
  call check_str2(ne, 36, 3, ent%field(1), 'in1')
  call check_str2(ne, 36, 4, ent%field(2), 'in2')

! 37: fgd_add_phase check
  call fgd_add_phase(d, 'new10', 'in1', 22, 0)
  call check_ok2(ne, 37, 1, d)

  n = fgd_entry(d, 'new10', ent)
  call check_ok2(ne, 37, 2, d)
  call check_int2(ne, 37, 1, n, GD_PHASE_ENTRY)
  call check_int2(ne, 37, 2, ent%fragment_index, 0)
  call check_int2(ne, 37, 3, ent%shift, 22)
  call check_str2(ne, 37, 4, ent%field(1), 'in1')

! 38: fgd_add_const check
  call fgd_add_const(d, 'new11', GD_FLOAT64, 0)
  call check_ok2(ne, 38, 1, d)

  n = fgd_entry(d, 'new11', ent)
  call check_ok2(ne, 38, 2, d)
  call check_int2(ne, 38, 1, n, GD_CONST_ENTRY)
  call check_int2(ne, 38, 2, ent%fragment_index, 0)
  call check_int2(ne, 38, 3, ent%data_type, GD_FLOAT64)

  call fgd_get_constant_r4(d, 'new11', fl)
  call check_ok2(ne, 38, 3, d)
  call check_dbl(ne, 38, 1d0 * fl, 0d0)

! 125: fgd_add check
  ent%shift = 33
  ent%field(1) = 'new9'
  ent%fragment_index = 0
  ent%field_type = GD_PHASE_ENTRY
  call fgd_add(d, 'new13', ent)
  call check_ok2(ne, 125, 1, d)

  n = fgd_entry(d, 'new13', ent)
  call check_ok2(ne, 125, 2, d)
  call check_int2(ne, 125, 1, n, GD_PHASE_ENTRY)
  call check_int2(ne, 125, 2, ent%fragment_index, 0)
  call check_int2(ne, 125, 3, ent%shift, 33)
  call check_str2(ne, 125, 4, ent%field(1), 'new9')

! 39: fgd_fragmentname check
  str = fgd_fragmentname(d, 0)
  call check_ok(ne, 39, d)
  call check_eos(ne, 39, str, 'test95_dirfile/format')

! 40: fgd_nfragments check
  n = fgd_nfragments(d)
  call check_ok(ne, 40, d)
  call check_int(ne, 40, n, 1)

! 41: fgd_include check
  call fgd_include(d, 'form2', 0, 0)
  call check_ok2(ne, 41, 3, d)

  call fgd_get_constant_i1(d, 'const2', ci1(1))
  call check_ok2(ne, 41, 3, d)
  call check_int2(ne, 1, 41, int(ci1(1)), -19)

! 42: fgd_nfields_by_type check
  n = fgd_nfields_by_type(d, GD_LINCOM_ENTRY)
  call check_ok(ne, 42, d)
  call check_int(ne, 42, n, 3)

! 43: fgd_field_list_by_type check
  fields(1) = 'lincom'
  fields(2) = 'new2'
  fields(3) = 'new3'
  l = flen
  call fgd_field_list_by_type(flist, d, GD_LINCOM_ENTRY, l)
  call check_ok(ne, 43, d)
  call check_int(ne, 43, l, flen)

  do i = 1, n
  call check_str2(ne, 43, i, flist(i), fields(i))
  end do

! 44: fgd_nvectors check
  n = fgd_nvectors(d)
  call check_ok(ne, 44, d)
  call check_int(ne, 44, n, 25)

! 45: fgd_vector_list check
  fields = (/    'INDEX      ', 'alias      ', 'bit        ', 'data       ', &
  'div        ', 'lincom     ', 'linterp    ', 'mplex      ', 'mult       ', &
  'new1       ', 'new10      ', 'new13      ', 'new2       ', 'new3       ', &
  'new4       ', 'new5       ', 'new6       ', 'new7       ', 'new8       ', &
  'new9       ', 'phase      ', 'polynom    ', 'recip      ', 'sbit       ', &
  'window     ' /)
  l = flen
  call fgd_vector_list(flist, d, l)
  call check_ok(ne, 45, d)
  call check_int(ne, 45, l, flen)
 
  do i=1,n
  call check_str2(ne, 45, i, flist(i), fields(i))
  end do

! 46: fgd_madd_lincom check
  call fgd_madd_lincom(d, 'data', 'mnew1', 2, 'in1', 9.9d0, 8.8d0, &
  'in2', 7.7d0, 6.6d0, '', 0d0, 0d0)
  call check_ok2(ne, 46, 1, d)

  n = fgd_entry(d, 'data/mnew1', ent)
  call check_ok2(ne, 46, 2, d)
  call check_int2(ne, 46, 3, n, GD_LINCOM_ENTRY)
  call check_int2(ne, 46, 4, ent%n_fields, 2)
  call check_int2(ne, 46, 5, ent%fragment_index, 0)
  call check_str2(ne, 46, 6, ent%field(1), 'in1')
  call check_str2(ne, 46, 7, ent%field(2), 'in2')
  call check_int2(ne, 46, 8, ent%comp_scal, 0)

  q = (/ 9.9, 8.8, 7.7, 6.6, 5.5, 5.5 /)
  DO i=1,2
  call check_dbl2(ne, 46, i * 2 - 1, ent%m(i), q(i *  2 - 1))
  call check_dbl2(ne, 46, i * 2, ent%b(i), q(i * 2))
  end do

! 47: fgd_madd_clincom check
  cq(1) = dcmplx(1.1, 1.2)
  cq(2) = dcmplx(1.3, 1.4)
  cq(3) = dcmplx(1.4, 1.5)
  cq(4) = dcmplx(1.6, 1.7)
  call fgd_madd_clincom(d, 'data', 'mnew2', 2, 'in1', cq(1), cq(2), &
  'in2', cq(3), cq(4), '', cq(5), cq(6))
  call check_ok2(ne, 47, 1, d)

  n = fgd_entry(d, 'data/mnew2', ent)
  call check_ok(ne, 47, d)
  call check_int2(ne, 47, 1, n, GD_LINCOM_ENTRY)
  call check_int2(ne, 47, 2, ent%n_fields, 2)
  call check_int2(ne, 47, 3, ent%fragment_index, 0)
  call check_str2(ne, 47, 4, ent%field(1), 'in1')
  call check_str2(ne, 47, 5, ent%field(2), 'in2')
  call check_int2(ne, 47, 6, ent%comp_scal, 1)

  cq(1) = dcmplx(1.1, 1.2)
  cq(2) = dcmplx(1.3, 1.4)
  cq(3) = dcmplx(1.4, 1.5)
  cq(4) = dcmplx(1.6, 1.7)
  DO i=1,2
  call check_cpx2(ne, 47, i * 2 - 1, ent%cm(i), cq(i * 2 - 1))
  call check_cpx2(ne, 47, i * 2, ent%cb(i), cq(i * 2))
  end do

! 48: fgd_madd_polynom check
  call fgd_madd_polynom(d, 'data', 'mnew3', 3, 'in1', 3d3, 4d4, 5d5, &
     6d6, 0d0, 0d0)
  call check_ok2(ne, 48, 1, d)

  n = fgd_entry(d, 'data/mnew3', ent)
  call check_ok2(ne, 48, 2, d)
  call check_int2(ne, 48, 1, n, GD_POLYNOM_ENTRY)
  call check_int2(ne, 48, 2, ent%poly_ord, 3)
  call check_int2(ne, 48, 3, ent%fragment_index, 0)
  call check_str2(ne, 48, 4, ent%field(1), 'in1')

  q = (/ 3d3, 4d4, 5d5, 6d6, 5.5d0, 5.5d0 /)
  DO i=1,4
  call check_dbl2(ne, 48, i, ent%a(i), q(i))
  end do

! 49: fgd_madd_cpolynom check
  cq(1) = dcmplx(1.1, 0.0)
  cq(2) = dcmplx(2.2, 0.0)
  cq(3) = dcmplx(2.2, 0.0)
  cq(4) = dcmplx(3.3, 4.4)
  call fgd_madd_cpolynom(d, 'data', 'mnew5', 3, 'in1', cq(1), cq(2), &
     cq(3), cq(4), dcmplx(0d0,0d0), dcmplx(0d0,0d0))
  call check_ok2(ne, 49, 1, d)

  n = fgd_entry(d, 'data/mnew5', ent)
  call check_ok2(ne, 49, 2, d)
  call check_int2(ne, 49, 1, n, GD_POLYNOM_ENTRY)
  call check_int2(ne, 49, 2, ent%poly_ord, 3)
  call check_int2(ne, 49, 3, ent%fragment_index, 0)
  call check_str2(ne, 49, 4, ent%field(1), 'in1')

  cq(1) = dcmplx(1.1, 0.0)
  cq(2) = dcmplx(2.2, 0.0)
  cq(3) = dcmplx(2.2, 0.0)
  cq(4) = dcmplx(3.3, 4.4)
  DO i=1,4
  call check_cpx2(ne, 49, i, ent%ca(i), cq(i))
  end do

! 50: fgd_madd_linterp check
  call fgd_madd_linterp(d, "data", "mnew6", "in", "./more/table")
  call check_ok2(ne, 50, 1, d)

  n = fgd_entry(d, 'data/mnew6', ent)
  call check_ok2(ne, 50, 2, d)
  call check_int2(ne, 50, 1, n, GD_LINTERP_ENTRY)
  call check_int2(ne, 50, 2, ent%fragment_index, 0)
  call check_str2(ne, 50, 3, ent%field(1), 'in')
  call check_str2(ne, 50, 4, ent%field(2), './more/table')

! 51: fgd_madd_bit check
  call fgd_madd_bit(d, "data", "mnew7", "in", 13, 12)
  call check_ok2(ne, 51, 1, d)

  n = fgd_entry(d, 'data/mnew7', ent)
  call check_ok2(ne, 51, 2, d)
  call check_int2(ne, 51, 1, n, GD_BIT_ENTRY)
  call check_int2(ne, 51, 2, ent%fragment_index, 0)
  call check_int2(ne, 51, 3, ent%numbits, 12)
  call check_int2(ne, 51, 4, ent%bitnum, 13)
  call check_str2(ne, 51, 5, ent%field(1), 'in')

! 52: fgd_madd_sbit check
  call fgd_madd_sbit(d, "data", "mnew8", "in", 13, 12)
  call check_ok2(ne, 52, 1, d)

  n = fgd_entry(d, 'data/mnew8', ent)
  call check_ok2(ne, 52, 2, d)
  call check_int2(ne, 52, 1, n, GD_SBIT_ENTRY)
  call check_int2(ne, 52, 2, ent%fragment_index, 0)
  call check_int2(ne, 52, 3, ent%numbits, 12)
  call check_int2(ne, 52, 4, ent%bitnum, 13)
  call check_str2(ne, 52, 5, ent%field(1), 'in')

! 53: fgd_madd_multiply check
  call fgd_madd_multiply(d, 'data', 'mnew9', 'in1', 'in2')
  call check_ok2(ne, 53, 1, d)

  n = fgd_entry(d, 'data/mnew9', ent)
  call check_ok2(ne, 53, 2, d)
  call check_int2(ne, 53, 1, n, GD_MULTIPLY_ENTRY)
  call check_int2(ne, 53, 2, ent%fragment_index, 0)
  call check_str2(ne, 53, 3, ent%field(1), 'in1')
  call check_str2(ne, 53, 4, ent%field(2), 'in2')

! 54: fgd_madd_phase check
  call fgd_madd_phase(d, 'data', 'mnew10', 'in1', 22)
  call check_ok2(ne, 54, 1, d)

  n = fgd_entry(d, 'data/mnew10', ent)
  call check_ok2(ne, 54, 2, d)
  call check_int2(ne, 54, 1, n, GD_PHASE_ENTRY)
  call check_int2(ne, 54, 2, ent%fragment_index, 0)
  call check_int2(ne, 54, 3, ent%shift, 22)
  call check_str2(ne, 54, 4, ent%field(1), 'in1')

! 55: fgd_madd_const check
  call fgd_madd_const(d, 'data', 'mnew11', GD_FLOAT64)
  call check_ok2(ne, 55, 1, d)

  n = fgd_entry(d, 'data/mnew11', ent)
  call check_ok2(ne, 55, 2, d)
  call check_int2(ne, 55, 1, n, GD_CONST_ENTRY)
  call check_int2(ne, 55, 2, ent%fragment_index, 0)
  call check_int2(ne, 55, 3, ent%data_type, GD_FLOAT64)

  call fgd_get_constant_r4(d, 'data/mnew11', fl)
  call check_ok2(ne, 55, 3, d)
  call check_dbl(ne, 55, 1d0 * fl, 0d0)

! 126: fgd_madd check
  ent%shift = 33
  ent%field(1) = 'data/mnew10'
  ent%fragment_index = 0
  ent%field_type = GD_PHASE_ENTRY
  call fgd_madd(d, 'data', 'mnew4', ent)
  call check_ok2(ne, 126, 1, d)

  n = fgd_entry(d, 'data/mnew4', ent)
  call check_ok2(ne, 126, 2, d)
  call check_int2(ne, 126, 1, n, GD_PHASE_ENTRY)
  call check_int2(ne, 126, 2, ent%fragment_index, 0)
  call check_int2(ne, 126, 3, ent%shift, 33)
  call check_str2(ne, 126, 4, ent%field(1), 'data/mnew10')

! 56: fgd_get_string check
  n = fgd_get_string(d, 'string', GD_FIELD_LEN, str)
  call check_ok(ne, 56, d)
  call check_int(ne, 56, n, 17)
  call check_str(ne, 56, str, "Zaphod Beeblebrox")

! 57: fgd_add_string check
  call fgd_add_string(d, 'new12', 0)
  call check_ok2(ne, 57, 1, d)

  n = fgd_get_string(d, 'new12', GD_FIELD_LEN, str)
  call check_ok2(ne, 57, 2, d)
  call check_str(ne, 57, str, "")

! 58: fgd_madd_string check
  call fgd_madd_string(d, "data", 'mnew12')
  call check_ok2(ne, 58, 1, d)

  n = fgd_get_string(d, 'data/mnew12', GD_FIELD_LEN, str)
  call check_ok2(ne, 58, 2, d)
  call check_str(ne, 58, str, "")

! 59: fgd_add_spec check
  call fgd_add_spec(d, 'lorem STRING "Lorem ipsum"', 0)
  call check_ok2(ne, 59, 1, d)

  n = fgd_get_string(d, 'lorem', GD_FIELD_LEN, str)
  call check_ok2(ne, 59, 2, d)
  call check_str(ne, 59, str, "Lorem ipsum")

! 60: fgd_madd_spec check
  call fgd_madd_spec(d, 'ipsum STRING "dolor sit amet."', 'lorem')
  call check_ok2(ne, 60, 1, d)

  n = fgd_get_string(d, 'lorem/ipsum', GD_FIELD_LEN, str)
  call check_ok2(ne, 60, 2, d)
  call check_str(ne, 60, str, "dolor sit amet.")

! 61: fgd_put_constant_i1 check
  ci1(1) = 61
  call fgd_put_constant_i1(d, 'const', ci1(1))
  call check_ok2(ne, 61, 1, d)

  call fgd_get_constant_r4(d, 'const', fl)
  call check_ok2(ne, 61, 2, d)
  call check_dbl(ne, 61, 1d0 * fl, 61d0)

! 127: fgd_put_constant_i2 check
  ci2(1) = 127
  call fgd_put_constant_i2(d, 'const', ci2(1))
  call check_ok2(ne, 127, 1, d)

  call fgd_get_constant_r4(d, 'const', fl)
  call check_ok2(ne, 127, 2, d)
  call check_dbl(ne, 127, 1d0 * fl, 127d0)

! 128: fgd_put_constant_i4 check
  ci4(1) = 128
  call fgd_put_constant_i4(d, 'const', ci4(1))
  call check_ok2(ne, 128, 1, d)

  call fgd_get_constant_r4(d, 'const', fl)
  call check_ok2(ne, 128, 2, d)
  call check_dbl(ne, 128, 1d0 * fl, 128d0)

! 129: fgd_put_constant_i8 check
  ci8(1) = 129
  call fgd_put_constant_i8(d, 'const', ci8(1))
  call check_ok2(ne, 129, 1, d)

  call fgd_get_constant_r4(d, 'const', fl)
  call check_ok2(ne, 129, 2, d)
  call check_dbl(ne, 129, 1d0 * fl, 129d0)

! 130: fgd_put_constant_r4 check
  cr4(1) = -8.1
  call fgd_put_constant_r4(d, 'new11', cr4(1))
  call check_ok2(ne, 130, 1, d)

  call fgd_get_constant_r4(d, 'new11', fl)
  call check_ok2(ne, 130, 2, d)
  call check_dbl(ne, 130, 1d0 * fl, -8.1d0)

! 131: fgd_put_constant_r8 check
  cr8(1) = 131
  call fgd_put_constant_r8(d, 'const', cr8(1))
  call check_ok2(ne, 131, 1, d)

  call fgd_get_constant_r4(d, 'const', fl)
  call check_ok2(ne, 131, 2, d)
  call check_dbl(ne, 131, 1d0 * fl, 131d0)

! 132: fgd_put_constant_c8 check
  cc8(1) = 132
  call fgd_put_constant_c8(d, 'const', cc8(1))
  call check_ok2(ne, 132, 1, d)

  call fgd_get_constant_r4(d, 'const', fl)
  call check_ok2(ne, 132, 2, d)
  call check_dbl(ne, 132, 1d0 * fl, 132d0)

! 133: fgd_put_constant_c16 check
  cc16(1) = 133
  call fgd_put_constant_c16(d, 'const', cc16(1))
  call check_ok2(ne, 133, 1, d)

  call fgd_get_constant_r4(d, 'const', fl)
  call check_ok2(ne, 133, 2, d)
  call check_dbl(ne, 133, 1d0 * fl, 133d0)

! 62: fgd_put_string check
  n = fgd_put_string(d, 'string', "Arthur Dent")
  call check_ok2(ne, 62, 1, d)
  call check_int(ne, 62, n, 11)

  n = fgd_get_string(d, 'string', GD_FIELD_LEN, str)
  call check_ok2(ne, 62, 2, d)
  call check_str(ne, 62, str, "Arthur Dent")

! 63: fgd_nmfields_by_type check
  n = fgd_nmfields_by_type(d, "data", GD_LINCOM_ENTRY)
  call check_ok(ne, 63, d)
  call check_int(ne, 63, n, 2)

! 64: fgd_mfield_list_by_type check
  fields(1) = 'mnew1'
  fields(2) = 'mnew2'
  l = flen
  call fgd_mfield_list_by_type(flist, d, "data", GD_LINCOM_ENTRY, l)
  call check_ok2(ne, 64, i, d)
  call check_int2(ne, 64, i, l, flen)

  do i = 1, n
  call check_str2(ne, 64, i, flist(i), fields(i))
  end do

! 65: fgd_nmvectors check
  n = fgd_nmvectors(d, "data")
  call check_ok(ne, 65, d)
  call check_int(ne, 65, n, 11)

! 66: fgd_mvector_list check
  fields = (/    'mlut       ', 'mnew1      ', 'mnew2      ', 'mnew3      ', &
  'mnew5      ', 'mnew6      ', 'mnew7      ', 'mnew8      ', 'mnew9      ', &
  'mnew10     ', 'mnew4      ', '           ', '           ', '           ', &
  '           ', '           ', '           ', '           ', '           ', &
  '           ', '           ', '           ', '           ', '           ', &
  '           ' /)
  l = flen
  call fgd_mvector_list(flist, d, "data", l)
  call check_ok2(ne, 66, i, d)
  call check_int2(ne, 66, i, l, flen)

  do i=1,n
  call check_str2(ne, 66, i, flist(i), fields(i))
  end do

! 67: fgd_alter_raw check
  call fgd_alter_raw(d, 'new1', GD_INT32, 4, 0)
  call check_ok2(ne, 67, 1, d)

  n = fgd_entry(d, 'new1', ent)
  call check_ok2(ne, 67, 2, d)
  call check_int2(ne, 67, 3, ent%fragment_index, 0)
  call check_int2(ne, 67, 4, ent%spf, 4)
  call check_int2(ne, 67, 5, ent%data_type, GD_INT32)
  call check_int2(ne, 67, 6, n, GD_RAW_ENTRY)

! 68: fgd_alter_lincom check
  call fgd_alter_lincom(d, 'new2', 3, 'in4', 9.9d-1, 7.8d0, 'in5', &
     1.1d1, 2.2d-2, 'in6', 1.96d0, 0d0)
  call check_ok2(ne, 68, 1, d)

  n = fgd_entry(d, 'new2', ent)
  call check_ok2(ne, 68, 2, d)
  call check_int2(ne, 68, 3, n, GD_LINCOM_ENTRY)
  call check_int2(ne, 68, 4, ent%n_fields, 3)
  call check_int2(ne, 68, 5, ent%fragment_index, 0)
  call check_str2(ne, 68, 6, ent%field(1), 'in4')
  call check_str2(ne, 68, 7, ent%field(2), 'in5')
  call check_str2(ne, 68, 8, ent%field(3), 'in6')
  call check_int2(ne, 68, 5, ent%comp_scal, 0)

  q = (/ 9.9d-1, 7.8d0, 1.1d1, 2.2d-2, 1.96d0, 0d0 /)
  DO i=1,3
  call check_dbl2(ne, 68, i * 2 - 1, ent%m(i), q(i * 2 - 1))
  call check_dbl2(ne, 68, i * 2, ent%b(i), q(i * 2))
  end do

! 69: fgd_alter_clincom check
  cq(1) = dcmplx(0.1, 0.2)
  cq(2) = dcmplx(0.3, 0.4)
  cq(3) = dcmplx(0.4, 0.5)
  cq(4) = dcmplx(0.6, 0.7)
  call fgd_alter_clincom(d, 'new3', 2, 'in4', cq(1), cq(2), 'in3', &
     cq(3), cq(4), '', cq(5), cq(6))
  call check_ok2(ne, 69, 1, d)

  n = fgd_entry(d, 'new3', ent)
  call check_ok(ne, 69, d)
  call check_int2(ne, 69, 1, n, GD_LINCOM_ENTRY)
  call check_int2(ne, 69, 2, ent%n_fields, 2)
  call check_int2(ne, 69, 3, ent%fragment_index, 0)
  call check_str2(ne, 69, 4, ent%field(1), 'in4')
  call check_str2(ne, 69, 5, ent%field(2), 'in3')
  call check_int2(ne, 69, 6, ent%comp_scal, 1)

  cq(1) = dcmplx(0.1, 0.2)
  cq(2) = dcmplx(0.3, 0.4)
  cq(3) = dcmplx(0.4, 0.5)
  cq(4) = dcmplx(0.6, 0.7)
  DO i=1,2
  call check_cpx2(ne, 69, i * 2 - 1, ent%cm(i), cq(i * 2 - 1))
  call check_cpx2(ne, 69, i * 2, ent%cb(i), cq(i * 2))
  end do

! 70: fgd_alter_polynom check
  call fgd_alter_polynom(d, 'new4', 4, 'in1', 3d0, 4d0, 5d0, 6d0, 7d0, 0d0)
  call check_ok2(ne, 70, 1, d)

  n = fgd_entry(d, 'new4', ent)
  call check_ok2(ne, 70, 2, d)
  call check_int2(ne, 70, 1, n, GD_POLYNOM_ENTRY)
  call check_int2(ne, 70, 2, ent%poly_ord, 4)
  call check_int2(ne, 70, 3, ent%fragment_index, 0)
  call check_str2(ne, 70, 4, ent%field(1), 'in1')

  q = (/ 3d0, 4d0, 5d0, 6d0, 7d0, 0d0 /)
  DO i=1,5
  call check_dbl2(ne, 70, i, ent%a(i), q(i))
  end do

! 71: fgd_alter_cpolynom check
  cq(1) = dcmplx(1.1, 5.0)
  cq(2) = dcmplx(1.2, 4.0)
  cq(3) = dcmplx(1.2, 3.0)
  cq(4) = dcmplx(1.3, 2.4)
  call fgd_alter_cpolynom(d, 'new5', 3, 'in1', cq(1), cq(2), cq(3), &
  cq(4), dcmplx(0d0,0d0), dcmplx(0d0,0d0))
  call check_ok2(ne, 71, 1, d)

  n = fgd_entry(d, 'new5', ent)
  call check_ok2(ne, 71, 2, d)
  call check_int2(ne, 71, 1, n, GD_POLYNOM_ENTRY)
  call check_int2(ne, 71, 2, ent%poly_ord, 3)
  call check_int2(ne, 71, 3, ent%fragment_index, 0)
  call check_str2(ne, 71, 4, ent%field(1), 'in1')
  call check_int2(ne, 71, 5, ent%comp_scal, 1)

  cq(1) = dcmplx(1.1, 5.0)
  cq(2) = dcmplx(1.2, 4.0)
  cq(3) = dcmplx(1.2, 3.0)
  cq(4) = dcmplx(1.3, 2.4)
  DO 710 i=1,4
  call check_cpx2(ne, 71, i, ent%ca(i), cq(i))
  710 CONTINUE

! 72: fgd_alter_linterp check
  call fgd_alter_linterp(d, "new6", "in3", "./other/table", 0)
  call check_ok2(ne, 72, 1, d)

  n = fgd_entry(d, 'new6', ent)
  call check_ok2(ne, 72, 2, d)
  call check_int2(ne, 72, 1, n, GD_LINTERP_ENTRY)
  call check_int2(ne, 72, 2, ent%fragment_index, 0)
  call check_str2(ne, 72, 3, ent%field(1), 'in3')
  call check_str2(ne, 72, 4, ent%field(2), './other/table')

! 73: fgd_alter_bit check
  call fgd_alter_bit(d, "new7", "in3", 3, 2)
  call check_ok2(ne, 73, 1, d)

  n = fgd_entry(d, 'new7', ent)
  call check_ok2(ne, 73, 2, d)
  call check_int2(ne, 73, 1, n, GD_BIT_ENTRY)
  call check_int2(ne, 73, 2, ent%fragment_index, 0)
  call check_int2(ne, 73, 3, ent%numbits, 2)
  call check_int2(ne, 73, 4, ent%bitnum, 3)
  call check_str2(ne, 73, 5, ent%field(1), 'in3')

! 74: fgd_alter_sbit check
  call fgd_alter_sbit(d, "new8", "out", 1, 22)
  call check_ok2(ne, 74, 1, d)

  n = fgd_entry(d, 'new8', ent)
  call check_ok2(ne, 74, 2, d)
  call check_int2(ne, 74, 1, n, GD_SBIT_ENTRY)
  call check_int2(ne, 74, 2, ent%fragment_index, 0)
  call check_int2(ne, 74, 3, ent%numbits, 22)
  call check_int2(ne, 74, 4, ent%bitnum, 1)
  call check_str2(ne, 74, 5, ent%field(1), 'out')

! 75: fgd_alter_multiply check
  call fgd_alter_multiply(d, 'new9', 'in6', 'in4')
  call check_ok2(ne, 75, 1, d)

  n = fgd_entry(d, 'new9', ent)
  call check_ok2(ne, 75, 2, d)
  call check_int2(ne, 75, 1, n, GD_MULTIPLY_ENTRY)
  call check_int2(ne, 75, 2, ent%fragment_index, 0)
  call check_str2(ne, 75, 3, ent%field(1), 'in6')
  call check_str2(ne, 75, 4, ent%field(2), 'in4')

! 76: fgd_alter_phase check
  call fgd_alter_phase(d, 'new10', 'in2', 8)
  call check_ok2(ne, 76, 1, d)

  n = fgd_entry(d, 'new10', ent)
  call check_ok2(ne, 76, 2, d)
  call check_int2(ne, 76, 1, n, GD_PHASE_ENTRY)
  call check_int2(ne, 76, 2, ent%fragment_index, 0)
  call check_int2(ne, 76, 3, ent%shift, 8)
  call check_str2(ne, 76, 4, ent%field(1), 'in2')

! 77: fgd_alter_const check
  call fgd_alter_const(d, 'new11', GD_FLOAT32)
  call check_ok2(ne, 77, 1, d)

  n = fgd_entry(d, 'new11', ent)
  call check_ok2(ne, 77, 2, d)
  call check_int2(ne, 77, 1, n, GD_CONST_ENTRY)
  call check_int2(ne, 77, 2, ent%fragment_index, 0)
  call check_int2(ne, 77, 3, ent%data_type, GD_FLOAT32)

  call fgd_get_constant_r4(d, 'new11', fl)
  call check_ok2(ne, 77, 3, d)
  call check_dbl(ne, 77, 1d0 * fl, -8.1d0)

! 78: fgd_encoding check
  n = fgd_encoding(d, 0)
  call check_ok(ne, 78, d)
  call check_int(ne, 78, n, GD_UNENCODED)

! 79: fgd_endianness check
  n = fgd_endianness(d, 0)
  call check_ok(ne, 79, d)
  call check_int(ne, 79, n, (GD_LITTLE_ENDIAN + GD_NOT_ARM_ENDIAN))

! 80: fgd_dirfilename check
  l = GD_FIELD_LEN
  call fgd_dirfilename(str, l, d, 0)
  call check_ok(ne, 80, d)
  call check_int(ne, 80, l, GD_FIELD_LEN)
  call check_str(ne, 80, str, 'test95_dirfile')

! 81: fgd_parent_fragment check
  n = fgd_parent_fragment(d, 1)
  call check_ok(ne, 81, d)
  call check_int(ne, 81, n, 0)

! 82: fgd_alter_protection check
  call fgd_alter_protection(d, GD_PROTECT_DATA, 1)
  call check_ok(ne, 82, d)

! 83: fgd_protection check
  n = fgd_protection(d, 1)
  call check_ok(ne, 83, d)
  call check_int(ne, 83, n, GD_PROTECT_DATA)

! 84: fgd_raw_filename check
  str = fgd_raw_filename(d, "data")
  call check_ok(ne, 84, d)
  call check_eos(ne, 84, str, 'test95_dirfile/data')

! 85: fgd_reference check
  str = fgd_reference(d, "new1")
  call check_ok(ne, 85, d)
  call check_str(ne, 85, str, 'new1')

! 87: fgd_alter_encoding check
  call fgd_alter_encoding(d, GD_SLIM_ENCODED, 1, 0)
  call check_ok2(ne, 87, 1, d)

  n = fgd_encoding(d, 1)
  call check_ok2(ne, 87, 2, d)
  call check_int(ne, 87, n, GD_SLIM_ENCODED)

! 88: fgd_alter_endianness check
  call fgd_alter_endianness(d, GD_BIG_ENDIAN, 1, 0)
  call check_ok2(ne, 88, 1, d)

  n = fgd_endianness(d, 1)
  call check_ok2(ne, 88, 2, d)
  call check_int(ne, 88, n, GD_BIG_ENDIAN)

! 89: fgd_alter_spec check
  call fgd_alter_spec(d, 'new10 PHASE in1 3', 0)
  call check_ok2(ne, 89, 1, d)

  n = fgd_entry(d, 'new10', ent)
  call check_ok2(ne, 89, 2, d)
  call check_int2(ne, 89, 1, n, GD_PHASE_ENTRY)
  call check_int2(ne, 89, 2, ent%fragment_index, 0)
  call check_int2(ne, 89, 3, ent%shift, 3)
  call check_str2(ne, 89, 4, ent%field(1), 'in1')

! 90: fgd_delete check
  call fgd_delete(d, 'new10', 0)
  call check_ok2(ne, 90, 1, d)

  n = fgd_entry(d, 'new10', ent)
  call check_err2(ne, 90, 2, d, GD_E_BAD_CODE)

! 91: fgd_malter_spec check
  call fgd_malter_spec(d, 'mnew10 PHASE in4 11', 'data', 0)
  call check_ok2(ne, 91, 1, d)

  n = fgd_entry(d, 'data/mnew10', ent)
  call check_ok2(ne, 91, 2, d)
  call check_int2(ne, 91, 1, n, GD_PHASE_ENTRY)
  call check_int2(ne, 91, 2, ent%fragment_index, 0)
  call check_int2(ne, 91, 3, ent%shift, 11)
  call check_str2(ne, 91, 4, ent%field(1), 'in4')

! 92: fgd_move check
  call fgd_move(d, 'new9', 1, 0)
  call check_ok2(ne, 92, 1, d)

  n = fgd_entry(d, 'new9', ent)
  call check_ok2(ne, 92, 2, d)
  call check_int2(ne, 92, 1, n, GD_MULTIPLY_ENTRY)
  call check_int2(ne, 92, 2, ent%fragment_index, 1)
  call check_str2(ne, 92, 3, ent%field(1), 'in6')
  call check_str2(ne, 92, 4, ent%field(2), 'in4')

! 93: fgd_rename check
  call fgd_rename(d, 'new9', 'newer', 0)
  call check_ok2(ne, 93, 1, d)

  n = fgd_entry(d, 'new9', ent)
  call check_err2(ne, 93, 2, d, GD_E_BAD_CODE)

  n = fgd_entry(d, 'newer', ent)
  call check_ok2(ne, 93, 3, d)
  call check_int2(ne, 93, 1, n, GD_MULTIPLY_ENTRY)
  call check_int2(ne, 92, 2, ent%fragment_index, 1)
  call check_str2(ne, 92, 3, ent%field(1), 'in6')
  call check_str2(ne, 92, 4, ent%field(2), 'in4')

! 94: fgd_uninclude check
  call fgd_uninclude(d, 1, 0)
  call check_ok2(ne, 94, 1, d)

  n = fgd_entry(d, 'newer', ent)
  call check_err2(ne, 94, 2, d, GD_E_BAD_CODE)

! 95: fgd_frameoffset check
  n = fgd_frameoffset(d, 0)
  call check_ok(ne, 95, d)
  call check_int(ne, 95, n, 0)

! 96: fgd_alter_frameoffset check
  call fgd_alter_frameoffset(d, 33, 0, 0)
  call check_ok2(ne, 96, 1, d)

  n = fgd_frameoffset(d, 0)
  call check_ok2(ne, 96, 2, d)
  call check_int(ne, 96, n, 33)

! 97: fgd_native_type check
  n = fgd_native_type(d, 'data')
  call check_ok(ne, 97, d)
  call check_int(ne, 97, n, GD_INT8)

! 99: fgd_validate check
  n = fgd_validate(d, 'new7')
  call check_err(ne, 99, d, GD_E_BAD_CODE)
  call check_int(ne, 99, n, -1)

! 100: fgd_framenum check
  str = fgd_reference(d, "data")
  dp = fgd_framenum(d, 'INDEX', 33.3d0)
  call check_ok(ne, 100, d)
  call check_dbl(ne, 100, dp, 33.3d0)

! 101: fgd_framenum_subset check
  dp = fgd_framenum_subset(d, 'data', 33.3d0, 6, 0)
  call check_ok(ne, 101, d)
  call check_dbl(ne, 101, dp, 37.0375d0)

! 86: fgd_eof check
  n = fgd_eof(d, 'lincom')
  call check_ok(ne, 86, d)
  call check_int(ne, 86, n, 344)

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
  call check_int2(ne, 145, 3, ent%comp_scal, 1)
  call check_str2(ne, 145, 4, ent%field(1), 'div')
  call check_cpx2(ne, 145, 5, ent%cdividend, dcmplx(6.5, 4.3))

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
  call check_int2(ne, 147, 4, ent%comp_scal, 0)
  call check_dbl2(ne, 147, 5, ent%dividend, 31.9d0)

! 148: fgd_add_recip check
  call fgd_add_crecip(d, 'new16', 'in1', dcmplx(31.9d0, 38.2d0), 0)
  call check_ok2(ne, 148, 1, d)

  n = fgd_entry(d, 'new16', ent)
  call check_ok2(ne, 148, 2, d)
  call check_int2(ne, 148, 1, n, GD_RECIP_ENTRY)
  call check_int2(ne, 148, 2, ent%fragment_index, 0)
  call check_str2(ne, 148, 3, ent%field(1), 'in1')
  call check_int2(ne, 148, 4, ent%comp_scal, 1)
  call check_cpx2(ne, 148, 5, ent%cdividend, dcmplx(31.9, 38.2))

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
  call check_int2(ne, 150, 4, ent%comp_scal, 0)
  call check_dbl2(ne, 150, 5, ent%dividend, 95.5d0)

! 151: fgd_madd_recip check
  call fgd_madd_crecip(d, 'data', 'new16', 'in3', dcmplx(8.47d0, 6.22d0))
  call check_ok2(ne, 151, 1, d)

  n = fgd_entry(d, 'data/new16', ent)
  call check_ok2(ne, 151, 2, d)
  call check_int2(ne, 151, 1, n, GD_RECIP_ENTRY)
  call check_int2(ne, 151, 2, ent%fragment_index, 0)
  call check_str2(ne, 151, 3, ent%field(1), 'in3')
  call check_int2(ne, 151, 4, ent%comp_scal, 1)
  call check_cpx2(ne, 151, 5, ent%cdividend, dcmplx(8.47, 6.22))

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
  m = fgd_invalid_dirfile()
  call check_ok2(ne, 98, 1, m)

  n = fgd_nfragments(m)
  call check_err2(ne, 98, 2, m, GD_E_BAD_DIRFILE)

  call fgd_close(m)

! 157: fgd_dirfile_standards
  n = fgd_dirfile_standards(d, GD_VERSION_CURRENT)
  call check_ok2(ne, 157, 1, d)
  call check_int(ne, 157, n, 9)

  n = fgd_dirfile_standards(d, 0)
  call check_err2(ne, 157, 2, d, GD_E_BAD_VERSION)

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
    call check_int2(ne, 162, i, ci8(i), i + 2)
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
  call check_cpx2(ne, 165, i, 1d0 * cc8(i), dcmplx((2 + i) * 1.1, 0))
  end do

! 166: gd_get_carray_slice (COMPLEX128)
  call fgd_get_carray_c16(d, "carray", 3, 2, cc16)
  call check_ok(ne, 166, d)

  do i=1,2
  call check_cpx2(ne, 166, i, cc16(i), dcmplx((2 + i) * 1.1, 0))
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

! 177: gd_carray_len
  n = fgd_carray_len(d, 'carray')
  call check_ok(ne, 177, d)
  call check_int(ne, 177, n, 6)

! 178: gd_entry (CARRAY)
  n = fgd_entry(d, 'carray', ent)
  call check_ok(ne, 178, d)
  call check_int2(ne, 178, 1, n, GD_CARRAY_ENTRY)
  call check_int2(ne, 178, 2, ent%fragment_index, 0)
  call check_int2(ne, 178, 2, ent%array_len, 6)
  call check_int2(ne, 178, 3, ent%data_type, GD_FLOAT64)

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
  call check_dbl2(ne, 179, i, 1d0 * cr4(i), 0.)
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
  call check_dbl2(ne, 180, i, 1d0 * cr4(i), 0.)
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

  call fgd_get_carray_r4(d, 'new17', 0, 0, cr4)
  call check_ok2(ne, 181, 3, d)

  do i=1,4
  call check_dbl2(ne, 181, i, 1d0 * cr4(i), 0.)
  end do

! 183: fgd_constants_i1 check
  iq(1) = -123
  iq(2) = -8
  n = fgd_nfields_by_type(d, GD_CONST_ENTRY)
  call fgd_constants_i1(ci1, d)
  call check_ok(ne, 183, d)

  do i = 1, n
  call check_int2(ne, 183, i, int(ci1(i)), iq(i))
  end do

! 184: fgd_constants_i2 check
  iq(1) = 133
  call fgd_constants_i2(ci2, d)
  call check_ok(ne, 184, d)

  do i = 1, n
  call check_int2(ne, 184, i, int(ci2(i)), iq(i))
  end do

! 185: fgd_constants_i4 check
  call fgd_constants_i4(ci4, d)
  call check_ok(ne, 185, d)

  do i = 1, n
  call check_int2(ne, 185, i, ci4(i), iq(i))
  end do

! 186: fgd_constants_i8 check
  call fgd_constants_i8(ci8, d)
  call check_ok(ne, 186, d)

  do i = 1, n
  call check_int2(ne, 186, i, ci8(i), iq(i))
  end do

! 187: fgd_constants_r4 check
  q(1) = 133.
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
  cq(1) = 133.
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
  call check_int2(ne, 191, i, int(ci1(i)), iq(i))
  end do

! 192: fgd_mconstants_i2 check
  call fgd_mconstants_i2(ci2, d, 'data')
  call check_ok(ne, 192, d)

  do i = 1, n
  call check_int2(ne, 192, i, int(ci2(i)), iq(i))
  end do

! 193: fgd_mconstants_i4 check
  call fgd_mconstants_i4(ci4, d, 'data')
  call check_ok(ne, 193, d)

  do i = 1, n
  call check_int2(ne, 193, i, ci4(i), iq(i))
  end do

! 194: fgd_mconstants_i8 check
  call fgd_mconstants_i8(ci8, d, 'data')
  call check_ok(ne, 194, d)

  do i = 1, n
  call check_int2(ne, 194, i, ci8(i), iq(i))
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

  m = fgd_getdata_r8(d, 'data', GD_HERE, 0, 1, 0, cr8)
  call check_int2(ne, 203, 2, m, 8)

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
  fields(3) = 'data/mnew20'
  fields(4) = 'new20'
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

! 224: GDMOVA check
  call fgd_move_alias(d, 'new20', 1)
  call check_ok2(ne, 224, 1, d)

  n = fgd_fragment_index(d, 'Anew20Z')
  call check_ok2(ne, 224, 2, d)
  call check_int(ne, 224, n, 1)

! 225: fgd_delete_alias check
  call fgd_delete_alias(d, 'Anew20Z', 0)
  call check_ok2(ne, 225, 1, d)

  n = fgd_fragment_index(d, 'Anew20Z')
  call check_err2(ne, 225, 2, d, GD_E_BAD_CODE)
  call check_int(ne, 225, n, -1)

! 226: fgd_fragment_affixes check
  call fgd_fragment_affixes(fields(1), fields(2), d, 1)
  call check_ok(ne, 226, d)
  call check_str2(ne, 226, 1, fields(1), 'A')
  call check_str2(ne, 226, 2, fields(2), 'Z')

! 227: fgd_alter_affixes check
  call fgd_alter_affixes(d, 1, 'B', '')
  call check_ok2(ne, 227, 1, d)

  call fgd_fragment_affixes(fields(1), fields(2), d, 1)
  call check_ok2(ne, 227, 2, d)
  call check_str2(ne, 227, 3, fields(1), 'B')
  call check_str2(ne, 227, 3, fields(2), '')

! 228: fgd_entry (MPLEX) check
  n = fgd_entry(d, 'mplex', ent)
  call check_ok(ne, 228, d)
  call check_int2(ne, 228, 1, n, GD_MPLEX_ENTRY)
  call check_int2(ne, 228, 2, ent%fragment_index, 0)
  call check_int2(ne, 228, 3, ent%count_val, 1)
  call check_str2(ne, 228, 4, ent%field(1), 'data')
  call check_str2(ne, 228, 5, ent%field(2), 'sbit')
  call check_int2(ne, 228, 6, ent%count_max, 10)

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
  call check_int2(ne, 229, 6, ent%count_max, 6)

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
  call check_int2(ne, 230, 6, ent%count_max, 12)

! 231: fgd_alter_mplex check
  call fgd_alter_mplex(d, 'new21', 'in3', 'in4', GD_COUNT_MAX, 7)
  call check_ok2(ne, 231, 1, d)

  n = fgd_entry(d, 'new21', ent)
  call check_ok2(ne, 231, 2, d)
  call check_int2(ne, 231, 1, n, GD_MPLEX_ENTRY)
  call check_int2(ne, 231, 2, ent%fragment_index, 0)
  call check_int2(ne, 231, 3, ent%count_val, 5)
  call check_str2(ne, 231, 4, ent%field(1), 'in3')
  call check_str2(ne, 231, 5, ent%field(2), 'in4')
  call check_dbl2(ne, 231, 6, ent%count_max, 7)

 


  


!================================================================
  call fgd_discard(d)

  call system ( 'rm -rf ' // fildir )

  if (ne .GT. 0) then
    write(*, 9000) ne
    call exit(1)
  end if

9000 format('ne = ', i0)
end program
