! Copyright (C) 2009-2010 D. V. Wiebe
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

program big_test
  use getdata
  character (len=*), parameter :: fildir = 'test95_dirfile'
  character (len=*), parameter :: frmat = 'test95_dirfile/format'
  character (len=*), parameter :: frm2 = 'test95_dirfile/form2'
  character (len=*), parameter :: dat = 'test95_dirfile/data'
  integer, parameter :: flen = 7
  integer, parameter :: nfields = 14
  integer, parameter :: slen = 26

  character (len=slen), dimension(3) :: strings
  character (len=slen), dimension(3) :: st
  character (len=flen), dimension(nfields + 8) :: fields
  character (len=flen), dimension(nfields + 8) :: flist
  character (len=GD_FIELD_LEN) :: str
  integer(1), dimension(80) :: datadata
  integer :: i, d, e, n, l, ne
  real :: fl
  double precision :: dp
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

  fields =(/ 'INDEX  ', 'bit    ', 'carray ', 'const  ', 'data   ', 'div    ', &
  'lincom ', 'linterp', 'mult   ', 'phase  ', 'polynom', 'recip  ', 'sbit   ', &
  'string ', '       ', '       ', '       ', '       ', '       ', '       ', &
  '       ', '       ' /)

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
  write(1, *) 'mult MULTIPLY data sbit'
  write(1, *) 'phase PHASE data 11'
  write(1, *) 'div DIVIDE mult bit'
  write(1, *) 'recip RECIP div 6.5;4.3'
  write(1, *) 'string STRING "Zaphod Beeblebrox"'
  close(1, status='keep')

  open(1, file=frm2, status='new')
  write(1, *) 'const2 CONST INT8 -19'
  close(1, status='keep')

  open(1, file=dat, form='unformatted', access='direct', recl=80, &
  status='new')
  write(1, rec=1) datadata
  close(1, status='keep')

! fgd_error check
  d = fgd_open('x', GD_RDONLY)
  e = fgd_error(d)

  ne = 0
  if (e .ne. GD_E_OPEN) then
    ne = ne + 1
    write(*, 9001), 0, e
  end if

! 1: fgd_open check
  d = fgd_open(fildir, GD_RDWR)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 1, e
  end if

! 2: fgd_getdata_i1 check
  n = fgd_getdata_i1(d, 'data', 5, 0, 1, 0, ci1)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 2, e
  end if

  if (n .ne. 8) then
    ne = ne + 1
    write(*, 9002) 2, n
  end if

  do i=1,8
  if (ci1(i) .ne. 40 + i) then
    ne = ne + 1
    write(*, 9004) i, 2, ci1(i)
  end if
  end do 

! 102: fgd_getdata_i2 check
  n = fgd_getdata_i2(d, 'data', 5, 0, 1, 0, ci2)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 102, e
  end if

  if (n .ne. 8) then
    ne = ne + 1
    write(*, 9002) 102, n
  end if

  do i=1,8
  if (ci2(i) .ne. 40 + i) then
    ne = ne + 1
    write(*, 9004) i, 102, ci2(i)
  end if
  end do 

! 103: fgd_getdata_i4 check
  n = fgd_getdata_i4(d, 'data', 5, 0, 1, 0, ci4)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 103, e
  end if

  if (n .ne. 8) then
    ne = ne + 1
    write(*, 9002) 103, n
  end if

  do i=1,8
  if (ci4(i) .ne. 40 + i) then
    ne = ne + 1
    write(*, 9004) i, 103, ci4(i)
  end if
  end do 

! 104: fgd_getdata_i8 check
  n = fgd_getdata_i8(d, 'data', 5, 0, 1, 0, ci8)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 104, e
  end if

  if (n .ne. 8) then
    ne = ne + 1
    write(*, 9002) 104, n
  end if

  do i=1,8
  if (ci8(i) .ne. 40 + i) then
    ne = ne + 1
    write(*, 9004) i, 104, ci8(i)
  end if
  end do 

! 105: fgd_getdata_r4 check
  n = fgd_getdata_r4(d, 'data', 5, 0, 1, 0, cr4)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 105, e
  end if

  if (n .ne. 8) then
    ne = ne + 1
    write(*, 9002) 105, n
  end if

  do i=1,8
  if (abs(cr4(i) - 40 - i) .gt. 1e-5) then
    ne = ne + 1
    write(*, 9010) i, 105, cr4(i)
  end if
  end do 

! 106: fgd_getdata_r8 check
  n = fgd_getdata_r8(d, 'data', 5, 0, 1, 0, cr8)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 106, e
  end if

  if (n .ne. 8) then
    ne = ne + 1
    write(*, 9002) 106, n
  end if

  do i=1,8
  if (abs(cr8(i) - 40 - i) .gt. 1e-5) then
    ne = ne + 1
    write(*, 9010) i, 106, cr8(i)
  end if
  end do 

! 107: fgd_getdata_c8 check
  n = fgd_getdata_c8(d, 'data', 5, 0, 1, 0, cc8)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 107, e
  end if

  if (n .ne. 8) then
    ne = ne + 1
    write(*, 9002) 107, n
  end if

  do i=1,8
  if (abs(cc8(i) - 40 - i) .gt. 1e-5) then
    ne = ne + 1
    write(*, 9011) i, 107, real(real(cc8(i))), real(aimag(cc8(i)))
  end if
  end do 

! 108: fgd_getdata_c16 check
  n = fgd_getdata_c16(d, 'data', 5, 0, 1, 0, cc16)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 108, e
  end if

  if (n .ne. 8) then
    ne = ne + 1
    write(*, 9002) 108, n
  end if

  do i=1,8
  if (abs(cc16(i) - 40 - i) .gt. 1e-5) then
    ne = ne + 1
    write(*, 9011) i, 107, real(real(cc16(i))), real(aimag(cc16(i)))
  end if
  end do 

! 109: fgd_getdata_n check
  n = fgd_getdata_n(d, 'data', 5, 0, 1, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 109, e
  end if

  if (n .ne. 8) then
    ne = ne + 1
    write(*, 9002) 109, n
  end if

! 3: fgd_get_constant_i1 check
  call fgd_get_constant_i1(d, 'const', ci1(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 3, e
  end if

  if (ci1(1) .ne. 5) then
    ne = ne + 1
    write(*, 9002) 3, ci1(1)
  end if

! 110: fgd_get_constant_i2 check
  call fgd_get_constant_i2(d, 'const', ci2(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 110, e
  end if

  if (ci2(1) .ne. 5) then
    ne = ne + 1
    write(*, 9002) 110, ci2(1)
  end if

! 111: fgd_get_constant_i4 check
  call fgd_get_constant_i4(d, 'const', ci4(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 111, e
  end if

  if (ci4(1) .ne. 5) then
    ne = ne + 1
    write(*, 9002) 111, ci4(1)
  end if

! 112: fgd_get_constant_i8 check
  call fgd_get_constant_i8(d, 'const', ci8(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 112, e
  end if

  if (ci8(1) .ne. 5) then
    ne = ne + 1
    write(*, 9002) 112, ci8(1)
  end if

! 113: fgd_get_constant_r4 check
  call fgd_get_constant_r4(d, 'const', cr4(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 113, e
  end if

  if (abs(cr4(1) - 5.5) .gt. 1e-5) then
    ne = ne + 1
    write(*, 9005) 113, cr4(1)
  end if

! 114: fgd_get_constant_r8 check
  call fgd_get_constant_r8(d, 'const', cr8(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 114, e
  end if

  if (abs(cr8(1) - 5.5) .gt. 1e-5) then
    ne = ne + 1
    write(*, 9012) 114, cr8(1)
  end if

! 115: fgd_get_constant_c8 check
  call fgd_get_constant_c8(d, 'const', cc8(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 115, e
  end if

  if (abs(cc8(1) - 5.5) .gt. 1e-5) then
    ne = ne + 1
    write(*, 9013) 115, real(real(cc8(i))), real(aimag(cc8(i)))
  end if

! 116: fgd_get_constant_c16 check
  call fgd_get_constant_c16(d, 'const', cc16(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 116, e
  end if

  if (abs(cc16(1) - 5.5) .gt. 1e-5) then
    ne = ne + 1
    write(*, 9013) 116, real(real(cc16(i))), real(aimag(cc16(i)))
  end if

! 117: fgd_get_constant_n check
  call fgd_get_constant_n(d, 'const')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 117, e
  end if

! 4: fgd_field_name_max check
  i = fgd_field_name_max(d)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 4, e
  end if

  if (i .ne. flen) then
    ne = ne + 1
    write(*, 9002) 4, i
  end if

! 5: fgd_mfield_name_max check
  i = fgd_mfield_name_max(d, 'data')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 5, e
  end if

  if (i .ne. 6) then
    ne = ne + 1
    write(*, 9002) 5, i
  end if

! 6: fgd_nfields check
  n = fgd_nfields(d)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 6, e
  end if

  if (n .ne. nfields) then
    ne = ne + 1
    write(*, 9002) 6, n
  end if

! 8: fgd_field_list check
  l = flen
  call fgd_field_list(flist, d, l)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 8, e
  end if

  if (l .ne. flen) then
    ne = ne + 1
    write(*, 9002) 8, l
  end if

  do i = 1, n
  if (flist(i) .ne. fields(i)) then
    ne = ne + 1
    write(*, 9008) i, 8, flist(i)
  end if
  end do

! 9: fgd_nmfields check
  n = fgd_nmfields(d, 'data')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 9, e
  end if

  if (n .ne. 3) then
    ne = ne + 1
    write(*, 9002) 9, n
  end if

! 10: fgd_mfield_list check
  fields(1) = 'mstr'
  fields(2) = 'mconst'
  fields(3) = 'mlut'

  l = flen
  call fgd_mfield_list(flist, d, 'data', l)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 10, i, e
  end if

  if (l .ne. flen) then
    ne = ne + 1
    write(*, 9007) 10, i, l
  end if

  DO i = 1, n
  if (flist(i) .ne. fields(i)) then
    ne = ne + 1
    write(*, 9008) i, 10, flist(i)
  end if
  end do

! 11: fgd_nframes check
  n = fgd_nframes(d)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 11, e
  end if

  if (n .ne. 10) then
    ne = ne + 1
    write(*, 9002) 11, n
  end if

! 12: fgd_spf check
  n = fgd_spf(d, 'data')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 12, e
  end if

  if (n .ne. 8) then
    ne = ne + 1
    write(*, 9002) 12, n
  end if

! 13: fgd_putdata_i1 check
  ci1 = (/ 13_1, 14_1, 15_1, 16_1, 17_1, 18_1, 19_1, 90_1 /)
  n = fgd_putdata_i1(d, 'data', 5, 1, 0, 4, ci1)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 13, e
  end if

  if (n .ne. 4) then
    ne = ne + 1
    write(*, 9002) 13, n
  end if

  n = fgd_getdata_i1(d, 'data', 5, 0, 1, 0, ci1)

  DO i = 1, 8
  if (((i .EQ. 1 .OR. i .GT. 5) .AND. ci1(i) .ne. 40 + i) .OR. &
  (i .GT. 1 .AND. i .LT. 6) .AND. ci1(i) .ne. 11 + i) then
    ne = ne + 1
    write(*, 9004) i, 13, ci1(i)
  end if
  end do

! 118: fgd_putdata_i2 check
  ci2 = (/ 23_2, 24_2, 25_2, 26_2, 27_2, 28_2, 29_2, 30_2 /)
  n = fgd_putdata_i2(d, 'data', 5, 1, 0, 4, ci2)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 118, e
  end if

  if (n .ne. 4) then
    ne = ne + 1
    write(*, 9002) 118, n
  end if

  n = fgd_getdata_i2(d, 'data', 5, 0, 1, 0, ci2)

  DO i = 1, 8
  if (((i .EQ. 1 .OR. i .GT. 5) .AND. ci2(i) .ne. 40 + i) .OR. &
  (i .GT. 1 .AND. i .LT. 6) .AND. ci2(i) .ne. 21 + i) then
    ne = ne + 1
    write(*, 9004) i, 118, ci2(i)
  end if
  end do

! 119: fgd_putdata_i4 check
  ci4 = (/ 33, 34, 35, 36, 37, 38, 39, 40 /)
  n = fgd_putdata_i4(d, 'data', 5, 1, 0, 4, ci4)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 119, e
  end if

  if (n .ne. 4) then
    ne = ne + 1
    write(*, 9002) 119, n
  end if

  n = fgd_getdata_i4(d, 'data', 5, 0, 1, 0, ci4)

  DO i = 1, 8
  if (((i .EQ. 1 .OR. i .GT. 5) .AND. ci4(i) .ne. 40 + i) .OR. &
  (i .GT. 1 .AND. i .LT. 6) .AND. ci4(i) .ne. 31 + i) then
    ne = ne + 1
    write(*, 9004) i, 119, ci4(i)
  end if
  end do

! 120: fgd_putdata_i8 check
  ci8 = (/ 43, 44, 45, 46, 47, 48, 49, 50 /)
  n = fgd_putdata_i8(d, 'data', 5, 1, 0, 4, ci8)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 120, e
  end if

  if (n .ne. 4) then
    ne = ne + 1
    write(*, 9002) 120, n
  end if

  n = fgd_getdata_i8(d, 'data', 5, 0, 1, 0, ci8)

  DO i = 1, 8
  if (((i .EQ. 1 .OR. i .GT. 5) .AND. ci8(i) .ne. 40 + i) .OR. &
  (i .GT. 1 .AND. i .LT. 6) .AND. ci8(i) .ne. 41 + i) then
    ne = ne + 1
    write(*, 9004) i, 120, ci8(i)
  end if
  end do

! 121: fgd_putdata_r4 check
  cr4 = (/ 33, 34, 35, 36, 37, 38, 39, 40 /)
  n = fgd_putdata_r4(d, 'data', 5, 1, 0, 4, cr4)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 121, e
  end if

  if (n .ne. 4) then
    ne = ne + 1
    write(*, 9002) 121, n
  end if

  n = fgd_getdata_r4(d, 'data', 5, 0, 1, 0, cr4)

  DO i = 1, 8
  if (((i .EQ. 1 .OR. i .GT. 5) .AND. abs(cr4(i) - 40 - i) .gt. 1e-5) .OR. &
  (i .GT. 1 .AND. i .LT. 6) .AND. abs(cr4(i) - 31 - i) .gt. 1e-5) then
    ne = ne + 1
    write(*, 9010) i, 121, cr4(i)
  end if
  end do

! 122: fgd_putdata_r8 check
  cr8 = (/ 43, 44, 45, 46, 47, 48, 49, 50 /)
  n = fgd_putdata_r8(d, 'data', 5, 1, 0, 4, cr8)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 122, e
  end if

  if (n .ne. 4) then
    ne = ne + 1
    write(*, 9002) 122, n
  end if

  n = fgd_getdata_r8(d, 'data', 5, 0, 1, 0, cr8)

  DO i = 1, 8
  if (((i .EQ. 1 .OR. i .GT. 5) .AND. abs(cr8(i) - 40 - i) .gt. 1e-5) .OR. &
  (i .GT. 1 .AND. i .LT. 6) .AND. abs(cr8(i) - 41 - i) .gt. 1e-5) then
    ne = ne + 1
    write(*, 9010) i, 122, cr8(i)
  end if
  end do

! 123: fgd_putdata_c8 check
  cc8 = (/ 53, 54, 55, 56, 57, 58, 59, 60 /)
  n = fgd_putdata_c8(d, 'data', 5, 1, 0, 4, cc8)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 123, e
  end if

  if (n .ne. 4) then
    ne = ne + 1
    write(*, 9002) 123, n
  end if

  n = fgd_getdata_c8(d, 'data', 5, 0, 1, 0, cc8)

  DO i = 1, 8
  if (((i .EQ. 1 .OR. i .GT. 5) .AND. abs(cc8(i) - 40 - i) .gt. 1e-5) .OR. &
  (i .GT. 1 .AND. i .LT. 6) .AND. abs(cc8(i) - 51 - i) .gt. 1e-5) then
    ne = ne + 1
    write(*, 9011) i, 123, real(real(cc8(i))), real(aimag(cc8(i)))
  end if
  end do

! 124: fgd_putdata_c16 check
  cc16 = (/ 63, 64, 65, 66, 67, 68, 69, 70 /)
  n = fgd_putdata_c16(d, 'data', 5, 1, 0, 4, cc16)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 124, e
  end if

  if (n .ne. 4) then
    ne = ne + 1
    write(*, 9002) 124, n
  end if

  n = fgd_getdata_c16(d, 'data', 5, 0, 1, 0, cc16)

  DO i = 1, 8
  if (((i .EQ. 1 .OR. i .GT. 5) .AND. abs(cc16(i) - 40 - i) .gt. 1e-5) .OR. &
  (i .GT. 1 .AND. i .LT. 6) .AND. abs(cc16(i) - 61 - i) .gt. 1e-5) then
    ne = ne + 1
    write(*, 9011) i, 124, real(real(cc16(i))), real(aimag(cc16(i)))
  end if
  end do

! 14: fgd_error_string check
  n = fgd_getdata_n(d, 'x', 5, 0, 1, 0)
  call fgd_error_string(d, str, GD_FIELD_LEN)

  if (str .ne. 'Field not found: x') then
    ne = ne + 1
    write(*, 9009) 14, str
  end if

! 15: fgd_entry_type check
  n = fgd_entry_type(d, 'data')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 15, e
  end if

  if (n .ne. GD_RAW_ENTRY) then
    ne = ne + 1
    write(*, 9002) 15, n
  end if

! 16: fgd_entry (raw) check
  n = fgd_entry(d, 'data', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 16, e
  end if

  if (n .ne. GD_RAW_ENTRY) then
    ne = ne + 1
    write(*, 9007) 16, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 16, 2, ent%fragment_index
  end if

  if (ent%spf .ne. 8) then
    ne = ne + 1
    write(*, 9007) 16, 3, ent%spf
  end if

  if (ent%data_type .ne. GD_INT8) then
    ne = ne + 1
    write(*, 9007) 16, 4, ent%data_type
  end if

! 18: fgd_entry (lincom) check
  n = fgd_entry(d, 'lincom', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 18, e
  end if

  if (n .ne. GD_LINCOM_ENTRY) then
    ne = ne + 1
    write(*, 9007) 18, 1, n
  end if

  if (ent%n_fields .ne. 3) then
    ne = ne + 1
    write(*, 9007) 18, 2, ent%n_fields
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 18, 3, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'data') then
    ne = ne + 1
    write(*, 9008) 18, 4, ent%field(1)
  end if

  if (ent%field(2) .ne. 'INDEX') then
    ne = ne + 1
    write(*, 9008) 18, 5, ent%field(2)
  end if

  if (ent%field(3) .ne. 'linterp') then
    ne = ne + 1
    write(*, 9008) 18, 6, ent%field(3)
  end if

  if (ent%comp_scal .ne. 1) then
    ne = ne + 1
    write(*, 9007) 18, 7, ent%comp_scal
  end if

  if (ent%scalar(3) .ne. 'const') then
    ne = ne + 1
    write(*, 9008) 18, 8, ent%scalar(3)
  end if

  if (ent%scalar(6) .ne. 'const') then
    ne = ne + 1
    write(*, 9008) 18, 9, ent%scalar(6)
  end if

  cq(1) = dcmplx(1.1, 0.0)
  cq(2) = dcmplx(2.2, 0.0)
  cq(3) = dcmplx(2.2, 0.0)
  cq(4) = dcmplx(3.3, 4.4)
  cq(5) = dcmplx(5.5, 0.0)
  cq(6) = dcmplx(5.5, 0.0)
  DO i=1,3
  if (abs(ent%cm(i) - cq(i * 2 - 1)) > 0.001) then
    ne = ne + 1
    write(*, 9010) i * 2 - 1, 18, ent%m(i)
  end if
  if (abs(ent%cb(i) - cq(i * 2)) > 0.001) then
    ne = ne + 1
    write(*, 9010) i * 2, 18, ent%b(i)
  end if
  end do

! 20: fgd_entry (polynom) check
  n = fgd_entry(d, 'polynom', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 20, e
  end if

  if (n .ne. GD_POLYNOM_ENTRY) then
    ne = ne + 1
    write(*, 9007) 20, 1, n
  end if

  if (ent%poly_ord .ne. 5) then
    ne = ne + 1
    write(*, 9007) 20, 2, ent%n_fields
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 20, 3, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'data') then
    ne = ne + 1
    write(*, 9008) 20, 4, ent%field(1)
  end if

  cq(1) = dcmplx(1.1, 0.0)
  cq(2) = dcmplx(2.2, 0.0)
  cq(3) = dcmplx(2.2, 0.0)
  cq(4) = dcmplx(3.3, 4.4)
  cq(5) = dcmplx(5.5, 0.0)
  cq(6) = dcmplx(5.5, 0.0)
  DO i=1,6
  if (abs(ent%ca(i) - cq(i)) > 0.001) then
    ne = ne + 1
    write(*, 9011) i, 30, real(real(ent%ca(i))), real(aimag(ent%ca(i)))
  end if
  end do

! 21: fgd_entry (linterp) check
  n = fgd_entry(d, 'linterp', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 21, e
  end if

  if (n .ne. GD_LINTERP_ENTRY) then
    ne = ne + 1
    write(*, 9007) 21, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 21, 2, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'data') then
    ne = ne + 1
    write(*, 9008) 21, 3, ent%field(1)
  end if

  if (ent%field(2) .ne. '/look/up/file') then
    ne = ne + 1
    write(*, 9008) 21, 4, ent%field(2)
  end if

! 22: fgd_entry (bit) check
  n = fgd_entry(d, 'bit', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 22, e
  end if

  if (n .ne. GD_BIT_ENTRY) then
    ne = ne + 1
    write(*, 9007) 22, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 22, 2, ent%fragment_index
  end if

  if (ent%bitnum .ne. 3) then
    ne = ne + 1
    write(*, 9007) 22, 3, ent%bitnum
  end if

  if (ent%numbits .ne. 4) then
    ne = ne + 1
    write(*, 9007) 22, 4, ent%numbits
  end if

  if (ent%field(1) .ne. 'data') then
    ne = ne + 1
    write(*, 9008) 22, 5, ent%field(1)
  end if

! 23: fgd_entry (Sbit) check
  n = fgd_entry(d, 'sbit', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 23, e
  end if

  if (n .ne. GD_SBIT_ENTRY) then
    ne = ne + 1
    write(*, 9007) 23, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 23, 2, ent%fragment_index
  end if

  if (ent%numbits .ne. 6) then
    ne = ne + 1
    write(*, 9007) 23, 3, ent%numbits
  end if

  if (ent%bitnum .ne. 5) then
    ne = ne + 1
    write(*, 9007) 23, 4, ent%bitnum
  end if

  if (ent%field(1) .ne. 'data') then
    ne = ne + 1
    write(*, 9008) 23, 5, ent%field(1)
  end if

! 24: fgd_entry (multiply) check
  n = fgd_entry(d, 'mult', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 24, e
  end if

  if (n .ne. GD_MULTIPLY_ENTRY) then
    ne = ne + 1
    write(*, 9007) 24, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 24, 2, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'data') then
    ne = ne + 1
    write(*, 9008) 24, 3, ent%field(1)
  end if

  if (ent%field(2) .ne. 'sbit') then
    ne = ne + 1
    write(*, 9008) 24, 4, ent%field(2)
  end if

! 25: fgd_entry (phase) check
  n = fgd_entry(d, 'phase', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 25, e
  end if

  if (n .ne. GD_PHASE_ENTRY) then
    ne = ne + 1
    write(*, 9007) 25, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 25, 2, ent%fragment_index
  end if

  if (ent%shift .ne. 11) then
    ne = ne + 1
    write(*, 9007) 25, 3, ent%shift
  end if

  if (ent%field(1) .ne. 'data') then
    ne = ne + 1
    write(*, 9008) 25, 4, ent%field(1)
  end if

! 26: fgd_entry (const) check
  n = fgd_entry(d, 'const', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 26, e
  end if

  if (n .ne. GD_CONST_ENTRY) then
    ne = ne + 1
    write(*, 9007) 26, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 26, 2, ent%fragment_index
  end if

  if (ent%data_type .ne. GD_FLOAT64) then
    ne = ne + 1
    write(*, 9007) 26, 3, ent%data_type
  end if

! 27: fgd_fragment_index check
  n = fgd_fragment_index(d, 'const')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 27, e
  end if

  if (n .ne. 0) then
    ne = ne + 1
    write(*, 9002) 27, n
  end if

! 28: fgd_add_raw check
  call fgd_add_raw(d, 'new1', GD_FLOAT64, 3, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 28, 1, e
  end if

  n = fgd_entry(d, 'new1', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 28, 2, e
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 28, 3, ent%fragment_index
  end if

  if (ent%spf .ne. 3) then
    ne = ne + 1
    write(*, 9007) 28, 4, ent%spf
  end if

  if (ent%data_type .ne. GD_FLOAT64) then
    ne = ne + 1
    write(*, 9007) 28, 5, i
  end if

  if (n .ne. GD_RAW_ENTRY) then
    ne = ne + 1
    write(*, 9007) 28, 6, n
  end if

! 29: fgd_add_lincom check
  call fgd_add_lincom(d, 'new2', 2, 'in1', 9.9d0, 8.8d0, &
  'in2', 7.7d0, 6.6d0, '', 0d0, 0d0, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 29, 1, e
  end if

  n = fgd_entry(d, 'new2', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 29, 2, e
  end if

  if (n .ne. GD_LINCOM_ENTRY) then
    ne = ne + 1
    write(*, 9007) 29, 3, n
  end if

  if (ent%n_fields .ne. 2) then
    ne = ne + 1
    write(*, 9007) 29, 4, ent%n_fields
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 29, 5, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 9008) 29, 6, ent%field(1)
  end if

  if (ent%field(2) .ne. 'in2') then
    ne = ne + 1
    write(*, 9008) 29, 7, ent%field(2)
  end if

  if (ent%comp_scal .ne. 0) then
    ne = ne + 1
    write(*, 9007) 29, 8, ent%comp_scal
  end if

  q = (/ 9.9, 8.8, 7.7, 6.6, 5.5, 5.5 /)
  do i=1,2
  if (abs(ent%m(i) - q(i * 2 - 1)) > 0.001) then
    ne = ne + 1
    write(*, 9010) i * 2 - 1, 29, ent%m(i)
  end if
  if (abs(ent%b(i) - q(i * 2)) > 0.001) then
    ne = ne + 1
    write(*, 9010) i * 2, 29, ent%b(i)
  end if
  end do

! 30: fgd_add_clincom check
  cq(1) = dcmplx(1.1, 1.2)
  cq(2) = dcmplx(1.3, 1.4)
  cq(3) = dcmplx(1.4, 1.5)
  cq(4) = dcmplx(1.6, 1.7)
  call fgd_add_clincom(d, 'new3', 2, 'in1', cq(1), cq(2), &
  'in2', cq(3), cq(4), '', cq(5), cq(6), 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 30, 1, e
  end if

  n = fgd_entry(d, 'new3', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 30, 2, e
  end if

  if (ent%n_fields .ne. 2) then
    ne = ne + 1
    write(*, 9007) 30, 1, ent%n_fields
  end if

  if (n .ne. GD_LINCOM_ENTRY) then
    ne = ne + 1
    write(*, 9007) 30, 2, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 30, 3, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 9008) 30, 4, ent%field(1)
  end if

  if (ent%field(2) .ne. 'in2') then
    ne = ne + 1
    write(*, 9008) 30, 5, ent%field(2)
  end if

  if (ent%comp_scal .ne. 1) then
    ne = ne + 1
    write(*, 9007) 30, 6, ent%comp_scal
  end if

  cq(1) = dcmplx(1.1, 1.2)
  cq(2) = dcmplx(1.3, 1.4)
  cq(3) = dcmplx(1.4, 1.5)
  cq(4) = dcmplx(1.6, 1.7)
  do i=1,2
  if (abs(ent%cm(i) - cq(i * 2 - 1)) > 0.001) then
    ne = ne + 1
    write(*, 9011) i * 2 - 1, 30, real(real(ent%cm(i))), real(aimag(ent%cm(i)))
  end if
  if (abs(ent%cb(i) - cq(i * 2)) > 0.001) then
    ne = ne + 1
    write(*, 9011) i * 2, 30, real(real(ent%cb(i))), real(aimag(ent%cb(i)))
  end if
  end do

! 31: fgd_add_polynom check
  call fgd_add_polynom(d, 'new4', 3, 'in1', 3d3, 4d4, 5d5, 6d6, 0d0, 0d0, &
  0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 31, 1, e
  end if

  n = fgd_entry(d, 'new4', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 31, 2, e
  end if

  if (n .ne. GD_POLYNOM_ENTRY) then
    ne = ne + 1
    write(*, 9007) 31, 1, n
  end if

  if (ent%poly_ord .ne. 3) then
    ne = ne + 1
    write(*, 9007) 31, 2, ent%poly_ord
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 31, 3, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 9008) 31, 4, ent%field(1)
  end if

  if (ent%comp_scal .ne. 0) then
    ne = ne + 1
    write(*, 9007) 31, 5, ent%comp_scal
  end if

  q = (/ 3d3, 4d4, 5d5, 6d6, 5.5d0, 5.5d0 /)
  DO i=1,4
  if (abs(ent%a(i) - q(i)) > 0.001) then
    ne = ne + 1
    write(*, 9010) i, 31, ent%a(i)
  end if
  end do

! 32: fgd_add_cpolynom check
  cq(1) = dcmplx(3.1, 7.0)
  cq(2) = dcmplx(4.2, 8.0)
  cq(3) = dcmplx(5.2, 9.0)
  cq(4) = dcmplx(6.3, 4.4)
  call fgd_add_cpolynom(d, 'new5', 3, 'in1', cq(1), cq(2), cq(3), cq(4), &
  dcmplx(0d0,0d0), dcmplx(0d0,0d0), 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 32, 1, e
  end if

  n = fgd_entry(d, 'new5', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 32, 2, e
  end if

  if (n .ne. GD_POLYNOM_ENTRY) then
    ne = ne + 1
    write(*, 9007) 32, 1, n
  end if

  if (ent%poly_ord .ne. 3) then
    ne = ne + 1
    write(*, 9007) 32, 2, ent%poly_ord
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 32, 3, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 9008) 32, 4, ent%field(1)
  end if

  if (ent%comp_scal .ne. 1) then
    ne = ne + 1
    write(*, 9007) 31, 5, ent%comp_scal
  end if

  cq(1) = dcmplx(3.1, 7.0)
  cq(2) = dcmplx(4.2, 8.0)
  cq(3) = dcmplx(5.2, 9.0)
  cq(4) = dcmplx(6.3, 4.4)
  DO i=1,4
  if (abs(ent%ca(i) - cq(i)) > 0.001) then
    ne = ne + 1
    write(*, 9011) i, 32, real(real(ent%ca(i))), real(aimag(ent%ca(i)))
  end if
  end do

! 33: fgd_add_linterp check
  call fgd_add_linterp(d, "new6", "in", "./some/table", 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 33, 1, e
  end if

  n = fgd_entry(d, 'new6', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 33, 2, e
  end if

  if (n .ne. GD_LINTERP_ENTRY) then
    ne = ne + 1
    write(*, 9007) 33, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 33, 2, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in') then
    ne = ne + 1
    write(*, 9008) 33, 3, ent%field(1)
  end if

  if (ent%field(2) .ne. './some/table') then
    ne = ne + 1
    write(*, 9008) 33, 4, ent%field(2)
  end if

! 34: fgd_add_bit check
  call fgd_add_bit(d, "new7", "in", 13, 12, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 34, 1, e
  end if

  n = fgd_entry(d, 'new7', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 34, 2, e
  end if

  if (n .ne. GD_BIT_ENTRY) then
    ne = ne + 1
    write(*, 9007) 34, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 34, 2, ent%fragment_index
  end if

  if (ent%numbits .ne. 12) then
    ne = ne + 1
    write(*, 9007) 34, 3, ent%numbits
  end if

  if (ent%bitnum .ne. 13) then
    ne = ne + 1
    write(*, 9007) 34, 4, ent%bitnum
  end if

  if (ent%field(1) .ne. 'in') then
    ne = ne + 1
    write(*, 9008) 34, 5, ent%field(1)
  end if

! 35: fgd_add_sbit check
  call fgd_add_sbit(d, "new8", "in", 13, 12, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 35, 1, e
  end if

  n = fgd_entry(d, "new8", ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 35, 2, e
  end if

  if (n .ne. GD_SBIT_ENTRY) then
    ne = ne + 1
    write(*, 9007) 35, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 35, 2, ent%fragment_index
  end if

  if (ent%numbits .ne. 12) then
    ne = ne + 1
    write(*, 9007) 35, 3, ent%numbits
  end if

  if (ent%bitnum .ne. 13) then
    ne = ne + 1
    write(*, 9007) 35, 4, ent%bitnum
  end if

  if (ent%field(1) .ne. 'in') then
    ne = ne + 1
    write(*, 9008) 35, 5, ent%field(1)
  end if

! 36: fgd_add_multiply check
  call fgd_add_multiply(d, 'new9', 'in1', 'in2', 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 36, 1, e
  end if

  n = fgd_entry(d, 'new9', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 36, 2, e
  end if

  if (n .ne. GD_MULTIPLY_ENTRY) then
    ne = ne + 1
    write(*, 9007) 36, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 36, 2, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 9008) 36, 3, ent%field(1)
  end if

  if (ent%field(2) .ne. 'in2') then
    ne = ne + 1
    write(*, 9008) 36, 4, ent%field(2)
  end if

! 37: fgd_add_phase check
  call fgd_add_phase(d, 'new10', 'in1', 22, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 37, 1, e
  end if

  n = fgd_entry(d, 'new10', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 37, 2, e
  end if

  if (n .ne. GD_PHASE_ENTRY) then
    ne = ne + 1
    write(*, 9007) 37, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 37, 2, ent%fragment_index
  end if

  if (ent%shift .ne. 22) then
    ne = ne + 1
    write(*, 9007) 37, 3, ent%shift
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 9008) 37, 4, ent%field(1)
  end if

! 38: fgd_add_const check
  call fgd_add_const(d, 'new11', GD_FLOAT64, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 38, 1, e
  end if

  n = fgd_entry(d, 'new11', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 38, 2, e
  end if

  if (n .ne. GD_CONST_ENTRY) then
    ne = ne + 1
    write(*, 9007) 38, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 38, 2, ent%fragment_index
  end if

  if (ent%data_type .ne. GD_FLOAT64) then
    ne = ne + 1
    write(*, 9007) 38, 3, ent%data_type
  end if

  call fgd_get_constant_r4(d, 'new11', fl)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 38, 3, e
  end if

  if (abs(fl) > 1e-5) then
    ne = ne + 1
    write(*, 9005) 38, fl
  end if

! 125: fgd_add check
  ent%shift = 33
  ent%field(1) = 'new9'
  ent%fragment_index = 0
  ent%field_type = GD_PHASE_ENTRY
  call fgd_add(d, 'new13', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 125, 1, e
  end if

  n = fgd_entry(d, 'new13', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 125, 2, e
  end if

  if (n .ne. GD_PHASE_ENTRY) then
    ne = ne + 1
    write(*, 9007) 125, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 125, 2, ent%fragment_index
  end if

  if (ent%shift .ne. 33) then
    ne = ne + 1
    write(*, 9007) 125, 3, ent%shift
  end if

  if (ent%field(1) .ne. 'new9') then
    ne = ne + 1
    write(*, 9008) 125, 4, ent%field(1)
  end if

! 39: fgd_fragmentname check
  str = fgd_fragmentname(d, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 39, e
  end if

  if (str .ne. 'test95_dirfile/format') then
    ne = ne + 1
    write(*, 9009), 39, str
  end if

! 40: fgd_nfragments check
  n = fgd_nfragments(d)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 40, e
  end if

  if (n .ne. 1) then
    ne = ne + 1
    write(*, 9002), 40, n
  end if

! 41: fgd_include check
  call fgd_include(d, 'form2', 0, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 41, 3, e
  end if

  call fgd_get_constant_i1(d, 'const2', ci1(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 41, 3, e
  end if

  if (ci1(1) .ne. -19) then
    ne = ne + 1
    write(*, 9004) 1, 41, ci1(1)
  end if

! 42: fgd_nfields_by_type check
  n = fgd_nfields_by_type(d, GD_LINCOM_ENTRY)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 42, e
  end if

  if (n .ne. 3) then
    ne = ne + 1
    write(*, 9002), 42, n
  end if

! 43: fgd_field_list_by_type check
  fields(1) = 'lincom'
  fields(2) = 'new2'
  fields(3) = 'new3'
  l = flen
  call fgd_field_list_by_type(flist, d, GD_LINCOM_ENTRY, l)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 43, e
  end if

  if (l .ne. flen) then
    ne = ne + 1
    write(*, 9002) 43, l
  end if

  do i = 1, n
  if (flist(i) .ne. fields(i)) then
    ne = ne + 1
    write(*, 9008) i, 43, flist(i)
  end if
  end do

! 44: fgd_nvectors check
  n = fgd_nvectors(d)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 44, e
  end if

  if (n .ne. 22) then
    ne = ne + 1
    write(*, 9002), 44, n
  end if

! 45: fgd_vector_list check
  fields =(/ 'INDEX  ', 'bit    ', 'data   ', 'div    ', 'lincom ', 'linterp', &
  'mult   ', 'new1   ', 'new10  ', 'new13  ', 'new2   ', 'new3   ', 'new4   ', &
  'new5   ', 'new6   ', 'new7   ', 'new8   ', 'new9   ', 'phase  ', 'polynom', &
  'recip  ', 'sbit   ' /)
  l = flen
  call fgd_vector_list(flist, d, l)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 45, e
  end if

  if (l .ne. flen) then
    ne = ne + 1
    write(*, 9002) 45, l
  end if
 
  do i=1,n
  if (flist(i) .ne. fields(i)) then
    ne = ne + 1
    write(*, 9008) i, 45, flist(i)
  end if
  end do

! 46: fgd_madd_lincom check
  call fgd_madd_lincom(d, 'data', 'mnew1', 2, 'in1', 9.9d0, 8.8d0, &
  'in2', 7.7d0, 6.6d0, '', 0d0, 0d0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 46, 1, e
  end if

  n = fgd_entry(d, 'data/mnew1', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 46, 2, e
  end if

  if (n .ne. GD_LINCOM_ENTRY) then
    ne = ne + 1
    write(*, 9007) 46, 3, n
  end if

  if (ent%n_fields .ne. 2) then
    ne = ne + 1
    write(*, 9007) 46, 4, ent%n_fields
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 46, 5, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 9008) 46, 6, ent%field(1)
  end if

  if (ent%field(2) .ne. 'in2') then
    ne = ne + 1
    write(*, 9008) 46, 7, ent%field(2)
  end if

  if (ent%comp_scal .ne. 0) then
    ne = ne + 1
    write(*, 9007) 46, 8, ent%comp_scal
  end if

  q = (/ 9.9, 8.8, 7.7, 6.6, 5.5, 5.5 /)
  DO i=1,2
  if (abs(ent%m(i) - q(i *  2 - 1)) > 0.001) then
    ne = ne + 1
    write(*, 9010) i * 2 - 1, 46, ent%m(i)
  end if
  if (abs(ent%b(i) - q(i * 2)) > 0.001) then
    ne = ne + 1
    write(*, 9010) i * 2, 46, ent%m(i)
  end if
  end do

! 47: fgd_madd_clincom check
  cq(1) = dcmplx(1.1, 1.2)
  cq(2) = dcmplx(1.3, 1.4)
  cq(3) = dcmplx(1.4, 1.5)
  cq(4) = dcmplx(1.6, 1.7)
  call fgd_madd_clincom(d, 'data', 'mnew2', 2, 'in1', cq(1), cq(2), &
  'in2', cq(3), cq(4), '', cq(5), cq(6))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 47, 1, e
  end if

  n = fgd_entry(d, 'data/mnew2', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 47, e
  end if

  if (n .ne. GD_LINCOM_ENTRY) then
    ne = ne + 1
    write(*, 9007) 47, 1, n
  end if

  if (ent%n_fields .ne. 2) then
    ne = ne + 1
    write(*, 9007) 47, 2, ent%n_fields
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 47, 3, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 9008) 47, 4, ent%field(1)
  end if

  if (ent%field(2) .ne. 'in2') then
    ne = ne + 1
    write(*, 9008) 47, 5, ent%field(2)
  end if

  if (ent%comp_scal .ne. 1) then
    ne = ne + 1
    write(*, 9007) 47, 6, ent%comp_scal
  end if

  cq(1) = dcmplx(1.1, 1.2)
  cq(2) = dcmplx(1.3, 1.4)
  cq(3) = dcmplx(1.4, 1.5)
  cq(4) = dcmplx(1.6, 1.7)
  DO i=1,2
  if (abs(ent%cm(i) - cq(i * 2 - 1)) > 0.001) then
    ne = ne + 1
    write(*, 9011) i * 2 - 1, 47, real(real(ent%cm(i))), real(aimag(ent%cm(i)))
  end if
  if (abs(ent%cb(i) - cq(i * 2)) > 0.001) then
    ne = ne + 1
    write(*, 9011) i * 2, 47, real(real(ent%cb(i))), real(aimag(ent%cb(i)))
  end if
  end do

! 48: fgd_madd_polynom check
  call fgd_madd_polynom(d, 'data', 'mnew3', 3, 'in1', 3d3, 4d4, 5d5, &
     6d6, 0d0, 0d0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 48, 1, e
  end if

  n = fgd_entry(d, 'data/mnew3', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 48, 2, e
  end if

  if (n .ne. GD_POLYNOM_ENTRY) then
    ne = ne + 1
    write(*, 9007) 48, 1, n
  end if

  if (ent%poly_ord .ne. 3) then
    ne = ne + 1
    write(*, 9007) 48, 2, ent%poly_ord
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 48, 3, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 9008) 48, 4, ent%field(1)
  end if

  q = (/ 3d3, 4d4, 5d5, 6d6, 5.5d0, 5.5d0 /)
  DO i=1,4
  if (abs(ent%a(i) - q(i)) > 0.001) then
    ne = ne + 1
    write(*, 9010) i, 48, ent%a(i)
  end if
  end do

! 49: fgd_madd_cpolynom check
  cq(1) = dcmplx(1.1, 0.0)
  cq(2) = dcmplx(2.2, 0.0)
  cq(3) = dcmplx(2.2, 0.0)
  cq(4) = dcmplx(3.3, 4.4)
  call fgd_madd_cpolynom(d, 'data', 'mnew5', 3, 'in1', cq(1), cq(2), &
     cq(3), cq(4), dcmplx(0d0,0d0), dcmplx(0d0,0d0))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 49, 1, e
  end if

  n = fgd_entry(d, 'data/mnew5', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 49, 2, e
  end if

  if (n .ne. GD_POLYNOM_ENTRY) then
    ne = ne + 1
    write(*, 9007) 49, 1, n
  end if

  if (ent%poly_ord .ne. 3) then
    ne = ne + 1
    write(*, 9007) 49, 2, ent%poly_ord
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 49, 3, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 9008) 49, 4, ent%field(1)
  end if

  cq(1) = dcmplx(1.1, 0.0)
  cq(2) = dcmplx(2.2, 0.0)
  cq(3) = dcmplx(2.2, 0.0)
  cq(4) = dcmplx(3.3, 4.4)
  DO i=1,4
  if (abs(ent%ca(i) - cq(i)) > 0.001) then
    ne = ne + 1
    write(*, 9011) i, 49, real(real(ent%ca(i))), real(aimag(ent%ca(i)))
  end if
  end do

! 50: fgd_madd_linterp check
  call fgd_madd_linterp(d, "data", "mnew6", "in", "./more/table")
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 50, 1, e
  end if

  n = fgd_entry(d, 'data/mnew6', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 50, 2, e
  end if

  if (n .ne. GD_LINTERP_ENTRY) then
    ne = ne + 1
    write(*, 9007) 50, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 50, 2, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in') then
    ne = ne + 1
    write(*, 9008) 50, 3, ent%field(1)
  end if

  if (ent%field(2) .ne. './more/table') then
    ne = ne + 1
    write(*, 9008) 50, 4, ent%field(2)
  end if

! 51: fgd_madd_bit check
  call fgd_madd_bit(d, "data", "mnew7", "in", 13, 12)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 51, 1, e
  end if

  n = fgd_entry(d, 'data/mnew7', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 51, 2, e
  end if

  if (n .ne. GD_BIT_ENTRY) then
    ne = ne + 1
    write(*, 9007) 51, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 51, 2, ent%fragment_index
  end if

  if (ent%numbits .ne. 12) then
    ne = ne + 1
    write(*, 9007) 51, 3, ent%numbits
  end if

  if (ent%bitnum .ne. 13) then
    ne = ne + 1
    write(*, 9007) 51, 4, ent%bitnum
  end if

  if (ent%field(1) .ne. 'in') then
    ne = ne + 1
    write(*, 9008) 51, 5, ent%field(1)
  end if

! 52: fgd_madd_sbit check
  call fgd_madd_sbit(d, "data", "mnew8", "in", 13, 12)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 52, 1, e
  end if

  n = fgd_entry(d, 'data/mnew8', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 52, 2, e
  end if

  if (n .ne. GD_SBIT_ENTRY) then
    ne = ne + 1
    write(*, 9007) 52, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 52, 2, ent%fragment_index
  end if

  if (ent%numbits .ne. 12) then
    ne = ne + 1
    write(*, 9007) 52, 3, ent%numbits
  end if

  if (ent%bitnum .ne. 13) then
    ne = ne + 1
    write(*, 9007) 52, 4, ent%bitnum
  end if

  if (ent%field(1) .ne. 'in') then
    ne = ne + 1
    write(*, 9008) 52, 5, ent%field(1)
  end if

! 53: fgd_madd_multiply check
  call fgd_madd_multiply(d, 'data', 'mnew9', 'in1', 'in2')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 53, 1, e
  end if

  n = fgd_entry(d, 'data/mnew9', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 53, 2, e
  end if

  if (n .ne. GD_MULTIPLY_ENTRY) then
    ne = ne + 1
    write(*, 9007) 53, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 53, 2, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 9008) 53, 3, ent%field(1)
  end if

  if (ent%field(2) .ne. 'in2') then
    ne = ne + 1
    write(*, 9008) 53, 4, ent%field(2)
  end if

! 54: fgd_madd_phase check
  call fgd_madd_phase(d, 'data', 'mnew10', 'in1', 22)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 54, 1, e
  end if

  n = fgd_entry(d, 'data/mnew10', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 54, 2, e
  end if

  if (n .ne. GD_PHASE_ENTRY) then
    ne = ne + 1
    write(*, 9007) 54, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 54, 2, ent%fragment_index
  end if

  if (ent%shift .ne. 22) then
    ne = ne + 1
    write(*, 9007) 54, 3, ent%shift
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 9008) 54, 4, ent%field(1)
  end if

! 55: fgd_madd_const check
  call fgd_madd_const(d, 'data', 'mnew11', GD_FLOAT64)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 55, 1, e
  end if

  n = fgd_entry(d, 'data/mnew11', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 55, 2, e
  end if

  if (n .ne. GD_CONST_ENTRY) then
    ne = ne + 1
    write(*, 9007) 55, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 55, 2, ent%fragment_index
  end if

  if (ent%data_type .ne. GD_FLOAT64) then
    ne = ne + 1
    write(*, 9007) 55, 3, ent%data_type
  end if

  call fgd_get_constant_r4(d, 'data/mnew11', fl)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 55, 3, e
  end if

  if (abs(fl) > 0.001) then
    ne = ne + 1
    write(*, 9005) 55, fl
  end if

! 126: fgd_madd check
  ent%shift = 33
  ent%field(1) = 'data/mnew10'
  ent%fragment_index = 0
  ent%field_type = GD_PHASE_ENTRY
  call fgd_madd(d, 'data', 'mnew4', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 126, 1, e
  end if

  n = fgd_entry(d, 'data/mnew4', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 126, 2, e
  end if

  if (n .ne. GD_PHASE_ENTRY) then
    ne = ne + 1
    write(*, 9007) 126, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 126, 2, ent%fragment_index
  end if

  if (ent%shift .ne. 33) then
    ne = ne + 1
    write(*, 9007) 126, 3, ent%shift
  end if

  if (ent%field(1) .ne. 'data/mnew10') then
    ne = ne + 1
    write(*, 9008) 126, 4, ent%field(1)
  end if

! 56: fgd_get_string check
  n = fgd_get_string(d, 'string', GD_FIELD_LEN, str)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 56, e
  end if

  if (n .ne. 17) then
    ne = ne + 1
    write(*, 9002) 56, n
  end if

  if (str .ne. "Zaphod Beeblebrox") then
    ne = ne + 1
    write(*, 9009) 56, str
  end if

! 57: fgd_add_string check
  call fgd_add_string(d, 'new12', 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 57, 1, e
  end if

  n = fgd_get_string(d, 'new12', GD_FIELD_LEN, str)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 57, 2, e
  end if

  if (str .ne. "") then
    ne = ne + 1
    write(*, 9009) 57, str
  end if

! 58: fgd_madd_string check
  call fgd_madd_string(d, "data", 'mnew12')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 58, 1, e
  end if

  n = fgd_get_string(d, 'data/mnew12', GD_FIELD_LEN, str)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 58, 2, e
  end if

  if (str .ne. "") then
    ne = ne + 1
    write(*, 9009) 58, str
  end if

! 59: fgd_add_spec check
  call fgd_add_spec(d, 'lorem STRING "Lorem ipsum"', 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 59, 1, e
  end if

  n = fgd_get_string(d, 'lorem', GD_FIELD_LEN, str)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 59, 2, e
  end if

  if (str .ne. "Lorem ipsum") then
    ne = ne + 1
    write(*, 9009) 59, str
  end if

! 60: fgd_madd_spec check
  call fgd_madd_spec(d, 'ipsum STRING "dolor sit amet."', 'lorem')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 60, 1, e
  end if

  n = fgd_get_string(d, 'lorem/ipsum', GD_FIELD_LEN, str)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 60, 2, e
  end if

  if (str .ne. "dolor sit amet.") then
    ne = ne + 1
    write(*, 9009) 60, str
  end if

! 61: fgd_put_constant_i1 check
  ci1(1) = 61
  call fgd_put_constant_i1(d, 'const', ci1(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 61, 1, e
  end if

  call fgd_get_constant_r4(d, 'const', fl)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 61, 2, e
  end if

  if (abs(fl - 61) > 0.001) then
    ne = ne + 1
    write(*, 9005) 61, fl
  end if

! 127: fgd_put_constant_i2 check
  ci2(1) = 127
  call fgd_put_constant_i2(d, 'const', ci2(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 127, 1, e
  end if

  call fgd_get_constant_r4(d, 'const', fl)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 127, 2, e
  end if

  if (abs(fl - 127) > 0.001) then
    ne = ne + 1
    write(*, 9005) 127, fl
  end if

! 128: fgd_put_constant_i4 check
  ci4(1) = 128
  call fgd_put_constant_i4(d, 'const', ci4(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 128, 1, e
  end if

  call fgd_get_constant_r4(d, 'const', fl)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 128, 2, e
  end if

  if (abs(fl - 128) > 0.001) then
    ne = ne + 1
    write(*, 9005) 128, fl
  end if

! 129: fgd_put_constant_i8 check
  ci8(1) = 129
  call fgd_put_constant_i8(d, 'const', ci8(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 129, 1, e
  end if

  call fgd_get_constant_r4(d, 'const', fl)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 129, 2, e
  end if

  if (abs(fl - 129) > 0.001) then
    ne = ne + 1
    write(*, 9005) 129, fl
  end if

! 130: fgd_put_constant_r4 check
  cr4(1) = -8.1
  call fgd_put_constant_r4(d, 'new11', cr4(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 130, 1, e
  end if

  call fgd_get_constant_r4(d, 'new11', fl)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 130, 2, e
  end if

  if (abs(fl + 8.1) > 0.001) then
    ne = ne + 1
    write(*, 9005) 130, fl
  end if

! 131: fgd_put_constant_r8 check
  cr8(1) = 131
  call fgd_put_constant_r8(d, 'const', cr8(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 131, 1, e
  end if

  call fgd_get_constant_r4(d, 'const', fl)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 131, 2, e
  end if

  if (abs(fl - 131) > 0.001) then
    ne = ne + 1
    write(*, 9005) 131, fl
  end if

! 132: fgd_put_constant_c8 check
  cc8(1) = 132
  call fgd_put_constant_c8(d, 'const', cc8(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 132, 1, e
  end if

  call fgd_get_constant_r4(d, 'const', fl)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 132, 2, e
  end if

  if (abs(fl - 132) > 0.001) then
    ne = ne + 1
    write(*, 9005) 132, fl
  end if

! 133: fgd_put_constant_c16 check
  cc16(1) = 133
  call fgd_put_constant_c16(d, 'const', cc16(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 133, 1, e
  end if

  call fgd_get_constant_r4(d, 'const', fl)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 133, 2, e
  end if

  if (abs(fl - 133) > 0.001) then
    ne = ne + 1
    write(*, 9005) 133, fl
  end if

! 62: fgd_put_string check
  n = fgd_put_string(d, 'string', "Arthur Dent")
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 62, 1, e
  end if

  if (n .ne. 11) then
    ne = ne + 1
    write(*, 9002) 62, n
  end if

  n = fgd_get_string(d, 'string', GD_FIELD_LEN, str)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 62, 2, e
  end if

  if (str .ne. "Arthur Dent") then
    ne = ne + 1
    write(*, 9009) 62, str
  end if

! 63: fgd_nmfields_by_type check
  n = fgd_nmfields_by_type(d, "data", GD_LINCOM_ENTRY)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 63, e
  end if

  if (n .ne. 2) then
    ne = ne + 1
    write(*, 9002), 63, n
  end if

! 64: fgd_mfield_list_by_type check
  fields(1) = 'mnew1'
  fields(2) = 'mnew2'
  l = flen
  call fgd_mfield_list_by_type(flist, d, "data", GD_LINCOM_ENTRY, l)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 64, i, e
  end if

  if (l .ne. flen) then
    ne = ne + 1
    write(*, 9007) 64, i, l
  end if

  do i = 1, n
  if (flist(i) .ne. fields(i)) then
    ne = ne + 1
    write(*, 9008) i, 64, flist(i)
  end if
  end do

! 65: fgd_nmvectors check
  n = fgd_nmvectors(d, "data")
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 65, e
  end if

  if (n .ne. 11) then
    ne = ne + 1
    write(*, 9002), 65, n
  end if

! 66: fgd_mvector_list check
  fields =(/ 'mlut  ', 'mnew1 ', 'mnew2 ', 'mnew3 ', 'mnew5 ', 'mnew6 ', &
  'mnew7 ', 'mnew8 ', 'mnew9 ', 'mnew10', 'mnew4 ', '      ', '      ', &
  '      ', '      ', '      ', '      ', '      ', '      ', '      ', &
  '      ', '      ' /)
  l = flen
  call fgd_mvector_list(flist, d, "data", l)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 66, i, e
  end if

  if (l .ne. flen) then
    ne = ne + 1
    write(*, 9007) 66, i, l
  end if

  do i=1,n
  if (flist(i) .ne. fields(i)) then
    ne = ne + 1
    write(*, 9008) i, 66, flist(i)
  end if
  end do

! 67: fgd_alter_raw check
  call fgd_alter_raw(d, 'new1', GD_INT32, 4, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 67, 1, e
  end if

  n = fgd_entry(d, 'new1', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 67, 2, e
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 67, 3, ent%fragment_index
  end if

  if (ent%spf .ne. 4) then
    ne = ne + 1
    write(*, 9007) 67, 4, ent%spf
  end if

  if (ent%data_type .ne. GD_INT32) then
    ne = ne + 1
    write(*, 9007) 67, 5, ent%data_type
  end if

  if (n .ne. GD_RAW_ENTRY) then
    ne = ne + 1
    write(*, 9007), 67, 6, n
  end if

! 68: fgd_alter_lincom check
  call fgd_alter_lincom(d, 'new2', 3, 'in4', 9.9d-1, 7.8d0, 'in5', &
     1.1d1, 2.2d-2, 'in6', 1.96d0, 0d0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 68, 1, e
  end if

  n = fgd_entry(d, 'new2', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 68, 2, e
  end if

  if (n .ne. GD_LINCOM_ENTRY) then
    ne = ne + 1
    write(*, 9007) 68, 3, n
  end if

  if (ent%n_fields .ne. 3) then
    ne = ne + 1
    write(*, 9007) 68, 4, ent%n_fields
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 68, 5, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in4') then
    ne = ne + 1
    write(*, 9008) 68, 6, ent%field(1)
  end if

  if (ent%field(2) .ne. 'in5') then
    ne = ne + 1
    write(*, 9008) 68, 7, ent%field(2)
  end if

  if (ent%field(3) .ne. 'in6') then
    ne = ne + 1
    write(*, 9008) 68, 8, ent%field(3)
  end if

  if (ent%comp_scal .ne. 0) then
    ne = ne + 1
    write(*, 9007) 68, 5, ent%comp_scal
  end if

  q = (/ 9.9d-1, 7.8d0, 1.1d1, 2.2d-2, 1.96d0, 0d0 /)
  DO i=1,3
  if (abs(ent%m(i) - q(i * 2 - 1)) > 0.001) then
    ne = ne + 1
    write(*, 9010) i * 2 - 1, 68, ent%m(i)
  end if
  if (abs(ent%b(i) - q(i * 2)) > 0.001) then
    ne = ne + 1
    write(*, 9010) i * 2, 68, ent%b(i)
  end if
  end do

! 69: fgd_alter_clincom check
  cq(1) = dcmplx(0.1, 0.2)
  cq(2) = dcmplx(0.3, 0.4)
  cq(3) = dcmplx(0.4, 0.5)
  cq(4) = dcmplx(0.6, 0.7)
  call fgd_alter_clincom(d, 'new3', 2, 'in4', cq(1), cq(2), 'in3', &
     cq(3), cq(4), '', cq(5), cq(6))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 69, 1, e
  end if

  n = fgd_entry(d, 'new3', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 69, e
  end if

  if (n .ne. GD_LINCOM_ENTRY) then
    ne = ne + 1
    write(*, 9007) 69, 1, n
  end if

  if (ent%n_fields .ne. 2) then
    ne = ne + 1
    write(*, 9007) 69, 2, ent%n_fields
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 69, 3, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in4') then
    ne = ne + 1
    write(*, 9008) 69, 4, ent%field(1)
  end if

  if (ent%field(2) .ne. 'in3') then
    ne = ne + 1
    write(*, 9008) 69, 5, ent%field(2)
  end if

  if (ent%comp_scal .ne. 1) then
    ne = ne + 1
    write(*, 9007) 69, 6, ent%comp_scal
  end if

  cq(1) = dcmplx(0.1, 0.2)
  cq(2) = dcmplx(0.3, 0.4)
  cq(3) = dcmplx(0.4, 0.5)
  cq(4) = dcmplx(0.6, 0.7)
  DO i=1,2
  if (abs(ent%cm(i) - cq(i * 2 - 1)) > 0.001) then
    ne = ne + 1
    write(*, 9011) i * 2 - 1, 69, real(real(ent%cm(i))), real(aimag(ent%cm(i)))
  end if
  if (abs(ent%cb(i) - cq(i * 2)) > 0.001) then
    ne = ne + 1
    write(*, 9011) i * 2, 69, real(real(ent%cb(i))), real(aimag(ent%cb(i)))
  end if
  end do

! 70: fgd_alter_polynom check
  call fgd_alter_polynom(d, 'new4', 4, 'in1', 3d0, 4d0, 5d0, 6d0, 7d0, 0d0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 70, 1, e
  end if

  n = fgd_entry(d, 'new4', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 70, 2, e
  end if

  if (n .ne. GD_POLYNOM_ENTRY) then
    ne = ne + 1
    write(*, 9007) 70, 1, n
  end if

  if (ent%poly_ord .ne. 4) then
    ne = ne + 1
    write(*, 9007) 70, 2, ent%poly_ord
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 70, 3, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 9008) 70, 4, ent%field(1)
  end if

  q = (/ 3d0, 4d0, 5d0, 6d0, 7d0, 0d0 /)
  DO i=1,5
  if (abs(ent%a(i) - q(i)) > 0.001) then
    ne = ne + 1
    write(*, 9010) i, 70, ent%a(i)
  end if
  end do

! 71: fgd_alter_cpolynom check
  cq(1) = dcmplx(1.1, 5.0)
  cq(2) = dcmplx(1.2, 4.0)
  cq(3) = dcmplx(1.2, 3.0)
  cq(4) = dcmplx(1.3, 2.4)
  call fgd_alter_cpolynom(d, 'new5', 3, 'in1', cq(1), cq(2), cq(3), &
  cq(4), dcmplx(0d0,0d0), dcmplx(0d0,0d0))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 71, 1, e
  end if

  n = fgd_entry(d, 'new5', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 71, 2, e
  end if

  if (n .ne. GD_POLYNOM_ENTRY) then
    ne = ne + 1
    write(*, 9007) 71, 1, n
  end if

  if (ent%poly_ord .ne. 3) then
    ne = ne + 1
    write(*, 9007) 71, 2, ent%poly_ord
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 71, 3, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 9008) 71, 4, ent%field(1)
  end if

  if (ent%comp_scal .ne. 1) then
    ne = ne + 1
    write(*, 9007) 71, 5, ent%comp_scal
  end if

  cq(1) = dcmplx(1.1, 5.0)
  cq(2) = dcmplx(1.2, 4.0)
  cq(3) = dcmplx(1.2, 3.0)
  cq(4) = dcmplx(1.3, 2.4)
  DO 710 i=1,4
  if (abs(ent%ca(i) - cq(i)) > 0.001) then
    ne = ne + 1
    write(*, 9011) i, 71, real(real(ent%ca(i))), real(aimag(ent%ca(i)))
  end if
  710 CONTINUE

! 72: fgd_alter_linterp check
  call fgd_alter_linterp(d, "new6", "in3", "./other/table", 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 72, 1, e
  end if

  n = fgd_entry(d, 'new6', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 72, 2, e
  end if

  if (n .ne. GD_LINTERP_ENTRY) then
    ne = ne + 1
    write(*, 9007) 72, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 72, 2, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in3') then
    ne = ne + 1
    write(*, 9008) 72, 3, ent%field(1)
  end if

  if (ent%field(2) .ne. './other/table') then
    ne = ne + 1
    write(*, 9008) 72, 4, ent%field(2)
  end if

! 73: fgd_alter_bit check
  call fgd_alter_bit(d, "new7", "in3", 3, 2)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 73, 1, e
  end if

  n = fgd_entry(d, 'new7', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 73, 2, e
  end if

  if (n .ne. GD_BIT_ENTRY) then
    ne = ne + 1
    write(*, 9007) 73, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 73, 2, ent%fragment_index
  end if

  if (ent%numbits .ne. 2) then
    ne = ne + 1
    write(*, 9007) 73, 3, ent%numbits
  end if

  if (ent%bitnum .ne. 3) then
    ne = ne + 1
    write(*, 9007) 73, 4, ent%bitnum
  end if

  if (ent%field(1) .ne. 'in3') then
    ne = ne + 1
    write(*, 9008) 73, 5, ent%field(1)
  end if

! 74: fgd_alter_sbit check
  call fgd_alter_sbit(d, "new8", "out", 1, 22)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 74, 1, e
  end if

  n = fgd_entry(d, 'new8', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 74, 2, e
  end if

  if (n .ne. GD_SBIT_ENTRY) then
    ne = ne + 1
    write(*, 9007) 74, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 74, 2, ent%fragment_index
  end if

  if (ent%numbits .ne. 22) then
    ne = ne + 1
    write(*, 9007) 74, 3, ent%numbits
  end if

  if (ent%bitnum .ne. 1) then
    ne = ne + 1
    write(*, 9007) 74, 4, ent%bitnum
  end if

  if (ent%field(1) .ne. 'out') then
    ne = ne + 1
    write(*, 9008) 74, 5, ent%field(1)
  end if

! 75: fgd_alter_multiply check
  call fgd_alter_multiply(d, 'new9', 'in6', 'in4')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 75, 1, e
  end if

  n = fgd_entry(d, 'new9', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 75, 2, e
  end if

  if (n .ne. GD_MULTIPLY_ENTRY) then
    ne = ne + 1
    write(*, 9007) 75, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 75, 2, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in6') then
    ne = ne + 1
    write(*, 9008) 75, 3, ent%field(1)
  end if

  if (ent%field(2) .ne. 'in4') then
    ne = ne + 1
    write(*, 9008) 75, 4, ent%field(2)
  end if

! 76: fgd_alter_phase check
  call fgd_alter_phase(d, 'new10', 'in2', 8)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 76, 1, e
  end if

  n = fgd_entry(d, 'new10', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 76, 2, e
  end if

  if (n .ne. GD_PHASE_ENTRY) then
    ne = ne + 1
    write(*, 9007) 76, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 76, 2, ent%fragment_index
  end if

  if (ent%shift .ne. 8) then
    ne = ne + 1
    write(*, 9007) 76, 3, ent%shift
  end if

  if (ent%field(1) .ne. 'in2') then
    ne = ne + 1
    write(*, 9008) 76, 4, ent%field(1)
  end if

! 77: fgd_alter_const check
  call fgd_alter_const(d, 'new11', GD_FLOAT32)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 77, 1, e
  end if

  n = fgd_entry(d, 'new11', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 77, 2, e
  end if

  if (n .ne. GD_CONST_ENTRY) then
    ne = ne + 1
    write(*, 9007) 77, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 77, 2, ent%fragment_index
  end if

  if (ent%data_type .ne. GD_FLOAT32) then
    ne = ne + 1
    write(*, 9007) 77, 3, ent%data_type
  end if

  call fgd_get_constant_r4(d, 'new11', fl)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 77, 3, e
  end if

  if (abs(fl + 8.1) > 0.001) then
    ne = ne + 1
    write(*, 9005) 77, fl
  end if

! 78: fgd_encoding check
  n = fgd_encoding(d, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 78, e
  end if

  if (n .ne. GD_UNENCODED) then
    ne = ne + 1
    write(*, 9002) 78, n
  end if

! 79: fgd_endianness check
  n = fgd_endianness(d, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 79, e
  end if

  if (n .ne. (GD_LITTLE_ENDIAN + GD_NOT_ARM_ENDIAN)) then
    ne = ne + 1
    write(*, 9002) 79, n
  end if

! 80: fgd_dirfilename check
  l = GD_FIELD_LEN
  call fgd_dirfilename(str, l, d, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 80, e
  end if

  if (l .ne. GD_FIELD_LEN) then
    ne = ne + 1
    write(*, 9002) 80, l
  end if

  if (str .ne. 'test95_dirfile') then
    ne = ne + 1
    write(*, 9009) 80, str
  end if

! 81: fgd_parent_fragment check
  n = fgd_parent_fragment(d, 1)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 81, e
  end if

  if (n .ne. 0) then
    ne = ne + 1
    write(*, 9002) 81, n
  end if

! 82: fgd_alter_protection check
  call fgd_alter_protection(d, GD_PROTECT_DATA, 1)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 82, e
  end if

! 83: fgd_protection check
  n = fgd_protection(d, 1)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 83, e
  end if

  if (n .ne. GD_PROTECT_DATA) then
    ne = ne + 1
    write(*, 9002) 83, n
  end if

! 84: fgd_raw_filename check
  str = fgd_raw_filename(d, "data")
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 84, e
  end if

  if (str .ne. 'test95_dirfile/data') then
    ne = ne + 1
    write(*, 9009) 84, str
  end if

! 85: fgd_reference check
  str = fgd_reference(d, "new1")
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 85, e
  end if

  if (str .ne. 'new1') then
    ne = ne + 1
    write(*, 9009) 85, str
  end if

! 87: fgd_alter_encoding check
  call fgd_alter_encoding(d, GD_SLIM_ENCODED, 1, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 87, 1, e
  end if

  n = fgd_encoding(d, 1)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 87, 2, e
  end if

  if (n .ne. GD_SLIM_ENCODED) then
    ne = ne + 1
    write(*, 9002) 87, n
  end if

! 88: fgd_alter_endianness check
  call fgd_alter_endianness(d, GD_BIG_ENDIAN, 1, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 88, 1, e
  end if

  n = fgd_endianness(d, 1)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 88, 2, e
  end if

  if (n .ne. GD_BIG_ENDIAN) then
    ne = ne + 1
    write(*, 9002) 88, n
  end if

! 89: fgd_alter_spec check
  call fgd_alter_spec(d, 'new10 PHASE in1 3', 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 89, 1, e
  end if

  n = fgd_entry(d, 'new10', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 89, 2, e
  end if

  if (n .ne. GD_PHASE_ENTRY) then
    ne = ne + 1
    write(*, 9007) 89, 1, l
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 89, 2, ent%fragment_index
  end if

  if (ent%shift .ne. 3) then
    ne = ne + 1
    write(*, 9007) 89, 3, ent%shift
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 9008) 89, 4, ent%field(1)
  end if

! 90: fgd_delete check
  call fgd_delete(d, 'new10', 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 90, 1, e
  end if

  n = fgd_entry(d, 'new10', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_BAD_CODE) then
    ne = ne + 1
    write(*, 9006) 90, 2, e
  end if

! 91: fgd_malter_spec check
  call fgd_malter_spec(d, 'mnew10 PHASE in4 11', 'data', 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 91, 1, e
  end if

  n = fgd_entry(d, 'data/mnew10', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 91, 2, e
  end if

  if (n .ne. GD_PHASE_ENTRY) then
    ne = ne + 1
    write(*, 9007) 91, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 91, 2, ent%fragment_index
  end if

  if (ent%shift .ne. 11) then
    ne = ne + 1
    write(*, 9007) 91, 3, ent%shift
  end if

  if (ent%field(1) .ne. 'in4') then
    ne = ne + 1
    write(*, 9008) 91, 4, ent%field(1)
  end if

! 92: fgd_move check
  call fgd_move(d, 'new9', 1, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 92, 1, e
  end if

  n = fgd_entry(d, 'new9', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 92, 2, e
  end if

  if (n .ne. GD_MULTIPLY_ENTRY) then
    ne = ne + 1
    write(*, 9007) 92, 1, n
  end if

  if (ent%fragment_index .ne. 1) then
    ne = ne + 1
    write(*, 9007) 92, 2, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in6') then
    ne = ne + 1
    write(*, 9008) 92, 3, ent%field(1)
  end if

  if (ent%field(2) .ne. 'in4') then
    ne = ne + 1
    write(*, 9008) 92, 4, ent%field(2)
  end if

! 93: fgd_rename check
  call fgd_rename(d, 'new9', 'newer', 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 93, 1, e
  end if

  n = fgd_entry(d, 'new9', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_BAD_CODE) then
    ne = ne + 1
    write(*, 9006) 93, 2, e
  end if

  n = fgd_entry(d, 'newer', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 93, 3, e
  end if

  if (n .ne. GD_MULTIPLY_ENTRY) then
    ne = ne + 1
    write(*, 9007) 93, 1, l
  end if

  if (ent%fragment_index .ne. 1) then
    ne = ne + 1
    write(*, 9007) 92, 2, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in6') then
    ne = ne + 1
    write(*, 9008) 92, 3, ent%field(1)
  end if

  if (ent%field(2) .ne. 'in4') then
    ne = ne + 1
    write(*, 9008) 92, 4, ent%field(2)
  end if

! 94: fgd_uninclude check
  call fgd_uninclude(d, 1, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 94, 1, e
  end if

  n = fgd_entry(d, 'newer', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_BAD_CODE) then
    ne = ne + 1
    write(*, 9006) 94, 2, e
  end if

! 95: fgd_frameoffset check
  n = fgd_frameoffset(d, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 95, e
  end if

  if (n .ne. 0) then
    ne = ne + 1
    write(*, 9002) 95, n
  end if

! 96: fgd_alter_frameoffset check
  call fgd_alter_frameoffset(d, 33, 0, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 96, 1, e
  end if

  n = fgd_frameoffset(d, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 96, 2, e
  end if

  if (n .ne. 33) then
    ne = ne + 1
    write(*, 9002) 96, n
  end if

! 97: fgd_native_type check
  n = fgd_native_type(d, 'data')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 97, e
  end if

  if (n .ne. GD_INT8) then
    ne = ne + 1
    write(*, 9002) 97, n
  end if

! 99: fgd_validate check
  n = fgd_validate(d, 'new7')
  e = fgd_error(d)

  if (e .ne. GD_E_BAD_CODE) then
    ne = ne + 1
    write(*, 9001) 99, e
  end if

  if (n .ne. -1) then
    ne = ne + 1
    write(*, 9002) 99, n
  end if

! 100: fgd_framenum check
  str = fgd_reference(d, "data")
  dp = fgd_framenum(d, 'INDEX', 33.3d0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 100, e
  end if

  if (abs(dp - 33.3) > 0.001) then
    ne = ne + 1
    write(*, 9012) 100, dp
  end if

! 101: fgd_framenum_subset check
  dp = fgd_framenum_subset(d, 'data', 33.3d0, 6, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 101, e
  end if

  if (abs(dp - 37.0375) > 0.001) then
    ne = ne + 1
    write(*, 9012) 101, dp
  end if

! 86: fgd_eof check
  n = fgd_eof(d, 'lincom')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001), 86, e
  end if

  if (n .ne. 344) then
    ne = ne + 1
    write(*, 9002), 86, n
  end if

! 142: fgd_bof check
  n = fgd_bof(d, 'lincom')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001), 142, e
  end if

  if (n .ne. 264) then
    ne = ne + 1
    write(*, 9002), 142, n
  end if

! 143: fgd_entry (divide) check
  n = fgd_entry(d, 'div', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 143, e
  end if

  if (n .ne. GD_DIVIDE_ENTRY) then
    ne = ne + 1
    write(*, 9007) 143, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 143, 2, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'mult') then
    ne = ne + 1
    write(*, 9008) 143, 3, ent%field(1)
  end if

  if (ent%field(2) .ne. 'bit') then
    ne = ne + 1
    write(*, 9008) 143, 4, ent%field(2)
  end if

! 145: fgd_entry (recip) check
  n = fgd_entry(d, 'recip', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 145, e
  end if

  if (n .ne. GD_RECIP_ENTRY) then
    ne = ne + 1
    write(*, 9007) 145, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 145, 2, ent%fragment_index
  end if

  if (ent%comp_scal .ne. 1) then
    ne = ne + 1
    write(*, 9007) 145, 3, ent%comp_scal
  end if

  if (ent%field(1) .ne. 'div') then
    ne = ne + 1
    write(*, 9008) 145, 4, ent%field(1)
  end if

  if (abs(ent%cdividend - dcmplx(6.5, 4.3)) .gt. 1e-5) then
    ne = ne + 1
    write(*, 9013) 145, real(real(ent%cdividend)), real(aimag(ent%cdividend))
  end if

! 146: fgd_add_divide check
  call fgd_add_divide(d, 'new14', 'in1', 'in2', 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 146, 1, e
  end if

  n = fgd_entry(d, 'new14', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 146, 2, e
  end if

  if (n .ne. GD_DIVIDE_ENTRY) then
    ne = ne + 1
    write(*, 9007) 146, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 146, 2, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 9008) 146, 3, ent%field(1)
  end if

  if (ent%field(2) .ne. 'in2') then
    ne = ne + 1
    write(*, 9008) 146, 4, ent%field(2)
  end if

! 147: fgd_add_recip check
  call fgd_add_recip(d, 'new15', 'in1', 31.9d0, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 147, 1, e
  end if

  n = fgd_entry(d, 'new15', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 147, 2, e
  end if

  if (n .ne. GD_RECIP_ENTRY) then
    ne = ne + 1
    write(*, 9007) 147, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 147, 2, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 9008) 147, 3, ent%field(1)
  end if

  if (ent%comp_scal .ne. 0) then
    ne = ne + 1
    write(*, 9007) 147, 4, ent%comp_scal
  end if

  if (abs(ent%dividend - 31.9) > 1e-5) then
    ne = ne + 1
    write(*, 9012) 147, ent%dividend
  end if

! 148: fgd_add_recip check
  call fgd_add_crecip(d, 'new16', 'in1', dcmplx(31.9d0, 38.2d0), 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 148, 1, e
  end if

  n = fgd_entry(d, 'new16', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 148, 2, e
  end if

  if (n .ne. GD_RECIP_ENTRY) then
    ne = ne + 1
    write(*, 9007) 148, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 148, 2, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 9008) 148, 3, ent%field(1)
  end if

  if (ent%comp_scal .ne. 1) then
    ne = ne + 1
    write(*, 9007) 148, 4, ent%comp_scal
  end if

  if (abs(ent%cdividend - dcmplx(31.9, 38.2)) > 1e-5) then
    ne = ne + 1
    write(*, 9012) 148, ent%cdividend
  end if

! 149: fgd_madd_divide check
  call fgd_madd_divide(d, 'data', 'new14', 'in3', 'in4')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 149, 1, e
  end if

  n = fgd_entry(d, 'data/new14', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 149, 2, e
  end if

  if (n .ne. GD_DIVIDE_ENTRY) then
    ne = ne + 1
    write(*, 9007) 149, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 149, 2, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in3') then
    ne = ne + 1
    write(*, 9008) 149, 3, ent%field(1)
  end if

  if (ent%field(2) .ne. 'in4') then
    ne = ne + 1
    write(*, 9008) 149, 4, ent%field(2)
  end if

! 150: fgd_madd_recip check
  call fgd_madd_recip(d, 'data', 'new15', 'in0', 95.5d0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 150, 1, e
  end if

  n = fgd_entry(d, 'data/new15', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 150, 2, e
  end if

  if (n .ne. GD_RECIP_ENTRY) then
    ne = ne + 1
    write(*, 9007) 150, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 150, 2, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in0') then
    ne = ne + 1
    write(*, 9008) 150, 3, ent%field(1)
  end if

  if (ent%comp_scal .ne. 0) then
    ne = ne + 1
    write(*, 9007) 150, 4, ent%comp_scal
  end if

  if (abs(ent%dividend - 95.5) > 1e-5) then
    ne = ne + 1
    write(*, 9012) 150, ent%dividend
  end if

! 151: fgd_madd_recip check
  call fgd_madd_crecip(d, 'data', 'new16', 'in3', dcmplx(8.47d0, 6.22d0))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 151, 1, e
  end if

  n = fgd_entry(d, 'data/new16', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 151, 2, e
  end if

  if (n .ne. GD_RECIP_ENTRY) then
    ne = ne + 1
    write(*, 9007) 151, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 151, 2, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in3') then
    ne = ne + 1
    write(*, 9008) 151, 3, ent%field(1)
  end if

  if (ent%comp_scal .ne. 1) then
    ne = ne + 1
    write(*, 9007) 151, 4, ent%comp_scal
  end if

  if (abs(ent%cdividend - dcmplx(8.47, 6.22)) > 1e-5) then
    ne = ne + 1
    write(*, 9012) 151, ent%cdividend
  end if

! 152: fgd_alter_divide check
  call fgd_alter_divide(d, 'new14', 'in6', 'in4')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 152, 1, e
  end if

  n = fgd_entry(d, 'new14', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 152, 2, e
  end if

  if (n .ne. GD_DIVIDE_ENTRY) then
    ne = ne + 1
    write(*, 9007) 152, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 152, 2, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in6') then
    ne = ne + 1
    write(*, 9008) 152, 3, ent%field(1)
  end if

  if (ent%field(2) .ne. 'in4') then
    ne = ne + 1
    write(*, 9008) 152, 4, ent%field(2)
  end if

! 153: fgd_alter_recip check
  call fgd_alter_recip(d, 'new15', 'in5', 0.187d0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 153, 1, e
  end if

  n = fgd_entry(d, 'new15', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 153, 2, e
  end if

  if (n .ne. GD_RECIP_ENTRY) then
    ne = ne + 1
    write(*, 9007) 153, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 153, 2, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in5') then
    ne = ne + 1
    write(*, 9008) 153, 3, ent%field(1)
  end if

  if (abs(ent%dividend - 0.187) > 1e-5) then
    ne = ne + 1
    write(*, 9012) 153, ent%dividend
  end if

! 154: fgd_alter_crecip check
  call fgd_alter_crecip(d, 'new16', 'in2', dcmplx(4.3d0, 81.81d0))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 154, 1, e
  end if

  n = fgd_entry(d, 'new16', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 154, 2, e
  end if

  if (n .ne. GD_RECIP_ENTRY) then
    ne = ne + 1
    write(*, 9007) 154, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 154, 2, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in2') then
    ne = ne + 1
    write(*, 9008) 154, 3, ent%field(1)
  end if

  if (abs(ent%cdividend - dcmplx(4.3d0, 81.81d0)) > 1e-5) then
    ne = ne + 1
    write(*, 9013) 154, ent%cdividend
  end if

! 155: fgd_rewrite_fragment check
  call fgd_rewrite_fragment(d, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001), 155, e
  end if

! 156: fgd_invalid_dirfile check
  m = fgd_invalid_dirfile()
  e = fgd_error(m)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006), 156, 1, e
  end if

  n = fgd_nfragments(m)
  e = fgd_error(m)

  if (e .ne. GD_E_BAD_DIRFILE) then
    ne = ne + 1
    write(*, 9006), 156, 2, e
  end if

  call fgd_close(m)

! 157: fgd_dirfile_standards
  n = fgd_dirfile_standards(d, GD_VERSION_CURRENT)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006), 157, 1, e
  end if

  if (n .ne. 8) then
    ne = ne + 1
    write(*, 9002), 157, n
  end if

  n = fgd_dirfile_standards(d, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_BAD_VERSION) then
    ne = ne + 1
    write(*, 9006), 156, 2, e
  end if

! 158: gd_get_carray_slice (INT8)
  call fgd_get_carray_i1(d, "carray", 0, 0, ci1)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001), 158, e
  end if

  do i=1,6
    if (ci1(i) .ne. i) then
      ne = ne + 1
      write(*, 9004), 158, i, ci1(i)
    end if
  end do

! 159: gd_get_carray_slice (INT8)
  call fgd_get_carray_i1(d, "carray", 3, 2, ci1)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001), 159, e
  end if

  do i=1,2
    if (ci1(i) .ne. i + 2) then
      ne = ne + 1
      write(*, 9004), 159, i, ci1(i)
    end if
  end do

! 160: gd_get_carray_slice (INT16)
  call fgd_get_carray_i2(d, "carray", 3, 2, ci2)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001), 160, e
  end if

  do i=1,2
    if (ci2(i) .ne. i + 2) then
      ne = ne + 1
      write(*, 9004), 160, i, ci2(i)
    end if
  end do

! 161: gd_get_carray_slice (INT32)
  call fgd_get_carray_i4(d, "carray", 3, 2, ci4)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001), 161, e
  end if

  do i=1,2
    if (ci4(i) .ne. i + 2) then
      ne = ne + 1
      write(*, 9004), 161, i, ci4(i)
    end if
  end do

! 162: gd_get_carray_slice (INT64)
  call fgd_get_carray_i8(d, "carray", 3, 2, ci8)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001), 162, e
  end if

  do i=1,2
    if (ci8(i) .ne. i + 2) then
      ne = ne + 1
      write(*, 9004), 162, i, ci8(i)
    end if
  end do

! 163: gd_get_carray_slice (FLOAT32)
  call fgd_get_carray_r4(d, "carray", 3, 2, cr4)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001), 163, e
  end if

  do i=1,2
    if (abs(cr4(i) - (2 + i) * 1.1) > 0.001) then
      ne = ne + 1
      write(*, 9010), 163, i, cr4(i)
    end if
  end do

! 164: gd_get_carray_slice (FLOAT64)
  call fgd_get_carray_r8(d, "carray", 3, 2, cr8)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001), 164, e
  end if

  do i=1,2
    if (abs(cr8(i) - (2 + i) * 1.1) > 0.001) then
      ne = ne + 1
      write(*, 9010), 164, i, cr8(i)
    end if
  end do

! 165: gd_get_carray_slice (COMPLEX64)
  call fgd_get_carray_c8(d, "carray", 3, 2, cc8)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001), 165, e
  end if

  do i=1,2
    if (abs(cc8(i) - (2 + i) * 1.1) > 0.001) then
      ne = ne + 1
      write(*, 9011), 165, i, real(real(cc8(i))), real(aimag(cc8(i)))
    end if
  end do

! 166: gd_get_carray_slice (COMPLEX128)
  call fgd_get_carray_c16(d, "carray", 3, 2, cc16)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001), 166, e
  end if

  do i=1,2
    if (abs(cc16(i) - (2 + i) * 1.1) > 0.001) then
      ne = ne + 1
      write(*, 9011), 166, i, real(real(cc16(i))), real(aimag(cc16(i)))
    end if
  end do

! 168: gd_put_carray
  ci1 = (/ 11_1, 12_1, 13_1, 14_1, 15_1, 16_1, 0_1, 0_1 /)
  call fgd_put_carray_i1(d, "carray", 0, 0, ci1)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006), 168, 1, e
  end if

  call fgd_get_carray_i1(d, "carray", 0, 0, ci1)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006), 168, 2, e
  end if

  do i=1,6
    if (ci1(i) .ne. i + 10) then
      ne = ne + 1
      write(*, 9004), 168, i, ci1(i)
    end if
  end do

! 169: gd_put_carray_slice (INT8)
  ci1 = (/ 72_1, 73_1, 0_1, 0_1, 0_1, 0_1, 0_1, 0_1 /)
  call fgd_put_carray_i1(d, "carray", 3, 2, ci1)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006), 169, 1, e
  end if

  call fgd_get_carray_i1(d, "carray", 0, 0, ci1)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006), 169, 2, e
  end if

  do i=1,6
    if (i .eq. 3 .or. i .eq. 4) then
      if (ci1(i) .ne. 69 + i) then
        ne = ne + 1
        write(*, 9004), 169, i, ci1(i)
      end if
    else
      if (ci1(i) .ne. i + 10) then
        ne = ne + 1
        write(*, 9004), 169, i, ci1(i)
      end if
    end if
  end do

! 170: gd_put_carray_slice (INT16)
  ci2 = (/ 173_2, 174_2, 0_2, 0_2, 0_2, 0_2, 0_2, 0_2 /)
  call fgd_put_carray_i2(d, "carray", 3, 2, ci2)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006), 170, 1, e
  end if

  call fgd_get_carray_i2(d, "carray", 0, 0, ci2)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006), 170, 2, e
  end if

  do i=1,6
    if (i .eq. 3 .or. i .eq. 4) then
      if (ci2(i) .ne. 170 + i) then
        ne = ne + 1
        write(*, 9004), 170, i, ci2(i)
      end if
    else
      if (ci2(i) .ne. i + 10) then
        ne = ne + 1
        write(*, 9004), 170, i, ci2(i)
      end if
    end if
  end do

! 171: gd_put_carray_slice (INT32)
  ci4 = (/ 174, 175, 0, 0, 0, 0, 0, 0 /)
  call fgd_put_carray_i4(d, "carray", 3, 2, ci4)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006), 171, 1, e
  end if

  call fgd_get_carray_i4(d, "carray", 0, 0, ci4)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006), 171, 2, e
  end if

  do i=1,6
    if (i .eq. 3 .or. i .eq. 4) then
      if (ci4(i) .ne. 171 + i) then
        ne = ne + 1
        write(*, 9004), 171, i, ci4(i)
      end if
    else
      if (ci4(i) .ne. i + 10) then
        ne = ne + 1
        write(*, 9004), 171, i, ci4(i)
      end if
    end if
  end do

! 172: gd_put_carray_slice (INT64)
  ci8 = (/ 175, 176, 0, 0, 0, 0, 0, 0 /)
  call fgd_put_carray_i8(d, "carray", 3, 2, ci8)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006), 172, 1, e
  end if

  call fgd_get_carray_i8(d, "carray", 0, 0, ci8)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006), 172, 2, e
  end if

  do i=1,6
    if (i .eq. 3 .or. i .eq. 4) then
      if (ci8(i) .ne. 172 + i) then
        ne = ne + 1
        write(*, 9004), 172, i, ci8(i)
      end if
    else
      if (ci8(i) .ne. i + 10) then
        ne = ne + 1
        write(*, 9004), 172, i, ci8(i)
      end if
    end if
  end do

! 173: gd_put_carray_slice (FLOAT32)
  cr4 = (/ 176., 177., 0., 0., 0., 0., 0., 0. /)
  call fgd_put_carray_r4(d, "carray", 3, 2, cr4)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006), 173, 1, e
  end if

  call fgd_get_carray_r4(d, "carray", 0, 0, cr4)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006), 173, 2, e
  end if

  do i=1,6
    if (i .eq. 3 .or. i .eq. 4) then
      if (abs(cr4(i) - 173. - i) > 0.001) then
        ne = ne + 1
        write(*, 9010), 173, i, cr4(i)
      end if
    else
      if (cr4(i) .ne. i + 10) then
        ne = ne + 1
        write(*, 9010), 173, i, cr4(i)
      end if
    end if
  end do

! 174: gd_put_carray_slice (FLOAT64)
  cr8 = (/ 177., 178., 0., 0., 0., 0., 0., 0. /)
  call fgd_put_carray_r8(d, "carray", 3, 2, cr8)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006), 174, 1, e
  end if

  call fgd_get_carray_r8(d, "carray", 0, 0, cr8)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006), 174, 2, e
  end if

  do i=1,6
    if (i .eq. 3 .or. i .eq. 4) then
      if (abs(cr8(i) - 174. - i) > 0.001) then
        ne = ne + 1
        write(*, 9010), 174, i, cr8(i)
      end if
    else
      if (cr8(i) .ne. i + 10) then
        ne = ne + 1
        write(*, 9010), 174, i, cr8(i)
      end if
    end if
  end do

! 175: gd_put_carray_slice (COMPLEX64)
  cc8 = (/ 178., 179., 0., 0., 0., 0., 0., 0. /)
  call fgd_put_carray_c8(d, "carray", 3, 2, cc8)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006), 175, 1, e
  end if

  call fgd_get_carray_c8(d, "carray", 0, 0, cc8)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006), 175, 2, e
  end if

  do i=1,6
    if (i .eq. 3 .or. i .eq. 4) then
      if (abs(cc8(i) - 175. - i) > 0.001) then
        ne = ne + 1
        write(*, 9011), 175, i, real(real(cc8(i))), real(aimag(cc8(i)))
      end if
    else
      if (cc8(i) .ne. i + 10) then
        ne = ne + 1
        write(*, 9011), 175, i, real(real(cc8(i))), real(aimag(cc8(i)))
      end if
    end if
  end do

! 176: gd_put_carray_slice (COMPLEX128)
  cc16 = (/ 179., 180., 0., 0., 0., 0., 0., 0. /)
  call fgd_put_carray_c16(d, "carray", 3, 2, cc16)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006), 176, 1, e
  end if

  call fgd_get_carray_c16(d, "carray", 0, 0, cc16)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006), 176, 2, e
  end if

  do i=1,6
    if (i .eq. 3 .or. i .eq. 4) then
      if (abs(cc16(i) - 176. - i) > 0.001) then
        ne = ne + 1
        write(*, 9011), 176, i, real(real(cc16(i))), real(aimag(cc16(i)))
      end if
    else
      if (cc16(i) .ne. i + 10) then
        ne = ne + 1
        write(*, 9011), 176, i, real(real(cc16(i))), real(aimag(cc16(i)))
      end if
    end if
  end do

! 177: gd_carray_len
  n = fgd_carray_len(d, 'carray')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001), 177, e
  end if

  if (n .ne. 6) then
    ne = ne + 1
    write(*, 9002), 177, n
  end if

! 178: gd_entry (CARRAY)
  n = fgd_entry(d, 'carray', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 178, e
  end if

  if (n .ne. GD_CARRAY_ENTRY) then
    ne = ne + 1
    write(*, 9007) 178, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 178, 2, ent%fragment_index
  end if

  if (ent%array_len .ne. 6) then
    ne = ne + 1
    write(*, 9007) 178, 2, ent%array_len
  end if

  if (ent%data_type .ne. GD_FLOAT64) then
    ne = ne + 1
    write(*, 9007) 178, 3, ent%data_type
  end if

! 179: gd_add_carray
  call fgd_add_carray(d, 'new17', GD_FLOAT64, 4, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 179, 1, e
  end if

  n = fgd_entry(d, 'new17', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 179, 2, e
  end if

  if (n .ne. GD_CARRAY_ENTRY) then
    ne = ne + 1
    write(*, 9007) 179, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 179, 2, ent%fragment_index
  end if

  if (ent%array_len .ne. 4) then
    ne = ne + 1
    write(*, 9007) 179, 3, ent%array_len
  end if

  if (ent%data_type .ne. GD_FLOAT64) then
    ne = ne + 1
    write(*, 9007) 179, 4, ent%data_type
  end if

  call fgd_get_carray_r4(d, 'new17', 0, 0, cr4)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 179, 3, e
  end if

  do i=1,4
    if (abs(cr4(i)) > 1e-5) then
      ne = ne + 1
      write(*, 9010) 179, i, cr4(i)
    end if
  end do

! 180: gd_madd_carray
  call fgd_madd_carray(d, 'data', 'new17', GD_FLOAT64, 4)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 180, 1, e
  end if

  n = fgd_entry(d, 'data/new17', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 180, 2, e
  end if

  if (n .ne. GD_CARRAY_ENTRY) then
    ne = ne + 1
    write(*, 9007) 180, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 180, 2, ent%fragment_index
  end if

  if (ent%array_len .ne. 4) then
    ne = ne + 1
    write(*, 9007) 180, 3, ent%array_len
  end if

  if (ent%data_type .ne. GD_FLOAT64) then
    ne = ne + 1
    write(*, 9007) 180, 4, ent%data_type
  end if

  call fgd_get_carray_r4(d, 'data/new17', 0, 0, cr4)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 180, 3, e
  end if

  do i=1,4
    if (abs(cr4(i)) > 1e-5) then
      ne = ne + 1
      write(*, 9010) 180, i, cr4(i)
    end if
  end do

! 181: gd_alter_carray
  call fgd_alter_carray(d, 'new17', GD_FLOAT32, 3)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 181, 1, e
  end if

  n = fgd_entry(d, 'new17', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 181, 2, e
  end if

  if (n .ne. GD_CARRAY_ENTRY) then
    ne = ne + 1
    write(*, 9007) 181, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 9007) 181, 2, ent%fragment_index
  end if

  if (ent%data_type .ne. GD_FLOAT32) then
    ne = ne + 1
    write(*, 9007) 181, 3, ent%data_type
  end if

  if (ent%array_len .ne. 3) then
    ne = ne + 1
    write(*, 9007) 181, 4, ent%data_type
  end if

  call fgd_get_carray_r4(d, 'new17', 0, 0, cr4)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9006) 181, 3, e
  end if

  do i=1,4
    if (abs(cr4(i)) > 1e-5) then
      ne = ne + 1
      write(*, 9010) 181, i, cr4(i)
    end if
  end do

! 183: fgd_constants_i1 check
  iq(1) = -123
  iq(2) = -8
  n = fgd_nfields_by_type(d, GD_CONST_ENTRY)
  call fgd_constants_i1(ci1, d)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 183, e
  end if

  do i = 1, n
  if (ci1(i) .ne. iq(i)) then
    ne = ne + 1
    write(*, 9004) i, 183, ci1(i)
  end if
  end do

! 184: fgd_constants_i2 check
  iq(1) = 133
  call fgd_constants_i2(ci2, d)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 184, e
  end if

  do i = 1, n
  if (ci2(i) .ne. iq(i)) then
    ne = ne + 1
    write(*, 9004) i, 184, ci2(i)
  end if
  end do

! 185: fgd_constants_i4 check
  call fgd_constants_i4(ci4, d)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 185, e
  end if

  do i = 1, n
  if (ci4(i) .ne. iq(i)) then
    ne = ne + 1
    write(*, 9004) i, 185, ci4(i)
  end if
  end do

! 186: fgd_constants_i8 check
  call fgd_constants_i8(ci8, d)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 186, e
  end if

  do i = 1, n
  if (ci8(i) .ne. iq(i)) then
    ne = ne + 1
    write(*, 9004) i, 186, ci8(i)
  end if
  end do

! 187: fgd_constants_r4 check
  q(1) = 133.
  q(2) = -8.1
  call fgd_constants_r4(cr4, d)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 187, e
  end if

  do i = 1, n
  if (abs(cr4(i) - q(i)) .gt. 1e-5) then
    ne = ne + 1
    write(*, 9010) i, 187, cr4(i)
  end if
  end do

! 188: fgd_constants_r8 check
  call fgd_constants_r8(cr8, d)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 188, e
  end if

  do i = 1, n
  if (abs(cr8(i) - q(i)) .gt. 1e-5) then
    ne = ne + 1
    write(*, 9010) i, 188, cr8(i)
  end if
  end do


! 189: fgd_constants_c8 check
  cq(1) = 133.
  cq(2) = -8.1
  call fgd_constants_c8(cc8, d)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 189, e
  end if

  do i = 1, n
  if (abs(cc8(i) - cq(i)) .gt. 1e-5) then
    ne = ne + 1
    write(*, 9011) i, 189, cc8(i)
  end if
  end do

! 190: fgd_constants_c16 check
  call fgd_constants_c16(cc16, d)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 190, e
  end if

  do i = 1, n
  if (abs(cc16(i) - cq(i)) .gt. 1e-5) then
    ne = ne + 1
    write(*, 9011) i, 190, cc16(i)
  end if
  end do

! 191: fgd_mconstants_i1 check
  iq(1) = 3
  iq(2) = 0
  n = fgd_nmfields_by_type(d, 'data', GD_CONST_ENTRY)
  call fgd_mconstants_i1(ci1, d, 'data')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 191, e
  end if

  do i = 1, n
  if (ci1(i) .ne. iq(i)) then
    ne = ne + 1
    write(*, 9004) i, 191, ci1(i)
  end if
  end do

! 192: fgd_mconstants_i2 check
  call fgd_mconstants_i2(ci2, d, 'data')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 192, e
  end if

  do i = 1, n
  if (ci2(i) .ne. iq(i)) then
    ne = ne + 1
    write(*, 9004) i, 192, ci2(i)
  end if
  end do

! 193: fgd_mconstants_i4 check
  call fgd_mconstants_i4(ci4, d, 'data')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 193, e
  end if

  do i = 1, n
  if (ci4(i) .ne. iq(i)) then
    ne = ne + 1
    write(*, 9004) i, 193, ci4(i)
  end if
  end do

! 194: fgd_mconstants_i8 check
  call fgd_mconstants_i8(ci8, d, 'data')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 194, e
  end if

  do i = 1, n
  if (ci8(i) .ne. iq(i)) then
    ne = ne + 1
    write(*, 9004) i, 194, ci8(i)
  end if
  end do

! 195: fgd_mconstants_r4 check
  q(1) = 3.3
  q(2) = 0.
  call fgd_mconstants_r4(cr4, d, 'data')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 195, e
  end if

  do i = 1, n
  if (abs(cr4(i) - q(i)) .gt. 1e-5) then
    ne = ne + 1
    write(*, 9010) i, 195, cr4(i)
  end if
  end do

! 196: fgd_mconstants_r8 check
  call fgd_mconstants_r8(cr8, d, 'data')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 196, e
  end if

  do i = 1, n
  if (abs(cr8(i) - q(i)) .gt. 1e-5) then
    ne = ne + 1
    write(*, 9010) i, 196, cr8(i)
  end if
  end do


! 197: fgd_mconstants_c8 check
  cq(1) = dcmplx(3.3, 4.4)
  cq(2) = 0.
  call fgd_mconstants_c8(cc8, d, 'data')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 197, e
  end if

  do i = 1, n
  if (abs(cc8(i) - cq(i)) .gt. 1e-5) then
    ne = ne + 1
    write(*, 9011) i, 197, cc8(i)
  end if
  end do

! 198: fgd_mconstants_c16 check
  call fgd_mconstants_c16(cc16, d, 'data')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 198, e
  end if

  do i = 1, n
  if (abs(cc16(i) - cq(i)) .gt. 1e-5) then
    ne = ne + 1
    write(*, 9011) i, 198, cc16(i)
  end if
  end do

! 199: fgd_strings check
  strings(1) = "Lorem ipsum"
  strings(2) = ""
  strings(3) = "Arthur Dent"
  n = fgd_nfields_by_type(d, GD_STRING_ENTRY)
  l = slen
  call fgd_strings(st, d, l)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 199, e
  end if

  if (l .ne. slen) then
    ne = ne + 1
    write(*, 9002) 199, l
  end if

  do i = 1, n
  if (st(i) .ne. strings(i)) then
    ne = ne + 1
    write(*, 9008) i, 199, st(i)
  end if
  end do

! 200: fgd_strings check
  strings(1) = "This is a string constant."
  strings(2) = ""
  n = fgd_nmfields_by_type(d, 'data', GD_STRING_ENTRY)
  l = slen
  call fgd_mstrings(st, d, 'data', l)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 200, e
  end if

  if (l .ne. slen) then
    ne = ne + 1
    write(*, 9002) 200, l
  end if

  do i = 1, n
  if (st(i) .ne. strings(i)) then
    ne = ne + 1
    write(*, 9008) i, 200, st(i)
  end if
  end do

! 201: fgd_string_value_max
  n = fgd_string_value_max(d)

  if (n .ne. 11) then
    ne = ne + 1
    write(*, 9002) 201, n
  end if

! 202: fgd_mstring_value_max
  n = fgd_mstring_value_max(d, 'data')

  if (n .ne. 26) then
    ne = ne + 1
    write(*, 9002) 202, n
  end if

! 203: fgd_seek check
  n = fgd_seek(d, 'data', 35, 0, GD_SEEK_SET)
  e = fgd_error(d)
  m = fgd_getdata_r8(d, 'data', GD_HERE, 0, 1, 0, cr8)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 203, e
  end if

  if (n .ne. 280) then
    ne = ne + 1
    write(*, 9007) 203, 1, n
  end if

  if (m .ne. 8) then
    ne = ne + 1
    write(*, 9007) 203, 2, n
  end if

  do i=1,8
  if (abs(cr8(i) - 16 - i) .gt. 1e-5) then
    ne = ne + 1
    write(*, 9010) i, 203, cr8(i)
  end if
  end do

! 204: fgd_tell check
  n = fgd_tell(d, 'data')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 9001) 204, e
  end if

  if (n .ne. 288) then
    ne = ne + 1
    write(*, 9002) 204, n
  end if

 


  


!================================================================
  call fgd_discard(d)

  call system ( 'rm -rf ' // fildir )

  if (ne .GT. 0) then
    write(*, 9003) ne
    call exit(1)
  end if

9001 format('e[', i0, '] = ', i0)
9002 format('n[', i0, '] = ', i0)
9003 format('ne = ', i0)
9004 format('c(', i0, ')[', i0, '] = ', i0)
9005 format('fl[', i0, '] = ', f0.16)
9006 format('e[', i0, ', ', i0, '] = ', i0)
9007 format('n[', i0, ', ', i0, '] = ', i0)
9008 format('fn(', i0, ')[', i0, '] = "', a, '"')
9009 format('s[' i0, '] = "', a, '"')
9010 format('p(', i0, ')[', i0, '] = ', d16.10)
9011 format('p(', i0, ')[', i0, '] = ', d16.10, ';', d16.10)
9012 format('d[', i0, '] = ', d16.10)
9013 format('c[', i0, '] = ', d16.10, ';', d16.10)
end program
