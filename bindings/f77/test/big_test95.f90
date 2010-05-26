! General test
!
! This very large test checks almost every procedure defined by the F95
! bindings.  Procedures not tested include: fgd_copen fgd_metaflush
! fgd_flush fgd_discard fgd_callback fgd_close
! (although this last one is used)

program big_test
  use getdata
  character (len=*), parameter :: fildir = 'test95_dirfile'
  character (len=*), parameter :: frmat = 'test95_dirfile/format'
  character (len=*), parameter :: frm2 = 'test95_dirfile/form2'
  character (len=*), parameter :: dat = 'test95_dirfile/data'
  integer, parameter :: flen = 7
  integer, parameter :: nfields = 11

  character (len=flen), dimension(nfields + 10) :: fields
  character (len=flen), dimension(nfields + 10) :: flist
  character (len=GD_FIELD_LEN) :: str
  integer(1), dimension(80) :: datadata
  integer :: i, d, e, n, l, ne
  real :: fl
  double precision :: dp
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
    datadata(i) = i
  end do

  call system ( 'rm -rf ' // fildir )
  call system ( 'mkdir ' // fildir )

  fields = (/ 'INDEX  ', 'bit    ', 'const  ', 'data   ', 'lincom ', &
  'linterp', 'mult   ', 'phase  ', 'polynom', 'sbit   ', 'string ', '       ', &
  '       ', '       ', '       ', '       ', '       ', '       ', '       ', &
  '       ', '       ' /)

  open(1, file=frmat, status='new')
  write(1, *) '/ENDIAN little'
  write(1, *) 'data RAW INT8 8'
  write(1, *) 'lincom LINCOM data 1.1 2.2 INDEX 2.2 3.3;4.4 linterp const const'
  write(1, *) '/META data mstr STRING "This is a string constant."'
  write(1, *) '/META data mconst CONST COMPLEX128 3.3;4.4'
  write(1, *) '/META data mlut LINTERP DATA ./lut'
  write(1, *) 'const CONST FLOAT64 5.5'
  write(1, *) 'linterp LINTERP data /look/up/file'
  write(1, *) 'polynom POLYNOM data 1.1 2.2 2.2 3.3;4.4 const const'
  write(1, *) 'bit BIT data 3 4'
  write(1, *) 'sbit SBIT data 5 6'
  write(1, *) 'mult MULTIPLY data sbit'
  write(1, *) 'phase PHASE data 11'
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
    write(*, 2001), 0, e
  end if

! 1: fgd_open check
  d = fgd_open(fildir, GD_RDWR)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 1, e
  end if

! 2: fgd_getdata_i1 check
  n = fgd_getdata_i1(d, 'data', 5, 0, 1, 0, ci1)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 2, e
  end if

  if (n .ne. 8) then
    ne = ne + 1
    write(*, 2002) 2, n
  end if

  do i=1,8
  if (ci1(i) .ne. 40 + i) then
    ne = ne + 1
    write(*, 2004) i, 2, ci1(i)
  end if
  end do 

! 102: fgd_getdata_i2 check
  n = fgd_getdata_i2(d, 'data', 5, 0, 1, 0, ci2)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 102, e
  end if

  if (n .ne. 8) then
    ne = ne + 1
    write(*, 2002) 102, n
  end if

  do i=1,8
  if (ci2(i) .ne. 40 + i) then
    ne = ne + 1
    write(*, 2004) i, 102, ci2(i)
  end if
  end do 

! 103: fgd_getdata_i4 check
  n = fgd_getdata_i4(d, 'data', 5, 0, 1, 0, ci4)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 103, e
  end if

  if (n .ne. 8) then
    ne = ne + 1
    write(*, 2002) 103, n
  end if

  do i=1,8
  if (ci4(i) .ne. 40 + i) then
    ne = ne + 1
    write(*, 2004) i, 103, ci4(i)
  end if
  end do 

! 104: fgd_getdata_i8 check
  n = fgd_getdata_i8(d, 'data', 5, 0, 1, 0, ci8)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 104, e
  end if

  if (n .ne. 8) then
    ne = ne + 1
    write(*, 2002) 104, n
  end if

  do i=1,8
  if (ci8(i) .ne. 40 + i) then
    ne = ne + 1
    write(*, 2004) i, 104, ci8(i)
  end if
  end do 

! 105: fgd_getdata_r4 check
  n = fgd_getdata_r4(d, 'data', 5, 0, 1, 0, cr4)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 105, e
  end if

  if (n .ne. 8) then
    ne = ne + 1
    write(*, 2002) 105, n
  end if

  do i=1,8
  if (abs(cr4(i) - 40 - i) .gt. 1e-5) then
    ne = ne + 1
    write(*, 2010) i, 105, cr4(i)
  end if
  end do 

! 106: fgd_getdata_r8 check
  n = fgd_getdata_r8(d, 'data', 5, 0, 1, 0, cr8)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 106, e
  end if

  if (n .ne. 8) then
    ne = ne + 1
    write(*, 2002) 106, n
  end if

  do i=1,8
  if (abs(cr8(i) - 40 - i) .gt. 1e-5) then
    ne = ne + 1
    write(*, 2010) i, 106, cr8(i)
  end if
  end do 

! 107: fgd_getdata_c8 check
  n = fgd_getdata_c8(d, 'data', 5, 0, 1, 0, cc8)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 107, e
  end if

  if (n .ne. 8) then
    ne = ne + 1
    write(*, 2002) 107, n
  end if

  do i=1,8
  if (abs(cc8(i) - 40 - i) .gt. 1e-5) then
    ne = ne + 1
    write(*, 2011) i, 107, real(real(cc8(i))), real(aimag(cc8(i)))
  end if
  end do 

! 108: fgd_getdata_c16 check
  n = fgd_getdata_c16(d, 'data', 5, 0, 1, 0, cc16)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 108, e
  end if

  if (n .ne. 8) then
    ne = ne + 1
    write(*, 2002) 108, n
  end if

  do i=1,8
  if (abs(cc16(i) - 40 - i) .gt. 1e-5) then
    ne = ne + 1
    write(*, 2011) i, 107, real(real(cc16(i))), real(aimag(cc16(i)))
  end if
  end do 

! 109: fgd_getdata_n check
  n = fgd_getdata_n(d, 'data', 5, 0, 1, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 109, e
  end if

  if (n .ne. 8) then
    ne = ne + 1
    write(*, 2002) 109, n
  end if

! 3: fgd_get_constant_i1 check
  n = fgd_get_constant_i1(d, 'const', ci1(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 3, e
  end if

  if (n .ne. 0) then
    ne = ne + 1
    write(*, 2002) 3, n
  end if

  if (ci1(1) .ne. 5) then
    ne = ne + 1
    write(*, 2002) 3, ci1(1)
  end if

! 110: fgd_get_constant_i2 check
  n = fgd_get_constant_i2(d, 'const', ci2(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 110, e
  end if

  if (n .ne. 0) then
    ne = ne + 1
    write(*, 2002) 110, n
  end if

  if (ci2(1) .ne. 5) then
    ne = ne + 1
    write(*, 2002) 110, ci2(1)
  end if

! 111: fgd_get_constant_i4 check
  n = fgd_get_constant_i4(d, 'const', ci4(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 111, e
  end if

  if (n .ne. 0) then
    ne = ne + 1
    write(*, 2002) 111, n
  end if

  if (ci4(1) .ne. 5) then
    ne = ne + 1
    write(*, 2002) 111, ci4(1)
  end if

! 112: fgd_get_constant_i8 check
  n = fgd_get_constant_i8(d, 'const', ci8(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 112, e
  end if

  if (n .ne. 0) then
    ne = ne + 1
    write(*, 2002) 112, n
  end if

  if (ci8(1) .ne. 5) then
    ne = ne + 1
    write(*, 2002) 112, ci8(1)
  end if

! 113: fgd_get_constant_r4 check
  n = fgd_get_constant_r4(d, 'const', cr4(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 113, e
  end if

  if (n .ne. 0) then
    ne = ne + 1
    write(*, 2002) 113, n
  end if

  if (abs(cr4(1) - 5.5) .gt. 1e-5) then
    ne = ne + 1
    write(*, 2005) 113, cr4(1)
  end if

! 114: fgd_get_constant_r8 check
  n = fgd_get_constant_r8(d, 'const', cr8(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 114, e
  end if

  if (n .ne. 0) then
    ne = ne + 1
    write(*, 2002) 114, n
  end if

  if (abs(cr8(1) - 5.5) .gt. 1e-5) then
    ne = ne + 1
    write(*, 2012) 114, cr8(1)
  end if

! 115: fgd_get_constant_c8 check
  n = fgd_get_constant_c8(d, 'const', cc8(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 115, e
  end if

  if (n .ne. 0) then
    ne = ne + 1
    write(*, 2002) 115, n
  end if

  if (abs(cc8(1) - 5.5) .gt. 1e-5) then
    ne = ne + 1
    write(*, 2013) 115, real(real(cc8(i))), real(aimag(cc8(i)))
  end if

! 116: fgd_get_constant_c16 check
  n = fgd_get_constant_c16(d, 'const', cc16(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 116, e
  end if

  if (n .ne. 0) then
    ne = ne + 1
    write(*, 2002) 116, n
  end if

  if (abs(cc16(1) - 5.5) .gt. 1e-5) then
    ne = ne + 1
    write(*, 2013) 116, real(real(cc16(i))), real(aimag(cc16(i)))
  end if

! 117: fgd_get_constant_n check
  n = fgd_get_constant_n(d, 'const')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 117, e
  end if

  if (n .ne. 0) then
    ne = ne + 1
    write(*, 2002) 117, n
  end if

! 4: fgd_get_field_name_max check
  i = fgd_get_field_name_max(d)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 4, e
  end if

  if (i .ne. flen) then
    ne = ne + 1
    write(*, 2002) 4, i
  end if

! 5: fgd_get_mfield_name_max check
  i = fgd_get_mfield_name_max(d, 'data')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 5, e
  end if

  if (i .ne. 6) then
    ne = ne + 1
    write(*, 2002) 5, i
  end if

! 6: fgd_get_nfields check
  n = fgd_get_nfields(d)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 6, e
  end if

  if (n .ne. nfields) then
    ne = ne + 1
    write(*, 2002) 6, n
  end if

! 8: fgd_get_field_list check
  l = flen
  call fgd_get_field_list(flist, d, l)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 8, e
  end if

  if (l .ne. flen) then
    ne = ne + 1
    write(*, 2002) 8, l
  end if

  do i = 1, n
  if (flist(i) .ne. fields(i)) then
    ne = ne + 1
    write(*, 2008) i, 8, flist(i)
  end if
  end do

! 9: fgd_get_nmfields check
  n = fgd_get_nmfields(d, 'data')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 9, e
  end if

  if (n .ne. 3) then
    ne = ne + 1
    write(*, 2002) 9, n
  end if

! 10: fgd_get_mfield_list check
  fields(1) = 'mstr'
  fields(2) = 'mconst'
  fields(3) = 'mlut'

  l = flen
  call fgd_get_mfield_list(flist, d, 'data', l)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 10, i, e
  end if

  if (l .ne. flen) then
    ne = ne + 1
    write(*, 2007) 10, i, l
  end if

  DO i = 1, n
  if (flist(i) .ne. fields(i)) then
    ne = ne + 1
    write(*, 2008) i, 10, flist(i)
  end if
  end do

! 11: fgd_get_nframes check
  n = fgd_get_nframes(d)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 11, e
  end if

  if (n .ne. 10) then
    ne = ne + 1
    write(*, 2002) 11, n
  end if

! 12: fgd_get_spf check
  n = fgd_get_spf(d, 'data')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 12, e
  end if

  if (n .ne. 8) then
    ne = ne + 1
    write(*, 2002) 12, n
  end if

! 13: fgd_putdata_i1 check
  ci1 = (/ 13, 14, 15, 16, 17, 18, 19, 20 /)
  n = fgd_putdata_i1(d, 'data', 5, 1, 0, 4, ci1)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 13, e
  end if

  if (n .ne. 4) then
    ne = ne + 1
    write(*, 2002) 13, n
  end if

  n = fgd_getdata_i1(d, 'data', 5, 0, 1, 0, ci1)

  DO i = 1, 8
  if (((i .EQ. 1 .OR. i .GT. 5) .AND. ci1(i) .ne. 40 + i) .OR. &
  (i .GT. 1 .AND. i .LT. 6) .AND. ci1(i) .ne. 11 + i) then
    ne = ne + 1
    write(*, 2004) i, 13, ci1(i)
  end if
  end do

! 118: fgd_putdata_i2 check
  ci2 = (/ 23, 24, 25, 26, 27, 28, 29, 30 /)
  n = fgd_putdata_i2(d, 'data', 5, 1, 0, 4, ci2)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 118, e
  end if

  if (n .ne. 4) then
    ne = ne + 1
    write(*, 2002) 118, n
  end if

  n = fgd_getdata_i2(d, 'data', 5, 0, 1, 0, ci2)

  DO i = 1, 8
  if (((i .EQ. 1 .OR. i .GT. 5) .AND. ci2(i) .ne. 40 + i) .OR. &
  (i .GT. 1 .AND. i .LT. 6) .AND. ci2(i) .ne. 21 + i) then
    ne = ne + 1
    write(*, 2004) i, 118, ci2(i)
  end if
  end do

! 119: fgd_putdata_i4 check
  ci4 = (/ 33, 34, 35, 36, 37, 38, 39, 40 /)
  n = fgd_putdata_i4(d, 'data', 5, 1, 0, 4, ci4)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 119, e
  end if

  if (n .ne. 4) then
    ne = ne + 1
    write(*, 2002) 119, n
  end if

  n = fgd_getdata_i4(d, 'data', 5, 0, 1, 0, ci4)

  DO i = 1, 8
  if (((i .EQ. 1 .OR. i .GT. 5) .AND. ci4(i) .ne. 40 + i) .OR. &
  (i .GT. 1 .AND. i .LT. 6) .AND. ci4(i) .ne. 31 + i) then
    ne = ne + 1
    write(*, 2004) i, 119, ci4(i)
  end if
  end do

! 120: fgd_putdata_i8 check
  ci8 = (/ 43, 44, 45, 46, 47, 48, 49, 50 /)
  n = fgd_putdata_i8(d, 'data', 5, 1, 0, 4, ci8)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 120, e
  end if

  if (n .ne. 4) then
    ne = ne + 1
    write(*, 2002) 120, n
  end if

  n = fgd_getdata_i8(d, 'data', 5, 0, 1, 0, ci8)

  DO i = 1, 8
  if (((i .EQ. 1 .OR. i .GT. 5) .AND. ci8(i) .ne. 40 + i) .OR. &
  (i .GT. 1 .AND. i .LT. 6) .AND. ci8(i) .ne. 41 + i) then
    ne = ne + 1
    write(*, 2004) i, 120, ci8(i)
  end if
  end do

! 121: fgd_putdata_r4 check
  cr4 = (/ 33, 34, 35, 36, 37, 38, 39, 40 /)
  n = fgd_putdata_r4(d, 'data', 5, 1, 0, 4, cr4)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 121, e
  end if

  if (n .ne. 4) then
    ne = ne + 1
    write(*, 2002) 121, n
  end if

  n = fgd_getdata_r4(d, 'data', 5, 0, 1, 0, cr4)

  DO i = 1, 8
  if (((i .EQ. 1 .OR. i .GT. 5) .AND. abs(cr4(i) - 40 - i) .gt. 1e-5) .OR. &
  (i .GT. 1 .AND. i .LT. 6) .AND. abs(cr4(i) - 31 - i) .gt. 1e-5) then
    ne = ne + 1
    write(*, 2010) i, 121, cr4(i)
  end if
  end do

! 122: fgd_putdata_r8 check
  cr8 = (/ 43, 44, 45, 46, 47, 48, 49, 50 /)
  n = fgd_putdata_r8(d, 'data', 5, 1, 0, 4, cr8)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 122, e
  end if

  if (n .ne. 4) then
    ne = ne + 1
    write(*, 2002) 122, n
  end if

  n = fgd_getdata_r8(d, 'data', 5, 0, 1, 0, cr8)

  DO i = 1, 8
  if (((i .EQ. 1 .OR. i .GT. 5) .AND. abs(cr8(i) - 40 - i) .gt. 1e-5) .OR. &
  (i .GT. 1 .AND. i .LT. 6) .AND. abs(cr8(i) - 41 - i) .gt. 1e-5) then
    ne = ne + 1
    write(*, 2010) i, 122, cr8(i)
  end if
  end do

! 123: fgd_putdata_c8 check
  cc8 = (/ 53, 54, 55, 56, 57, 58, 59, 60 /)
  n = fgd_putdata_c8(d, 'data', 5, 1, 0, 4, cc8)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 123, e
  end if

  if (n .ne. 4) then
    ne = ne + 1
    write(*, 2002) 123, n
  end if

  n = fgd_getdata_c8(d, 'data', 5, 0, 1, 0, cc8)

  DO i = 1, 8
  if (((i .EQ. 1 .OR. i .GT. 5) .AND. abs(cc8(i) - 40 - i) .gt. 1e-5) .OR. &
  (i .GT. 1 .AND. i .LT. 6) .AND. abs(cc8(i) - 51 - i) .gt. 1e-5) then
    ne = ne + 1
    write(*, 2011) i, 123, real(real(cc8(i))), real(aimag(cc8(i)))
  end if
  end do

! 124: fgd_putdata_c16 check
  cc16 = (/ 63, 64, 65, 66, 67, 68, 69, 70 /)
  n = fgd_putdata_c16(d, 'data', 5, 1, 0, 4, cc16)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 124, e
  end if

  if (n .ne. 4) then
    ne = ne + 1
    write(*, 2002) 124, n
  end if

  n = fgd_getdata_c16(d, 'data', 5, 0, 1, 0, cc16)

  DO i = 1, 8
  if (((i .EQ. 1 .OR. i .GT. 5) .AND. abs(cc16(i) - 40 - i) .gt. 1e-5) .OR. &
  (i .GT. 1 .AND. i .LT. 6) .AND. abs(cc16(i) - 61 - i) .gt. 1e-5) then
    ne = ne + 1
    write(*, 2011) i, 124, real(real(cc16(i))), real(aimag(cc16(i)))
  end if
  end do

! 14: fgd_error_string check
  n = fgd_getdata_n(d, 'x', 5, 0, 1, 0)
  call fgd_error_string(d, str, GD_FIELD_LEN)

  if (str .ne. 'Field not found: x') then
    ne = ne + 1
    write(*, 2009) 14, str
  end if

! 15: fgd_get_entry_type check
  n = fgd_get_entry_type(d, 'data')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 15, e
  end if

  if (n .ne. GD_RAW_ENTRY) then
    ne = ne + 1
    write(*, 2002) 15, n
  end if

! 16: fgd_get_entry (raw) check
  n = fgd_get_entry(d, 'data', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 16, e
  end if

  if (n .ne. GD_RAW_ENTRY) then
    ne = ne + 1
    write(*, 2007) 16, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 16, 2, ent%fragment_index
  end if

  if (ent%spf .ne. 8) then
    ne = ne + 1
    write(*, 2007) 16, 3, ent%spf
  end if

  if (ent%data_type .ne. GD_INT8) then
    ne = ne + 1
    write(*, 2007) 16, 4, ent%data_type
  end if

! 18: fgd_get_entry (lincom) check
  n = fgd_get_entry(d, 'lincom', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 18, e
  end if

  if (n .ne. GD_LINCOM_ENTRY) then
    ne = ne + 1
    write(*, 2007) 18, 1, n
  end if

  if (ent%n_fields .ne. 3) then
    ne = ne + 1
    write(*, 2007) 18, 2, ent%n_fields
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 18, 3, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'data') then
    ne = ne + 1
    write(*, 2008) 18, 4, ent%field(1)
  end if

  if (ent%field(2) .ne. 'INDEX') then
    ne = ne + 1
    write(*, 2008) 18, 5, ent%field(2)
  end if

  if (ent%field(3) .ne. 'linterp') then
    ne = ne + 1
    write(*, 2008) 18, 6, ent%field(3)
  end if

  if (ent%comp_scal .ne. 1) then
    ne = ne + 1
    write(*, 2007) 18, 7, ent%comp_scal
  end if

  if (ent%scalar(3) .ne. 'const') then
    ne = ne + 1
    write(*, 2008) 18, 8, ent%scalar(3)
  end if

  if (ent%scalar(6) .ne. 'const') then
    ne = ne + 1
    write(*, 2008) 18, 9, ent%scalar(6)
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
    write(*, 2010) i * 2 - 1, 18, ent%m(i)
  end if
  if (abs(ent%cb(i) - cq(i * 2)) > 0.001) then
    ne = ne + 1
    write(*, 2010) i * 2, 18, ent%b(i)
  end if
  end do

! 20: fgd_get_entry (polynom) check
  n = fgd_get_entry(d, 'polynom', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 20, e
  end if

  if (n .ne. GD_POLYNOM_ENTRY) then
    ne = ne + 1
    write(*, 2007) 20, 1, n
  end if

  if (ent%poly_ord .ne. 5) then
    ne = ne + 1
    write(*, 2007) 20, 2, ent%n_fields
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 20, 3, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'data') then
    ne = ne + 1
    write(*, 2008) 20, 4, ent%field(1)
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
    write(*, 2011) i, 30, real(real(ent%ca(i))), real(aimag(ent%ca(i)))
  end if
  end do

! 21: fgd_get_entry (linterp) check
  n = fgd_get_entry(d, 'linterp', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 21, e
  end if

  if (n .ne. GD_LINTERP_ENTRY) then
    ne = ne + 1
    write(*, 2007) 21, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 21, 2, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'data') then
    ne = ne + 1
    write(*, 2008) 21, 3, ent%field(1)
  end if

  if (ent%field(2) .ne. '/look/up/file') then
    ne = ne + 1
    write(*, 2008) 21, 4, ent%field(2)
  end if

! 22: fgd_get_entry (bit) check
  n = fgd_get_entry(d, 'bit', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 22, e
  end if

  if (n .ne. GD_BIT_ENTRY) then
    ne = ne + 1
    write(*, 2007) 22, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 22, 2, ent%fragment_index
  end if

  if (ent%bitnum .ne. 3) then
    ne = ne + 1
    write(*, 2007) 22, 3, ent%bitnum
  end if

  if (ent%numbits .ne. 4) then
    ne = ne + 1
    write(*, 2007) 22, 4, ent%numbits
  end if

  if (ent%field(1) .ne. 'data') then
    ne = ne + 1
    write(*, 2008) 22, 5, ent%field(1)
  end if

! 23: fgd_get_entry (Sbit) check
  n = fgd_get_entry(d, 'sbit', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 23, e
  end if

  if (n .ne. GD_SBIT_ENTRY) then
    ne = ne + 1
    write(*, 2007) 23, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 23, 2, ent%fragment_index
  end if

  if (ent%numbits .ne. 6) then
    ne = ne + 1
    write(*, 2007) 23, 3, ent%numbits
  end if

  if (ent%bitnum .ne. 5) then
    ne = ne + 1
    write(*, 2007) 23, 4, ent%bitnum
  end if

  if (ent%field(1) .ne. 'data') then
    ne = ne + 1
    write(*, 2008) 23, 5, ent%field(1)
  end if

! 24: fgd_get_entry (multiply) check
  n = fgd_get_entry(d, 'mult', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 24, e
  end if

  if (n .ne. GD_MULTIPLY_ENTRY) then
    ne = ne + 1
    write(*, 2007) 24, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 24, 2, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'data') then
    ne = ne + 1
    write(*, 2008) 24, 3, ent%field(1)
  end if

  if (ent%field(2) .ne. 'sbit') then
    ne = ne + 1
    write(*, 2008) 24, 4, ent%field(2)
  end if

! 25: fgd_get_entry (phase) check
  n = fgd_get_entry(d, 'phase', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 25, e
  end if

  if (n .ne. GD_PHASE_ENTRY) then
    ne = ne + 1
    write(*, 2007) 25, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 25, 2, ent%fragment_index
  end if

  if (ent%shift .ne. 11) then
    ne = ne + 1
    write(*, 2007) 25, 3, ent%shift
  end if

  if (ent%field(1) .ne. 'data') then
    ne = ne + 1
    write(*, 2008) 25, 4, ent%field(1)
  end if

! 26: fgd_get_entry (const) check
  n = fgd_get_entry(d, 'const', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 26, e
  end if

  if (n .ne. GD_CONST_ENTRY) then
    ne = ne + 1
    write(*, 2007) 26, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 26, 2, nfragment_index
  end if

  if (ent%data_type .ne. GD_FLOAT64) then
    ne = ne + 1
    write(*, 2007) 26, 3, ent%data_type
  end if

! 27: fgd_get_fragment_index check
  n = fgd_get_fragment_index(d, 'const')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 27, e
  end if

  if (n .ne. 0) then
    ne = ne + 1
    write(*, 2002) 27, n
  end if

! 28: fgd_add_raw check
  call fgd_add_raw(d, 'new1', GD_FLOAT64, 3, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 28, 1, e
  end if

  n = fgd_get_entry(d, 'new1', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 28, 2, e
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 28, 3, ent%fragment_index
  end if

  if (ent%spf .ne. 3) then
    ne = ne + 1
    write(*, 2007) 28, 4, ent%spf
  end if

  if (ent%data_type .ne. GD_FLOAT64) then
    ne = ne + 1
    write(*, 2007) 28, 5, i
  end if

  if (n .ne. GD_RAW_ENTRY) then
    ne = ne + 1
    write(*, 2007) 28, 6, n
  end if

! 29: fgd_add_lincom check
  call fgd_add_lincom(d, 'new2', 2, 'in1', 9.9d0, 8.8d0, &
  'in2', 7.7d0, 6.6d0, '', 0d0, 0d0, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 29, 1, e
  end if

  n = fgd_get_entry(d, 'new2', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 29, 2, e
  end if

  if (n .ne. GD_LINCOM_ENTRY) then
    ne = ne + 1
    write(*, 2007) 29, 3, n
  end if

  if (ent%n_fields .ne. 2) then
    ne = ne + 1
    write(*, 2007) 29, 4, ent%n_fields
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 29, 5, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 2008) 29, 6, ent%field(1)
  end if

  if (ent%field(2) .ne. 'in2') then
    ne = ne + 1
    write(*, 2008) 29, 7, ent%field(2)
  end if

  if (ent%comp_scal .ne. 0) then
    ne = ne + 1
    write(*, 2007) 29, 8, ent%comp_scal
  end if

  q = (/ 9.9, 8.8, 7.7, 6.6, 5.5, 5.5 /)
  do i=1,2
  if (abs(ent%m(i) - q(i * 2 - 1)) > 0.001) then
    ne = ne + 1
    write(*, 2010) i * 2 - 1, 29, ent%m(i)
  end if
  if (abs(ent%b(i) - q(i * 2)) > 0.001) then
    ne = ne + 1
    write(*, 2010) i * 2, 29, ent%b(i)
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
    write(*, 2006) 30, 1, e
  end if

  n = fgd_get_entry(d, 'new3', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 30, 2, e
  end if

  if (ent%n_fields .ne. 2) then
    ne = ne + 1
    write(*, 2007) 30, 1, ent%n_fields
  end if

  if (n .ne. GD_LINCOM_ENTRY) then
    ne = ne + 1
    write(*, 2007) 30, 2, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 30, 3, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 2008) 30, 4, ent%field(1)
  end if

  if (ent%field(2) .ne. 'in2') then
    ne = ne + 1
    write(*, 2008) 30, 5, ent%field(2)
  end if

  if (ent%comp_scal .ne. 1) then
    ne = ne + 1
    write(*, 2007) 29, 6, ent%comp_scal
  end if

  cq(1) = dcmplx(1.1, 1.2)
  cq(2) = dcmplx(1.3, 1.4)
  cq(3) = dcmplx(1.4, 1.5)
  cq(4) = dcmplx(1.6, 1.7)
  do i=1,2
  if (abs(ent%cm(i) - cq(i * 2 - 1)) > 0.001) then
    ne = ne + 1
    write(*, 2011) i * 2 - 1, 30, real(real(ent%cm(i))), real(aimag(ent%cm(i)))
  end if
  if (abs(ent%cb(i) - cq(i * 2)) > 0.001) then
    ne = ne + 1
    write(*, 2011) i * 2, 30, real(real(ent%cb(i))), real(aimag(ent%cb(i)))
  end if
  end do

! 31: fgd_add_polynom check
  call fgd_add_polynom(d, 'new4', 3, 'in1', 3d3, 4d4, 5d5, 6d6, 0d0, 0d0, &
  0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 31, 1, e
  end if

  n = fgd_get_entry(d, 'new4', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 31, 2, e
  end if

  if (n .ne. GD_POLYNOM_ENTRY) then
    ne = ne + 1
    write(*, 2007) 31, 1, n
  end if

  if (ent%poly_ord .ne. 3) then
    ne = ne + 1
    write(*, 2007) 31, 2, ent%poly_ord
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 31, 3, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 2008) 31, 4, ent%field(1)
  end if

  if (ent%comp_scal .ne. 0) then
    ne = ne + 1
    write(*, 2007) 31, 5, ent%comp_scal
  end if

  q = (/ 3d3, 4d4, 5d5, 6d6, 5.5d0, 5.5d0 /)
  DO i=1,4
  if (abs(ent%a(i) - q(i)) > 0.001) then
    ne = ne + 1
    write(*, 2010) i, 31, ent%a(i)
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
    write(*, 2006) 32, 1, e
  end if

  n = fgd_get_entry(d, 'new5', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 32, 2, e
  end if

  if (n .ne. GD_POLYNOM_ENTRY) then
    ne = ne + 1
    write(*, 2007) 32, 1, n
  end if

  if (ent%poly_ord .ne. 3) then
    ne = ne + 1
    write(*, 2007) 32, 2, ent%poly_ord
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 32, 3, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 2008) 32, 4, ent%field(1)
  end if

  if (ent%comp_scal .ne. 1) then
    ne = ne + 1
    write(*, 2007) 31, 5, ent%comp_scal
  end if

  cq(1) = dcmplx(3.1, 7.0)
  cq(2) = dcmplx(4.2, 8.0)
  cq(3) = dcmplx(5.2, 9.0)
  cq(4) = dcmplx(6.3, 4.4)
  DO i=1,4
  if (abs(ent%ca(i) - cq(i)) > 0.001) then
    ne = ne + 1
    write(*, 2011) i, 32, real(real(ent%ca(i))), real(aimag(ent%ca(i)))
  end if
  end do

! 33: fgd_add_linterp check
  call fgd_add_linterp(d, "new6", "in", "./some/table", 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 33, 1, e
  end if

  n = fgd_get_entry(d, 'new6', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 33, 2, e
  end if

  if (n .ne. GD_LINTERP_ENTRY) then
    ne = ne + 1
    write(*, 2007) 33, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 33, 2, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in') then
    ne = ne + 1
    write(*, 2008) 33, 3, ent%field(1)
  end if

  if (ent%field(2) .ne. './some/table') then
    ne = ne + 1
    write(*, 2008) 33, 4, ent%field(2)
  end if

! 34: fgd_add_bit check
  call fgd_add_bit(d, "new7", "in", 13, 12, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 34, 1, e
  end if

  n = fgd_get_entry(d, 'new7', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 34, 2, e
  end if

  if (n .ne. GD_BIT_ENTRY) then
    ne = ne + 1
    write(*, 2007) 34, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 34, 2, ent%fragment_index
  end if

  if (ent%numbits .ne. 12) then
    ne = ne + 1
    write(*, 2007) 34, 3, ent%numbits
  end if

  if (ent%bitnum .ne. 13) then
    ne = ne + 1
    write(*, 2007) 34, 4, ent%bitnum
  end if

  if (ent%field(1) .ne. 'in') then
    ne = ne + 1
    write(*, 2008) 34, 5, ent%field(1)
  end if

! 35: fgd_add_sbit check
  call fgd_add_sbit(d, "new8", "in", 13, 12, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 35, 1, e
  end if

  n = fgd_get_entry(d, "new8", ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 35, 2, e
  end if

  if (n .ne. GD_SBIT_ENTRY) then
    ne = ne + 1
    write(*, 2007) 35, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 35, 2, ent%fragment_index
  end if

  if (ent%numbits .ne. 12) then
    ne = ne + 1
    write(*, 2007) 35, 3, ent%numbits
  end if

  if (ent%bitnum .ne. 13) then
    ne = ne + 1
    write(*, 2007) 35, 4, ent%bitnum
  end if

  if (ent%field(1) .ne. 'in') then
    ne = ne + 1
    write(*, 2008) 35, 5, ent%field(1)
  end if

! 36: fgd_add_multiply check
  call fgd_add_multiply(d, 'new9', 'in1', 'in2', 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 36, 1, e
  end if

  n = fgd_get_entry(d, 'new9', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 36, 2, e
  end if

  if (n .ne. GD_MULTIPLY_ENTRY) then
    ne = ne + 1
    write(*, 2007) 36, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 36, 2, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 2008) 36, 3, ent%field(1)
  end if

  if (ent%field(2) .ne. 'in2') then
    ne = ne + 1
    write(*, 2008) 36, 4, ent%field(2)
  end if

! 37: fgd_add_phase check
  call fgd_add_phase(d, 'new10', 'in1', 22, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 37, 1, e
  end if

  n = fgd_get_entry(d, 'new10', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 37, 2, e
  end if

  if (n .ne. GD_PHASE_ENTRY) then
    ne = ne + 1
    write(*, 2007) 37, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 37, 2, ent%fragment_index
  end if

  if (ent%shift .ne. 22) then
    ne = ne + 1
    write(*, 2007) 37, 3, ent%shift
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 2008) 37, 4, ent%field(1)
  end if

! 38: fgd_add_const check
  call fgd_add_const(d, 'new11', GD_FLOAT64, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 38, 1, e
  end if

  n = fgd_get_entry(d, 'new11', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 38, 2, e
  end if

  if (n .ne. GD_CONST_ENTRY) then
    ne = ne + 1
    write(*, 2007) 38, 1, n
  end if

  if (ent%fragment_index .ne. ent%fragment_index) then
    ne = ne + 1
    write(*, 2007) 38, 2, ent%fragment_index
  end if

  if (ent%data_type .ne. GD_FLOAT64) then
    ne = ne + 1
    write(*, 2007) 38, 3, ent%data_type
  end if

  n = fgd_get_constant_r4(d, 'new11', fl)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 38, 3, e
  end if

  if (abs(fl) > 1e-5) then
    ne = ne + 1
    write(*, 2005) 38, fl
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
    write(*, 2006) 125, 1, e
  end if

  n = fgd_get_entry(d, 'new13', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 125, 2, e
  end if

  if (n .ne. GD_PHASE_ENTRY) then
    ne = ne + 1
    write(*, 2007) 125, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 125, 2, ent%fragment_index
  end if

  if (ent%shift .ne. 33) then
    ne = ne + 1
    write(*, 2007) 125, 3, ent%shift
  end if

  if (ent%field(1) .ne. 'new9') then
    ne = ne + 1
    write(*, 2008) 125, 4, ent%field(1)
  end if

! 39: fgd_get_fragmentname check
  str = fgd_get_fragmentname(d, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 39, e
  end if

  if (str .ne. 'test95_dirfile/format') then
    ne = ne + 1
    write(*, 2009), 39, str
  end if

! 40: fgd_get_nfragments check
  n = fgd_get_nfragments(d)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 40, e
  end if

  if (n .ne. 1) then
    ne = ne + 1
    write(*, 2002), 40, n
  end if

! 41: fgd_include check
  call fgd_include(d, 'form2', 0, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 41, 3, e
  end if

  n = fgd_get_constant_i1(d, 'const2', ci1(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 41, 3, e
  end if

  if (ci1(1) .ne. -19) then
    ne = ne + 1
    write(*, 2004) 1, 41, ci1(1)
  end if

! 42: fgd_get_nfields_by_type check
  n = fgd_get_nfields_by_type(d, GD_LINCOM_ENTRY)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 42, e
  end if

  if (n .ne. 3) then
    ne = ne + 1
    write(*, 2002), 42, n
  end if

! 43: fgd_get_field_list_by_type check
  fields(1) = 'lincom'
  fields(2) = 'new2'
  fields(3) = 'new3'
  l = flen
  call fgd_get_field_list_by_type(flist, d, GD_LINCOM_ENTRY, l)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 43, e
  end if

  if (l .ne. flen) then
    ne = ne + 1
    write(*, 2002) 43, l
  end if

  do i = 1, n
  if (flist(i) .ne. fields(i)) then
    ne = ne + 1
    write(*, 2008) i, 43, flist(i)
  end if
  end do

! 44: fgd_get_nvectors check
  n = fgd_get_nvectors(d)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 44, e
  end if

  if (n .ne. 20) then
    ne = ne + 1
    write(*, 2002), 44, n
  end if

! 45: fgd_get_vector_list check
  fields = (/ 'INDEX  ', 'bit    ', 'data   ', 'lincom ', 'linterp', &
  'mult   ', 'new1   ', 'new10  ', 'new13  ', 'new2   ', 'new3   ', 'new4   ', &
  'new5   ', 'new6   ', 'new7   ', 'new8   ', 'new9   ', 'phase  ', 'polynom', &
  'sbit   ', 'string ' /)
  l = flen
  call fgd_get_vector_list(flist, d, l)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 45, e
  end if

  if (l .ne. flen) then
    ne = ne + 1
    write(*, 2002) 45, l
  end if
 
  do i=1,n
  if (flist(i) .ne. fields(i)) then
    ne = ne + 1
    write(*, 2008) i, 45, flist(i)
  end if
  end do

! 46: fgd_madd_lincom check
  call fgd_madd_lincom(d, 'data', 'mnew1', 2, 'in1', 9.9d0, 8.8d0, &
  'in2', 7.7d0, 6.6d0, '', 0d0, 0d0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 46, 1, e
  end if

  n = fgd_get_entry(d, 'data/mnew1', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 46, 2, e
  end if

  if (n .ne. GD_LINCOM_ENTRY) then
    ne = ne + 1
    write(*, 2007) 46, 3, n
  end if

  if (ent%n_fields .ne. 2) then
    ne = ne + 1
    write(*, 2007) 46, 4, ent%n_fields
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 46, 5, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 2008) 46, 6, ent%field(1)
  end if

  if (ent%field(2) .ne. 'in2') then
    ne = ne + 1
    write(*, 2008) 46, 7, ent%field(2)
  end if

  if (ent%comp_scal .ne. 0) then
    ne = ne + 1
    write(*, 2007) 46, 8, ent%comp_scal
  end if

  q = (/ 9.9, 8.8, 7.7, 6.6, 5.5, 5.5 /)
  DO i=1,2
  if (abs(ent%m(i) - q(i *  2 - 1)) > 0.001) then
    ne = ne + 1
    write(*, 2010) i * 2 - 1, 46, ent%m(i)
  end if
  if (abs(ent%b(i) - q(i * 2)) > 0.001) then
    ne = ne + 1
    write(*, 2010) i * 2, 46, ent%m(i)
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
    write(*, 2006) 47, 1, e
  end if

  n = fgd_get_entry(d, 'data/mnew2', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 47, e
  end if

  if (n .ne. GD_LINCOM_ENTRY) then
    ne = ne + 1
    write(*, 2007) 47, 1, n
  end if

  if (ent%n_fields .ne. 2) then
    ne = ne + 1
    write(*, 2007) 47, 2, ent%n_fields
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 47, 3, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 2008) 47, 4, ent%field(1)
  end if

  if (ent%field(2) .ne. 'in2') then
    ne = ne + 1
    write(*, 2008) 47, 5, ent%field(2)
  end if

  if (ent%comp_scal .ne. 1) then
    ne = ne + 1
    write(*, 2007) 47, 6, ent%comp_scal
  end if

  cq(1) = dcmplx(1.1, 1.2)
  cq(2) = dcmplx(1.3, 1.4)
  cq(3) = dcmplx(1.4, 1.5)
  cq(4) = dcmplx(1.6, 1.7)
  DO i=1,2
  if (abs(ent%cm(i) - cq(i * 2 - 1)) > 0.001) then
    ne = ne + 1
    write(*, 2011) i * 2 - 1, 47, real(real(ent%cm(i))), real(aimag(ent%cm(i)))
  end if
  if (abs(ent%cb(i) - cq(i * 2)) > 0.001) then
    ne = ne + 1
    write(*, 2011) i * 2, 47, real(real(ent%cb(i))), real(aimag(ent%cb(i)))
  end if
  end do

! 48: fgd_madd_polynom check
  call fgd_madd_polynom(d, 'data', 'mnew3', 3, 'in1', 3d3, 4d4, 5d5, &
     6d6, 0d0, 0d0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 48, 1, e
  end if

  n = fgd_get_entry(d, 'data/mnew3', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 48, 2, e
  end if

  if (n .ne. GD_POLYNOM_ENTRY) then
    ne = ne + 1
    write(*, 2007) 48, 1, n
  end if

  if (ent%poly_ord .ne. 3) then
    ne = ne + 1
    write(*, 2007) 48, 2, ent%poly_ord
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 48, 3, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 2008) 48, 4, ent%field(1)
  end if

  q = (/ 3d3, 4d4, 5d5, 6d6, 5.5d0, 5.5d0 /)
  DO i=1,4
  if (abs(ent%a(i) - q(i)) > 0.001) then
    ne = ne + 1
    write(*, 2010) i, 48, ent%a(i)
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
    write(*, 2006) 49, 1, e
  end if

  n = fgd_get_entry(d, 'data/mnew5', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 49, 2, e
  end if

  if (n .ne. GD_POLYNOM_ENTRY) then
    ne = ne + 1
    write(*, 2007) 49, 1, n
  end if

  if (ent%poly_ord .ne. 3) then
    ne = ne + 1
    write(*, 2007) 49, 2, ent%poly_ord
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 49, 3, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 2008) 49, 4, ent%field(1)
  end if

  cq(1) = dcmplx(1.1, 0.0)
  cq(2) = dcmplx(2.2, 0.0)
  cq(3) = dcmplx(2.2, 0.0)
  cq(4) = dcmplx(3.3, 4.4)
  DO i=1,4
  if (abs(ent%ca(i) - cq(i)) > 0.001) then
    ne = ne + 1
    write(*, 2011) i, 49, real(real(ent%ca(i))), real(aimag(ent%ca(i)))
  end if
  end do

! 50: fgd_madd_linterp check
  call fgd_madd_linterp(d, "data", "mnew6", "in", "./more/table")
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 50, 1, e
  end if

  n = fgd_get_entry(d, 'data/mnew6', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 50, 2, e
  end if

  if (n .ne. GD_LINTERP_ENTRY) then
    ne = ne + 1
    write(*, 2007) 50, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 50, 2, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in') then
    ne = ne + 1
    write(*, 2008) 50, 3, ent%field(1)
  end if

  if (ent%field(2) .ne. './more/table') then
    ne = ne + 1
    write(*, 2008) 50, 4, ent%field(2)
  end if

! 51: fgd_madd_bit check
  call fgd_madd_bit(d, "data", "mnew7", "in", 13, 12)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 51, 1, e
  end if

  n = fgd_get_entry(d, 'data/mnew7', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 51, 2, e
  end if

  if (n .ne. GD_BIT_ENTRY) then
    ne = ne + 1
    write(*, 2007) 51, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 51, 2, ent%fragment_index
  end if

  if (ent%numbits .ne. 12) then
    ne = ne + 1
    write(*, 2007) 51, 3, ent%numbits
  end if

  if (ent%bitnum .ne. 13) then
    ne = ne + 1
    write(*, 2007) 51, 4, ent%bitnum
  end if

  if (ent%field(1) .ne. 'in') then
    ne = ne + 1
    write(*, 2008) 51, 5, ent%field(1)
  end if

! 52: fgd_madd_sbit check
  call fgd_madd_sbit(d, "data", "mnew8", "in", 13, 12)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 52, 1, e
  end if

  n = fgd_get_entry(d, 'data/mnew8', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 52, 2, e
  end if

  if (n .ne. GD_SBIT_ENTRY) then
    ne = ne + 1
    write(*, 2007) 52, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 52, 2, ent%fragment_index
  end if

  if (ent%numbits .ne. 12) then
    ne = ne + 1
    write(*, 2007) 52, 3, ent%numbits
  end if

  if (ent%bitnum .ne. 13) then
    ne = ne + 1
    write(*, 2007) 52, 4, ent%bitnum
  end if

  if (ent%field(1) .ne. 'in') then
    ne = ne + 1
    write(*, 2008) 52, 5, ent%field(1)
  end if

! 53: fgd_madd_multiply check
  call fgd_madd_multiply(d, 'data', 'mnew9', 'in1', 'in2')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 53, 1, e
  end if

  n = fgd_get_entry(d, 'data/mnew9', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 53, 2, e
  end if

  if (n .ne. GD_MULTIPLY_ENTRY) then
    ne = ne + 1
    write(*, 2007) 53, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 53, 2, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 2008) 53, 3, ent%field(1)
  end if

  if (ent%field(2) .ne. 'in2') then
    ne = ne + 1
    write(*, 2008) 53, 4, ent%field(2)
  end if

! 54: fgd_madd_phase check
  call fgd_madd_phase(d, 'data', 'mnew10', 'in1', 22)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 54, 1, e
  end if

  n = fgd_get_entry(d, 'data/mnew10', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 54, 2, e
  end if

  if (n .ne. GD_PHASE_ENTRY) then
    ne = ne + 1
    write(*, 2007) 54, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 54, 2, ent%fragment_index
  end if

  if (ent%shift .ne. 22) then
    ne = ne + 1
    write(*, 2007) 54, 3, ent%shift
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 2008) 54, 4, ent%field(1)
  end if

! 55: fgd_madd_const check
  call fgd_madd_const(d, 'data', 'mnew11', GD_FLOAT64)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 55, 1, e
  end if

  n = fgd_get_entry(d, 'data/mnew11', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 55, 2, e
  end if

  if (n .ne. GD_CONST_ENTRY) then
    ne = ne + 1
    write(*, 2007) 55, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 55, 2, ent%fragment_index
  end if

  if (ent%data_type .ne. GD_FLOAT64) then
    ne = ne + 1
    write(*, 2007) 55, 3, ent%data_type
  end if

  n = fgd_get_constant_r4(d, 'data/mnew11', fl)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 55, 3, e
  end if

  if (abs(fl) > 0.001) then
    ne = ne + 1
    write(*, 2005) 55, fl
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
    write(*, 2006) 126, 1, e
  end if

  n = fgd_get_entry(d, 'data/mnew4', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 126, 2, e
  end if

  if (n .ne. GD_PHASE_ENTRY) then
    ne = ne + 1
    write(*, 2007) 126, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 126, 2, ent%fragment_index
  end if

  if (ent%shift .ne. 33) then
    ne = ne + 1
    write(*, 2007) 126, 3, ent%shift
  end if

  if (ent%field(1) .ne. 'data/mnew10') then
    ne = ne + 1
    write(*, 2008) 126, 4, ent%field(1)
  end if

! 56: fgd_get_string check
  n = fgd_get_string(d, 'string', GD_FIELD_LEN, str)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 56, e
  end if

  if (n .ne. 17) then
    ne = ne + 1
    write(*, 2002) 56, n
  end if

  if (str .ne. "Zaphod Beeblebrox") then
    ne = ne + 1
    write(*, 2009) 56, str
  end if

! 57: fgd_add_string check
  call fgd_add_string(d, 'new12', 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 57, 1, e
  end if

  n = fgd_get_string(d, 'new12', GD_FIELD_LEN, str)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 57, 2, e
  end if

  if (str .ne. "") then
    ne = ne + 1
    write(*, 2009) 57, str
  end if

! 58: fgd_madd_string check
  call fgd_madd_string(d, "data", 'mnew12')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 58, 1, e
  end if

  n = fgd_get_string(d, 'data/mnew12', GD_FIELD_LEN, str)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 58, 2, e
  end if

  if (str .ne. "") then
    ne = ne + 1
    write(*, 2009) 58, str
  end if

! 59: fgd_add_spec check
  call fgd_add_spec(d, 'lorem STRING "Lorem ipsum"', 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 59, 1, e
  end if

  n = fgd_get_string(d, 'lorem', GD_FIELD_LEN, str)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 59, 2, e
  end if

  if (str .ne. "Lorem ipsum") then
    ne = ne + 1
    write(*, 2009) 59, str
  end if

! 60: fgd_madd_spec check
  call fgd_madd_spec(d, 'ipsum STRING "dolor sit amet."', 'lorem')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 60, 1, e
  end if

  n = fgd_get_string(d, 'lorem/ipsum', GD_FIELD_LEN, str)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 60, 2, e
  end if

  if (str .ne. "dolor sit amet.") then
    ne = ne + 1
    write(*, 2009) 60, str
  end if

! 61: fgd_put_constant_i1 check
  ci1(1) = 61
  n = fgd_put_constant_i1(d, 'const', ci1(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 61, 1, e
  end if

  if (n .ne. 0) then
    ne = ne + 1
    write(*, 2002) 61, n
  end if

  n = fgd_get_constant_r4(d, 'const', fl)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 61, 2, e
  end if

  if (abs(fl - 61) > 0.001) then
    ne = ne + 1
    write(*, 2005) 61, fl
  end if

! 127: fgd_put_constant_i2 check
  ci2(1) = 127
  n = fgd_put_constant_i2(d, 'const', ci2(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 127, 1, e
  end if

  if (n .ne. 0) then
    ne = ne + 1
    write(*, 2002) 127, n
  end if

  n = fgd_get_constant_r4(d, 'const', fl)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 127, 2, e
  end if

  if (abs(fl - 127) > 0.001) then
    ne = ne + 1
    write(*, 2005) 127, fl
  end if

! 128: fgd_put_constant_i4 check
  ci4(1) = 128
  n = fgd_put_constant_i4(d, 'const', ci4(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 128, 1, e
  end if

  if (n .ne. 0) then
    ne = ne + 1
    write(*, 2002) 128, n
  end if

  n = fgd_get_constant_r4(d, 'const', fl)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 128, 2, e
  end if

  if (abs(fl - 128) > 0.001) then
    ne = ne + 1
    write(*, 2005) 128, fl
  end if

! 129: fgd_put_constant_i8 check
  ci8(1) = 129
  n = fgd_put_constant_i8(d, 'const', ci8(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 129, 1, e
  end if

  if (n .ne. 0) then
    ne = ne + 1
    write(*, 2002) 129, n
  end if

  n = fgd_get_constant_r4(d, 'const', fl)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 129, 2, e
  end if

  if (abs(fl - 129) > 0.001) then
    ne = ne + 1
    write(*, 2005) 129, fl
  end if

! 130: fgd_put_constant_r4 check
  cr4(1) = 130
  n = fgd_put_constant_r4(d, 'const', cr4(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 130, 1, e
  end if

  if (n .ne. 0) then
    ne = ne + 1
    write(*, 2002) 130, n
  end if

  n = fgd_get_constant_r4(d, 'const', fl)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 130, 2, e
  end if

  if (abs(fl - 130) > 0.001) then
    ne = ne + 1
    write(*, 2005) 130, fl
  end if

! 131: fgd_put_constant_r8 check
  cr8(1) = 131
  n = fgd_put_constant_r8(d, 'const', cr8(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 131, 1, e
  end if

  if (n .ne. 0) then
    ne = ne + 1
    write(*, 2002) 131, n
  end if

  n = fgd_get_constant_r4(d, 'const', fl)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 131, 2, e
  end if

  if (abs(fl - 131) > 0.001) then
    ne = ne + 1
    write(*, 2005) 131, fl
  end if

! 132: fgd_put_constant_c8 check
  cc8(1) = 132
  n = fgd_put_constant_c8(d, 'const', cc8(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 132, 1, e
  end if

  if (n .ne. 0) then
    ne = ne + 1
    write(*, 2002) 132, n
  end if

  n = fgd_get_constant_r4(d, 'const', fl)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 132, 2, e
  end if

  if (abs(fl - 132) > 0.001) then
    ne = ne + 1
    write(*, 2005) 132, fl
  end if

! 133: fgd_put_constant_c16 check
  cc16(1) = 133
  n = fgd_put_constant_c16(d, 'const', cc16(1))
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 133, 1, e
  end if

  if (n .ne. 0) then
    ne = ne + 1
    write(*, 2002) 133, n
  end if

  n = fgd_get_constant_r4(d, 'const', fl)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 133, 2, e
  end if

  if (abs(fl - 133) > 0.001) then
    ne = ne + 1
    write(*, 2005) 133, fl
  end if

! 62: fgd_put_string check
  n = fgd_put_string(d, 'string', "Arthur Dent")
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 62, 1, e
  end if

  if (n .ne. 11) then
    ne = ne + 1
    write(*, 2002) 62, n
  end if

  n = fgd_get_string(d, 'string', GD_FIELD_LEN, str)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 62, 2, e
  end if

  if (str .ne. "Arthur Dent") then
    ne = ne + 1
    write(*, 2009) 62, str
  end if

! 63: fgd_get_nmfields_by_type check
  n = fgd_get_nmfields_by_type(d, "data", GD_LINCOM_ENTRY)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 63, e
  end if

  if (n .ne. 2) then
    ne = ne + 1
    write(*, 2002), 63, n
  end if

! 64: fgd_get_mfield_list_by_type check
  fields(1) = 'mnew1'
  fields(2) = 'mnew2'
  l = flen
  call fgd_get_mfield_list_by_type(flist, d, "data", GD_LINCOM_ENTRY, l)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 64, i, e
  end if

  if (l .ne. flen) then
    ne = ne + 1
    write(*, 2007) 64, i, l
  end if

  do i = 1, n
  if (flist(i) .ne. fields(i)) then
    ne = ne + 1
    write(*, 2008) i, 64, flist(i)
  end if
  end do

! 65: fgd_get_nmvectors check
  n = fgd_get_nmvectors(d, "data")
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 65, e
  end if

  if (n .ne. 11) then
    ne = ne + 1
    write(*, 2002), 65, n
  end if

! 66: fgd_get_mvector_list check
  fields = (/ 'mlut  ', 'mnew1 ', 'mnew2 ', 'mnew3 ', 'mnew5 ', 'mnew6 ', &
  'mnew7 ', 'mnew8 ', 'mnew9 ', 'mnew10', 'mnew4 ', '      ', '      ', &
  '      ', '      ', '      ', '      ', '      ', '      ', '      ', &
  '      ' /)
  l = flen
  call fgd_get_mvector_list(flist, d, "data", l)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 66, i, e
  end if

  if (l .ne. flen) then
    ne = ne + 1
    write(*, 2007) 66, i, l
  end if

  do i=1,n
  if (flist(i) .ne. fields(i)) then
    ne = ne + 1
    write(*, 2008) i, 66, flist(i)
  end if
  end do

! 67: fgd_alter_raw check
  call fgd_alter_raw(d, 'new1', GD_INT32, 4, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 67, 1, e
  end if

  n = fgd_get_entry(d, 'new1', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 67, 2, e
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 67, 3, ent%fragment_index
  end if

  if (ent%spf .ne. 4) then
    ne = ne + 1
    write(*, 2007) 67, 4, ent%spf
  end if

  if (ent%data_type .ne. GD_INT32) then
    ne = ne + 1
    write(*, 2007) 67, 5, ent%data_type
  end if

  if (n .ne. GD_RAW_ENTRY) then
    ne = ne + 1
    write(*, 2007), 67, 6, n
  end if

! 68: fgd_alter_lincom check
  call fgd_alter_lincom(d, 'new2', 3, 'in4', 9.9d-1, 7.8d0, 'in5', &
     1.1d1, 2.2d-2, 'in6', 1.96d0, 0d0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 68, 1, e
  end if

  n = fgd_get_entry(d, 'new2', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 68, 2, e
  end if

  if (n .ne. GD_LINCOM_ENTRY) then
    ne = ne + 1
    write(*, 2007) 68, 3, n
  end if

  if (ent%n_fields .ne. 3) then
    ne = ne + 1
    write(*, 2007) 68, 4, ent%n_fields
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 68, 5, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in4') then
    ne = ne + 1
    write(*, 2008) 68, 6, ent%field(1)
  end if

  if (ent%field(2) .ne. 'in5') then
    ne = ne + 1
    write(*, 2008) 68, 7, ent%field(2)
  end if

  if (ent%field(3) .ne. 'in6') then
    ne = ne + 1
    write(*, 2008) 68, 8, ent%field(3)
  end if

  if (ent%comp_scal .ne. 0) then
    ne = ne + 1
    write(*, 2007) 68, 5, ent%comp_scal
  end if

  q = (/ 9.9d-1, 7.8d0, 1.1d1, 2.2d-2, 1.96d0, 0d0 /)
  DO i=1,3
  if (abs(ent%m(i) - q(i * 2 - 1)) > 0.001) then
    ne = ne + 1
    write(*, 2010) i * 2 - 1, 68, ent%m(i)
  end if
  if (abs(ent%b(i) - q(i * 2)) > 0.001) then
    ne = ne + 1
    write(*, 2010) i * 2, 68, ent%b(i)
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
    write(*, 2006) 69, 1, e
  end if

  n = fgd_get_entry(d, 'new3', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 69, e
  end if

  if (n .ne. GD_LINCOM_ENTRY) then
    ne = ne + 1
    write(*, 2007) 69, 1, n
  end if

  if (ent%n_fields .ne. 2) then
    ne = ne + 1
    write(*, 2007) 69, 2, ent%n_fields
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 69, 3, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in4') then
    ne = ne + 1
    write(*, 2008) 69, 4, ent%field(1)
  end if

  if (ent%field(2) .ne. 'in3') then
    ne = ne + 1
    write(*, 2008) 69, 5, ent%field(2)
  end if

  if (ent%comp_scal .ne. 1) then
    ne = ne + 1
    write(*, 2007) 69, 6, ent%comp_scal
  end if

  cq(1) = dcmplx(0.1, 0.2)
  cq(2) = dcmplx(0.3, 0.4)
  cq(3) = dcmplx(0.4, 0.5)
  cq(4) = dcmplx(0.6, 0.7)
  DO i=1,2
  if (abs(ent%cm(i) - cq(i * 2 - 1)) > 0.001) then
    ne = ne + 1
    write(*, 2011) i * 2 - 1, 69, real(real(ent%cm(i))), real(aimag(ent%cm(i)))
  end if
  if (abs(ent%cb(i) - cq(i * 2)) > 0.001) then
    ne = ne + 1
    write(*, 2011) i * 2, 69, real(real(ent%cb(i))), real(aimag(ent%cb(i)))
  end if
  end do

! 70: fgd_alter_polynom check
  call fgd_alter_polynom(d, 'new4', 4, 'in1', 3d0, 4d0, 5d0, 6d0, 7d0, 0d0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 70, 1, e
  end if

  n = fgd_get_entry(d, 'new4', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 70, 2, e
  end if

  if (n .ne. GD_POLYNOM_ENTRY) then
    ne = ne + 1
    write(*, 2007) 70, 1, n
  end if

  if (ent%poly_ord .ne. 4) then
    ne = ne + 1
    write(*, 2007) 70, 2, ent%poly_ord
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 70, 3, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 2008) 70, 4, ent%field(1)
  end if

  q = (/ 3d0, 4d0, 5d0, 6d0, 7d0, 0d0 /)
  DO i=1,5
  if (abs(ent%a(i) - q(i)) > 0.001) then
    ne = ne + 1
    write(*, 2010) i, 70, ent%a(i)
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
    write(*, 2006) 71, 1, e
  end if

  n = fgd_get_entry(d, 'new5', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 71, 2, e
  end if

  if (n .ne. GD_POLYNOM_ENTRY) then
    ne = ne + 1
    write(*, 2007) 71, 1, n
  end if

  if (ent%poly_ord .ne. 3) then
    ne = ne + 1
    write(*, 2007) 71, 2, ent%poly_ord
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 71, 3, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 2008) 71, 4, ent%field(1)
  end if

  if (ent%comp_scal .ne. 1) then
    ne = ne + 1
    write(*, 2007) 71, 5, ent%comp_scal
  end if

  cq(1) = dcmplx(1.1, 5.0)
  cq(2) = dcmplx(1.2, 4.0)
  cq(3) = dcmplx(1.2, 3.0)
  cq(4) = dcmplx(1.3, 2.4)
  DO 710 i=1,4
  if (abs(ent%ca(i) - cq(i)) > 0.001) then
    ne = ne + 1
    write(*, 2011) i, 71, real(real(ent%ca(i))), real(aimag(ent%ca(i)))
  end if
  710 CONTINUE

! 72: fgd_alter_linterp check
  call fgd_alter_linterp(d, "new6", "in3", "./other/table", 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 72, 1, e
  end if

  n = fgd_get_entry(d, 'new6', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 72, 2, e
  end if

  if (n .ne. GD_LINTERP_ENTRY) then
    ne = ne + 1
    write(*, 2007) 72, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 72, 2, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in3') then
    ne = ne + 1
    write(*, 2008) 72, 3, ent%field(1)
  end if

  if (ent%field(2) .ne. './other/table') then
    ne = ne + 1
    write(*, 2008) 72, 4, ent%field(2)
  end if

! 73: fgd_alter_bit check
  call fgd_alter_bit(d, "new7", "in3", 3, 2)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 73, 1, e
  end if

  n = fgd_get_entry(d, 'new7', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 73, 2, e
  end if

  if (n .ne. GD_BIT_ENTRY) then
    ne = ne + 1
    write(*, 2007) 73, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 73, 2, ent%fragment_index
  end if

  if (ent%numbits .ne. 2) then
    ne = ne + 1
    write(*, 2007) 73, 3, ent%numbits
  end if

  if (ent%bitnum .ne. 3) then
    ne = ne + 1
    write(*, 2007) 73, 4, ent%bitnum
  end if

  if (ent%field(1) .ne. 'in3') then
    ne = ne + 1
    write(*, 2008) 73, 5, ent%field(1)
  end if

! 74: fgd_alter_sbit check
  call fgd_alter_sbit(d, "new8", "out", 1, 22)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 74, 1, e
  end if

  n = fgd_get_entry(d, 'new8', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 74, 2, e
  end if

  if (n .ne. GD_SBIT_ENTRY) then
    ne = ne + 1
    write(*, 2007) 74, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 74, 2, ent%fragment_index
  end if

  if (ent%numbits .ne. 22) then
    ne = ne + 1
    write(*, 2007) 74, 3, ent%numbits
  end if

  if (ent%bitnum .ne. 1) then
    ne = ne + 1
    write(*, 2007) 74, 4, ent%bitnum
  end if

  if (ent%field(1) .ne. 'out') then
    ne = ne + 1
    write(*, 2008) 74, 5, ent%field(1)
  end if

! 75: fgd_alter_multiply check
  call fgd_alter_multiply(d, 'new9', 'in6', 'in4')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 75, 1, e
  end if

  n = fgd_get_entry(d, 'new9', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 75, 2, e
  end if

  if (n .ne. GD_MULTIPLY_ENTRY) then
    ne = ne + 1
    write(*, 2007) 75, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 75, 2, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in6') then
    ne = ne + 1
    write(*, 2008) 75, 3, ent%field(1)
  end if

  if (ent%field(2) .ne. 'in4') then
    ne = ne + 1
    write(*, 2008) 75, 4, ent%field(2)
  end if

! 76: fgd_alter_phase check
  call fgd_alter_phase(d, 'new10', 'in2', 8)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 76, 1, e
  end if

  n = fgd_get_entry(d, 'new10', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 76, 2, e
  end if

  if (n .ne. GD_PHASE_ENTRY) then
    ne = ne + 1
    write(*, 2007) 76, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 76, 2, ent%fragment_index
  end if

  if (ent%shift .ne. 8) then
    ne = ne + 1
    write(*, 2007) 76, 3, ent%shift
  end if

  if (ent%field(1) .ne. 'in2') then
    ne = ne + 1
    write(*, 2008) 76, 4, ent%field(1)
  end if

! 77: fgd_alter_const check
  call fgd_alter_const(d, 'new11', GD_FLOAT32)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 77, 1, e
  end if

  n = fgd_get_entry(d, 'new11', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 77, 2, e
  end if

  if (n .ne. GD_CONST_ENTRY) then
    ne = ne + 1
    write(*, 2007) 77, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 77, 2, ent%fragment_index
  end if

  if (ent%data_type .ne. GD_FLOAT32) then
    ne = ne + 1
    write(*, 2007) 77, 3, ent%data_type
  end if

  n = fgd_get_constant_r4(d, 'new11', fl)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 77, 3, e
  end if

  if (abs(fl) > 0.001) then
    ne = ne + 1
    write(*, 2005) 77, fl
  end if

! 78: fgd_get_encoding check
  n = fgd_get_encoding(d, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 78, e
  end if

  if (n .ne. GD_UNENCODED) then
    ne = ne + 1
    write(*, 2002) 78, n
  end if

! 79: fgd_get_endianness check
  n = fgd_get_endianness(d, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 79, e
  end if

  if (n .ne. GD_LITTLE_ENDIAN) then
    ne = ne + 1
    write(*, 2002) 79, n
  end if

! 80: fgd_dirfilename check
  l = GD_FIELD_LEN
  call fgd_dirfilename(str, l, d, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 80, e
  end if

  if (l .ne. GD_FIELD_LEN) then
    ne = ne + 1
    write(*, 2002) 80, l
  end if

  if (str .ne. 'test95_dirfile') then
    ne = ne + 1
    write(*, 2009) 80, str
  end if

! 81: fgd_get_parent_fragment check
  n = fgd_get_parent_fragment(d, 1)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 81, e
  end if

  if (n .ne. 0) then
    ne = ne + 1
    write(*, 2002) 81, n
  end if

! 82: fgd_protect check
  call fgd_protect(d, GD_PROTECT_DATA, 1)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 82, e
  end if

! 83: fgd_get_protection check
  n = fgd_get_protection(d, 1)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 83, e
  end if

  if (n .ne. GD_PROTECT_DATA) then
    ne = ne + 1
    write(*, 2002) 83, n
  end if

! 84: fgd_get_raw_filename check
  str = fgd_get_raw_filename(d, "data")
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 84, e
  end if

  if (str .ne. 'test95_dirfile/data') then
    ne = ne + 1
    write(*, 2009) 84, str
  end if

! 85: fgd_reference check
  str = fgd_reference(d, "new1")
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 85, e
  end if

  if (str .ne. 'new1') then
    ne = ne + 1
    write(*, 2009) 85, str
  end if

! 87: fgd_alter_encoding check
  call fgd_alter_encoding(d, GD_SLIM_ENCODED, 1, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 87, 1, e
  end if

  n = fgd_get_encoding(d, 1)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 87, 2, e
  end if

  if (n .ne. GD_SLIM_ENCODED) then
    ne = ne + 1
    write(*, 2002) 87, n
  end if

! 88: fgd_alter_endianness check
  call fgd_alter_endianness(d, GD_BIG_ENDIAN, 1, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 88, 1, e
  end if

  n = fgd_get_endianness(d, 1)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 88, 2, e
  end if

  if (n .ne. GD_BIG_ENDIAN) then
    ne = ne + 1
    write(*, 2002) 88, n
  end if

! 89: fgd_alter_spec check
  call fgd_alter_spec(d, 'new10 PHASE in1 3', 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 89, 1, e
  end if

  n = fgd_get_entry(d, 'new10', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 89, 2, e
  end if

  if (n .ne. GD_PHASE_ENTRY) then
    ne = ne + 1
    write(*, 2007) 89, 1, l
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 89, 2, ent%fragment_index
  end if

  if (ent%shift .ne. 3) then
    ne = ne + 1
    write(*, 2007) 89, 3, ent%shift
  end if

  if (ent%field(1) .ne. 'in1') then
    ne = ne + 1
    write(*, 2008) 89, 4, ent%field(1)
  end if

! 90: fgd_delete check
  call fgd_delete(d, 'new10', 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 90, 1, e
  end if

  n = fgd_get_entry(d, 'new10', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_BAD_CODE) then
    ne = ne + 1
    write(*, 2006) 90, 2, e
  end if

! 91: fgd_malter_spec check
  call fgd_malter_spec(d, 'mnew10 PHASE in4 11', 'data', 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 91, 1, e
  end if

  n = fgd_get_entry(d, 'data/mnew10', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 91, 2, e
  end if

  if (n .ne. GD_PHASE_ENTRY) then
    ne = ne + 1
    write(*, 2007) 91, 1, n
  end if

  if (ent%fragment_index .ne. 0) then
    ne = ne + 1
    write(*, 2007) 91, 2, ent%fragment_index
  end if

  if (ent%shift .ne. 11) then
    ne = ne + 1
    write(*, 2007) 91, 3, ent%shift
  end if

  if (ent%field(1) .ne. 'in4') then
    ne = ne + 1
    write(*, 2008) 91, 4, ent%field(1)
  end if

! 92: fgd_move check
  call fgd_move(d, 'new9', 1, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 92, 1, e
  end if

  n = fgd_get_entry(d, 'new9', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 92, 2, e
  end if

  if (n .ne. GD_MULTIPLY_ENTRY) then
    ne = ne + 1
    write(*, 2007) 92, 1, n
  end if

  if (ent%fragment_index .ne. 1) then
    ne = ne + 1
    write(*, 2007) 92, 2, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in6') then
    ne = ne + 1
    write(*, 2008) 92, 3, ent%field(1)
  end if

  if (ent%field(2) .ne. 'in4') then
    ne = ne + 1
    write(*, 2008) 92, 4, ent%field(2)
  end if

! 93: fgd_rename check
  call fgd_rename(d, 'new9', 'newer', 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 93, 1, e
  end if

  n = fgd_get_entry(d, 'new9', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_BAD_CODE) then
    ne = ne + 1
    write(*, 2006) 93, 2, e
  end if

  n = fgd_get_entry(d, 'newer', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 93, 3, e
  end if

  if (n .ne. GD_MULTIPLY_ENTRY) then
    ne = ne + 1
    write(*, 2007) 93, 1, l
  end if

  if (ent%fragment_index .ne. 1) then
    ne = ne + 1
    write(*, 2007) 92, 2, ent%fragment_index
  end if

  if (ent%field(1) .ne. 'in6') then
    ne = ne + 1
    write(*, 2008) 92, 3, ent%field(1)
  end if

  if (ent%field(2) .ne. 'in4') then
    ne = ne + 1
    write(*, 2008) 92, 4, ent%field(2)
  end if

! 94: fgd_uninclude check
  call fgd_uninclude(d, 1, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 94, 1, e
  end if

  n = fgd_get_entry(d, 'newer', ent)
  e = fgd_error(d)

  if (e .ne. GD_E_BAD_CODE) then
    ne = ne + 1
    write(*, 2006) 94, 2, e
  end if

! 95: fgd_get_frameoffset check
  n = fgd_get_frameoffset(d, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 95, e
  end if

  if (n .ne. 0) then
    ne = ne + 1
    write(*, 2002) 95, n
  end if

! 96: fgd_alter_frameoffset check
  call fgd_alter_frameoffset(d, 33, 0, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 96, 1, e
  end if

  n = fgd_get_frameoffset(d, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2006) 96, 2, e
  end if

  if (n .ne. 33) then
    ne = ne + 1
    write(*, 2002) 96, n
  end if

! 97: fgd_get_native_type check
  n = fgd_get_native_type(d, 'data')
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 97, e
  end if

  if (n .ne. GD_INT8) then
    ne = ne + 1
    write(*, 2002) 97, n
  end if

! 99: fgd_validate check
  n = fgd_validate(d, 'new7')
  e = fgd_error(d)

  if (e .ne. GD_E_BAD_CODE) then
    ne = ne + 1
    write(*, 2001) 99, e
  end if

  if (n .ne. -1) then
    ne = ne + 1
    write(*, 2002) 99, n
  end if

! 100: fgd_get_framenum check
  str = fgd_reference(d, "data")
  dp = fgd_get_framenum(d, 'INDEX', 33.3d0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 100, e
  end if

  if (abs(dp - 33.3) > 0.001) then
    ne = ne + 1
    write(*, 2012) 100, dp
  end if

! 101: fgd_get_framenum_subset check
  dp = fgd_get_framenum_subset(d, 'data', 33.3d0, 6, 0)
  e = fgd_error(d)

  if (e .ne. GD_E_OK) then
    ne = ne + 1
    write(*, 2001) 101, e
  end if

  if (abs(dp - 37.0375) > 0.001) then
    ne = ne + 1
    write(*, 2012) 101, dp
  end if

  call fgd_close(d)

  call system ( 'rm -rf ' // fildir )

  if (ne .GT. 0) then
    write(*, 2003) ne
    call exit(1)
  end if

2001 format('e[', i0, '] = ', i0)
2002 format('n[', i0, '] = ', i0)
2003 format('ne = ', i0)
2004 format('c(', i0, ')[', i0, '] = ', i0)
2005 format('fl[', i0, '] = ', f0.16)
2006 format('e[', i0, ', ', i0, '] = ', i0)
2007 format('n[', i0, ', ', i0, '] = ', i0)
2008 format('fn(', i0, ')[', i0, '] = "', a, '"')
2009 format('s[' i0, '] = "', a, '"')
2010 format('p(', i0, ')[', i0, '] = ', d16.10)
2011 format('p(', i0, ')[', i0, '] = ', d16.10, ';', d16.10)
2012 format('d[', i0, '] = ', d16.10)
2013 format('c[', i0, '] = ', d16.10, ';', d16.10)
end program
