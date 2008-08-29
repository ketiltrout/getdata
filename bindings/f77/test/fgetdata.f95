! attempt to read INT8 via the F95 bindings

program get_int8
  use getdata
  character (len=*), parameter :: filedir='test95_dirfile'
  character (len=*), parameter :: frmat='test95_dirfile/format'
  character (len=*), parameter :: dat='test95_dirfile/data'
  character (len=*), parameter :: formatdata='data RAW INT8 8'
  integer*1, dimension(8) :: c
  integer*1, dimension(80) :: datadata
  integer :: i, d, n, e

  do i=1,8
    c(i) = 0;
  end do

  call system ( 'rm -rf ' // filedir )
  call system ( 'mkdir ' // filedir )

  do i=1,80
    datadata(i) = i
  end do

  open(1, file=frmat, status='new')
  write(1, *) formatdata
  close(1, status='keep')

  open(1, file=dat, form='unformatted', access='direct', recl=80, &
  status='new')
  write(1, rec=1) datadata
  close(1, status='keep')

  d = fdirfile_open(filedir, GD_RDONLY)
  n = fgetdata_i1(d, 'data', 5, 0, 1, 0, c)
  e = fget_error(d)
  call fdirfile_close(d)

  call system ( 'rm -rf ' // filedir )

  if (e .ne. GD_E_OK) call exit(1)
  if (n .ne. 8) call exit(1)
  
  do i=1,8
    if (c(i) .ne. 40 + i) call exit(1)
  end do
end program
