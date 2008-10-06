! attempt to read INT8 via the F95 bindings

program get_int8
  use getdata
  character (len=*), parameter :: filedir='test95_dirfile'
  character (len=*), parameter :: frmat='test95_dirfile/format'
  character (len=*), parameter :: formatdata='data RAW INT8 8'
  integer :: d, n, l, e, c=10
  character(len=10), dimension(1) :: fl

  call system ( 'rm -rf ' // filedir )
  call system ( 'mkdir ' // filedir )

  open(1, file=frmat, status='new')
  write(1, *) formatdata
  close(1, status='keep')

  d = fdirfile_open(filedir, GD_RDONLY)
  n = fget_nfields(d)
  l = fget_field_name_max(d)
  e = fget_error(d)
  call fget_field_list(fl, d, c)
  call fdirfile_close(d)

  call system ( 'rm -rf ' // filedir )

  if (e .ne. GD_E_OK) call exit(1)
  if (l .ne. 4) call exit(1)
  if (n .ne. 1) call exit(1)
  if (fl(1) /= 'data') call exit(1)
end program
