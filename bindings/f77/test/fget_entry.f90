! attempt to read INT8 via the F95 bindings

program get_int8
  use getdata
  character (len=*), parameter :: filedir='test95_dirfile'
  character (len=*), parameter :: frmat='test95_dirfile/format'
  character (len=*), parameter :: formatdata='data RAW INT8 8'
  integer :: i, d, n, e
  type(gd_entry) :: ent

  call system ( 'rm -rf ' // filedir )
  call system ( 'mkdir ' // filedir )

  open(1, file=frmat, status='new')
  write(1, *) formatdata
  close(1, status='keep')

  d = fdirfile_open(filedir, GD_RDONLY)
  n = fget_entry(d, 'data', ent)
  e = fget_error(d)
  call fdirfile_close(d)

  call system ( 'rm -rf ' // filedir )

  if (e .ne. GD_E_OK) call exit(1)
  if (n .ne. GD_RAW_ENTRY) call exit(1)
  if (ent%field1 .ne. './data') call exit(1)
  if (ent%spf .ne. 8) call exit(1)
  if (ent%data_type .ne. GD_INT8) call exit(1)
end program
