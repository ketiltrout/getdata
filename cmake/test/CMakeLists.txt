project(GetDataTests)

GD_FILES(gd test)

add_definitions(-DGD_C89_API)

set(tests_ignored
  bzip_add bzip_get bzip_get_far bzip_get_get bzip_get_get2 bzip_get_put
  bzip_move_from bzip_move_to bzip_nframes bzip_put bzip_put_back
  bzip_put_endian bzip_put_get bzip_put_pad bzip_put_sub bzip_seek
  bzip_seek_far bzip_sync

  desync_path

  error_verbose error_verbose_prefix

  flac_add flac_get_big flac_get_far flac_get_get flac_get_get2 flac_get_little
  flac_nframes flac_put_big flac_put_complex128 flac_put_float64 flac_put_int32
  flac_put_little flac_seek flac_seek_far flac_sync

  get_off64

  gzip_add gzip_del gzip_get gzip_get_far gzip_get_get gzip_get_get2
  gzip_get_put gzip_move_from gzip_move_to gzip_nframes gzip_put gzip_put_back
  gzip_put_endian gzip_put_get gzip_put_nframes gzip_put_off gzip_put_pad
  gzip_put_sub gzip_seek gzip_seek_far gzip_seek_put gzip_sync

  header_off64t

  lzma_get lzma_nframes lzma_put lzma_xz_add lzma_xz_get lzma_xz_get_far
  lzma_xz_get_get lzma_xz_get_get2 lzma_xz_get_put lzma_xz_move_to
  lzma_xz_nframes lzma_xz_put lzma_xz_put_back lzma_xz_put_endian
  lzma_xz_put_get lzma_xz_put_pad lzma_xz_seek lzma_xz_seek_far lzma_xz_sync

  legacy_error legacy_estring legacy_format legacy_get legacy_get_put
  legacy_get_rofs legacy_nframes legacy_nonexistent legacy_put legacy_spf

  open_eaccess open_sym_a open_sym_al open_sym_as open_sym_at open_sym_c
  open_sym_cl open_sym_ct open_sym_d open_sym_l open_sym_p open_sym_pl
  open_sym_pt

  put_off64 put_nofile

  slim_get slim_nframes slim_seek slim_seek_far

  trunc_rofs

  xz_get xz_nframes

  zzip_data zzip_get zzip_get_get zzip_nframes zzip_seek zzip_seek_far

  zzslim_get zzslim_nframes zzslim_seek zzslim_seek_far
  )

foreach(_item ${tests_ignored})
  list(REMOVE_ITEM gd_sources ${GD_DIR}/test/${_item}.c)
endforeach()

#set(gd_sources ${GD_DIR}/test/sie_get_little.c)

foreach(_test ${gd_sources})
  get_filename_component(testname ${_test} NAME_WE)
  set(testname test_${testname})
  add_executable(${testname} ${_test})
  target_link_libraries(${testname} getdata)
  add_test(NAME ${testname} COMMAND ${testname})
endforeach()
