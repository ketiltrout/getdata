project(GetDataTests)

GD_FILES(gd test)

add_definitions(-DGD_C89_API)

set(tests_ignored
  ascii_complex128 ascii_complex64 ascii_float32 ascii_float64

  bzip_add bzip_complex128 bzip_complex64 bzip_del bzip_enoent bzip_float32
  bzip_float64 bzip_get bzip_get_cont bzip_get_far bzip_get_get bzip_get_get2
  bzip_get_put bzip_int16 bzip_int32 bzip_int64 bzip_int8 bzip_move_from
  bzip_move_to bzip_nframes bzip_put bzip_put_back bzip_put_endian bzip_put_get
  bzip_put_offs bzip_put_pad bzip_put_sub bzip_seek bzip_seek_far bzip_sync
  bzip_uint16 bzip_uint32 bzip_uint64 bzip_uint8

  desync_path

  enc_add enc_complex128 enc_complex64 enc_del enc_enoent enc_float32
  enc_float64 enc_get_cont enc_int16 enc_int32 enc_int64 enc_int8
  enc_move_from enc_nframes enc_put_offs enc_seek enc_uint16 enc_uint32
  enc_uint64 enc_uint8

  error_verbose error_verbose_prefix

  flac_add flac_complex128 flac_complex64 flac_del flac_enoent flac_float32
  flac_float64 flac_get_big flac_get_cont flac_get_far flac_get_get
  flac_get_get2 flac_get_int64 flac_get_int8 flac_get_little flac_get_long
  flac_int16 flac_int32 flac_int64 flac_int8 flac_move_from flac_nframes
  flac_put_big flac_put_complex128 flac_put_float64 flac_put_int32
  flac_put_little flac_put_offs flac_seek flac_seek_far flac_sync flac_uint16
  flac_uint32 flac_uint64 flac_uint8

  get_off64

  gzip_add gzip_complex128 gzip_complex64 gzip_del gzip_enoent gzip_float32
  gzip_float64 gzip_get gzip_get_cont gzip_get_far gzip_get_get gzip_get_get2
  gzip_get_put gzip_int16 gzip_int32 gzip_int64 gzip_int8 gzip_move_from
  gzip_move_to gzip_nframes gzip_put gzip_put_back gzip_put_endian gzip_put_get
  gzip_put_nframes gzip_put_off gzip_put_offs gzip_put_pad gzip_put_sub
  gzip_seek gzip_seek_far gzip_seek_put gzip_sync gzip_uint16 gzip_uint32
  gzip_uint64 gzip_uint8

  header_off64t

  lzma_enoent lzma_get lzma_nframes lzma_put lzma_xz_add lzma_xz_complex128
  lzma_xz_complex64 lzma_xz_del lzma_xz_float32 lzma_xz_float64 lzma_xz_get
  lzma_xz_get_cont lzma_xz_get_far lzma_xz_get_get lzma_xz_get_get2
  lzma_xz_get_put lzma_xz_int16 lzma_xz_int32 lzma_xz_int64 lzma_xz_int8
  lzma_xz_move_from lzma_xz_move_to lzma_xz_nframes lzma_xz_offs_clear
  lzma_xz_put lzma_xz_put_back lzma_xz_put_endian lzma_xz_put_get
  lzma_xz_put_offs lzma_xz_put_pad lzma_xz_seek lzma_xz_seek_far lzma_xz_sync
  lzma_xz_uint16 lzma_xz_uint32 lzma_xz_uint64 lzma_xz_uint8

  legacy_error legacy_estring legacy_format legacy_get legacy_get_put
  legacy_get_rofs legacy_nframes legacy_nonexistent legacy_put legacy_spf

  match_fragment match_pcre match_pcre_bad match_pcre_caseless match_pcre_ext
  match_pcre_js match_pcre_utf8 match_regex match_regex_bad match_regex_ext
  match_regex_icase

  open_eaccess open_sym_a open_sym_al open_sym_as open_sym_at open_sym_c
  open_sym_cl open_sym_ct open_sym_d open_sym_l open_sym_p open_sym_pl
  open_sym_pt

  parse_huge

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
