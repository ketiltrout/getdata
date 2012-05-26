project(GetDataTests)

GD_FILES(gd test)

add_definitions(-DGD_C89_API)

set(tests_ignored
  bzip_get bzip_get_get bzip_move_from bzip_nframes bzip_put
  gzip_add gzip_get gzip_get_get gzip_get_put gzip_move_from gzip_move_to gzip_nframes gzip_put gzip_put_get
  lzma_get lzma_nframes
  legacy_get legacy_get_put legacy_get_rofs legacy_nframes legacy_nonexistent legacy_put legacy_spf
  open_eaccess
  slim_get slim_nframes slim_put
  trunc_rofs
  xz_get xz_nframes
  zzip_data zzip_get zzip_get_get zzip_nframes
  zzslim_get zzslim_nframes
  )

if(MSVC)
  set(tests_ignored 
    ${tests_ignored}
    error_verbose error_verbose_prefix
    get_off64 put_off64
    )
endif()

if(MINGW)
  set(tests_ignored
    ${tests_ignored}
    error_verbose error_verbose_prefix
    get_off64 put_off64
    )
endif()

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
