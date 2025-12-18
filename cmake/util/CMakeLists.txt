include_directories(${GD_DIR}/util)

add_executable(checkdirfile ${GD_DIR}/util/checkdirfile.c)
target_link_libraries(checkdirfile getdata)
install(TARGETS checkdirfile DESTINATION bin)

if(HAVE_GETOPT_LONG)
  add_executable(dirfile2ascii ${GD_DIR}/util/dirfile2ascii.c)
  target_link_libraries(dirfile2ascii getdata)
  install(TARGETS dirfile2ascii DESTINATION bin)
endif()
