include_directories(${GD_DIR}/util)

add_executable(checkdirfile ${GD_DIR}/util/checkdirfile.c)
target_link_libraries(checkdirfile getdata)
# For wheel builds, set RPATH so repair tools can find the library
if(SKBUILD)
  if(APPLE)
    set_target_properties(checkdirfile PROPERTIES
      INSTALL_RPATH "@loader_path/../lib"
      BUILD_WITH_INSTALL_RPATH FALSE)
  else()
    set_target_properties(checkdirfile PROPERTIES
      INSTALL_RPATH "$ORIGIN/../lib"
      BUILD_WITH_INSTALL_RPATH FALSE)
  endif()
endif()
install(TARGETS checkdirfile DESTINATION bin)

if(HAVE_GETOPT_LONG)
  add_executable(dirfile2ascii ${GD_DIR}/util/dirfile2ascii.c)
  target_link_libraries(dirfile2ascii getdata)
  if(SKBUILD)
    if(APPLE)
      set_target_properties(dirfile2ascii PROPERTIES
        INSTALL_RPATH "@loader_path/../lib"
        BUILD_WITH_INSTALL_RPATH FALSE)
    else()
      set_target_properties(dirfile2ascii PROPERTIES
        INSTALL_RPATH "$ORIGIN/../lib"
        BUILD_WITH_INSTALL_RPATH FALSE)
    endif()
  endif()
  install(TARGETS dirfile2ascii DESTINATION bin)
endif()
