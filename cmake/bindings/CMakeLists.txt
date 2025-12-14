
add_subdirectory(cxx)

if(BUILD_PYTHON)
  # Build make_parameters utility for generating pyconstants.c
  add_executable(make_parameters ${GD_DIR}/bindings/make_parameters.c)
  target_include_directories(make_parameters PRIVATE ${GD_DIR}/src ${CMAKE_BINARY_DIR})
  target_link_libraries(make_parameters PRIVATE getdata)
  target_compile_definitions(make_parameters PRIVATE HAVE_CONFIG_H)

  add_subdirectory(python)
endif()


