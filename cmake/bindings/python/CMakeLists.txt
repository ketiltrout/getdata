# Generate pyconstants.c using make_parameters
add_custom_command(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/pyconstants.c
  COMMAND make_parameters p > ${CMAKE_CURRENT_BINARY_DIR}/pyconstants.c
  DEPENDS make_parameters getdata
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  COMMENT "Generating pyconstants.c"
)

# Build Python extension module
Python3_add_library(pygetdata MODULE
  ${GD_DIR}/bindings/python/pydirfile.c
  ${GD_DIR}/bindings/python/pyentry.c
  ${GD_DIR}/bindings/python/pyfragment.c
  ${GD_DIR}/bindings/python/pygetdata.c
  ${CMAKE_CURRENT_BINARY_DIR}/pyconstants.c
)

target_include_directories(pygetdata PRIVATE
  ${GD_DIR}/src
  ${GD_DIR}/bindings/python
  ${CMAKE_BINARY_DIR}
  ${Python3_NumPy_INCLUDE_DIRS}
)

# Link against shared library - wheel repair tools will bundle it
target_link_libraries(pygetdata PRIVATE getdata)

# Remove 'lib' prefix on Unix-like systems
set_target_properties(pygetdata PROPERTIES PREFIX "")

# For wheel builds, set RPATH so repair tools can find the library
if(SKBUILD)
  if(APPLE)
    set_target_properties(pygetdata PROPERTIES
      INSTALL_RPATH "@loader_path/lib"
      BUILD_WITH_INSTALL_RPATH FALSE)
  else()
    set_target_properties(pygetdata PROPERTIES
      INSTALL_RPATH "$ORIGIN/lib"
      BUILD_WITH_INSTALL_RPATH FALSE)
  endif()
endif()

install(TARGETS pygetdata LIBRARY DESTINATION .)
