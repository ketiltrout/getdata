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

# Link against static library for self-contained wheels
target_link_libraries(pygetdata PRIVATE getdata_static)

target_compile_definitions(pygetdata PRIVATE HAVE_CONFIG_H)

# Remove 'lib' prefix on Unix-like systems
set_target_properties(pygetdata PROPERTIES PREFIX "")

install(TARGETS pygetdata LIBRARY DESTINATION .)
