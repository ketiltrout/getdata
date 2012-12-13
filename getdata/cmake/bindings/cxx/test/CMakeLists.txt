project(GetDataTests)

add_executable(cxx_test_big_test ${GD_DIR}/bindings/cxx/test/big_test.cpp)
target_link_libraries(cxx_test_big_test ${GD_CXX} getdata)
add_test(NAME cxx_test_big_test COMMAND cxx_test_big_test)
