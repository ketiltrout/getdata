
GD_FILES(s bindings/cxx)
GD_FILES(h bindings/cxx/getdata)

include_directories(${GD_DIR}/bindings/cxx)

set(GD_CXX getdata++)
if(MSVC AND NOT MSVC_IDE)
	# nmake could not handle ++ in the path
	set(GD_CXX getdataxx)
endif()

add_library(${GD_CXX} STATIC ${h_headers} ${s_sources})


install(FILES ${h_headers} DESTINATION include/getdata)
install(TARGETS ${GD_CXX} DESTINATION lib)
