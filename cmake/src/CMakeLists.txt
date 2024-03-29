
set(files_ignored
	${GD_DIR}/src/bzip.c
	${GD_DIR}/src/flac.c
	${GD_DIR}/src/gzip.c
	${GD_DIR}/src/lzma.c
	${GD_DIR}/src/legacy.c
	${GD_DIR}/src/slim.c
	${GD_DIR}/src/zzslim.c
	${GD_DIR}/src/zzip.c
	)

GD_FILES(gd src)

if(MSVC)
	set(gd_sources 
		${gd_sources}
		${GD_DIR}/src/msvc/dirent.h
		${GD_DIR}/src/msvc/dirent.c
		${GD_DIR}/src/msvc/dirname.c
		${GD_DIR}/src/msvc/libgen.h
		${GD_DIR}/src/msvc/inttypes.h
		)
	add_definitions(-DGD_C89_API)
endif()

if(GD_DEBUG)
  add_definitions(-DGETDATA_DEBUG)
else()
  set(files_ignored
    ${files_ignored}
    ${GD_DIR}/src/debug.c
    )
endif()

list(REMOVE_ITEM gd_sources ${files_ignored})

add_library(getdata STATIC ${gd_sources} ${gd_headers} ${CMAKE_BINARY_DIR}/getdata.h)


install(FILES ${CMAKE_BINARY_DIR}/getdata.h DESTINATION include)
install(TARGETS getdata DESTINATION lib)



