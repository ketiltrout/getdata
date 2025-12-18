
set(files_ignored
	${GD_DIR}/src/legacy.c
	${GD_DIR}/src/slim.c
	${GD_DIR}/src/zzslim.c
	)

# Conditionally include encoding modules based on found libraries
if(NOT BZIP2_FOUND)
  list(APPEND files_ignored ${GD_DIR}/src/bzip.c)
endif()
if(NOT FLAC_FOUND)
  list(APPEND files_ignored ${GD_DIR}/src/flac.c)
endif()
if(NOT ZLIB_FOUND)
  list(APPEND files_ignored ${GD_DIR}/src/gzip.c)
endif()
if(NOT LIBLZMA_FOUND)
  list(APPEND files_ignored ${GD_DIR}/src/lzma.c)
endif()
if(NOT ZZIP_FOUND)
  list(APPEND files_ignored ${GD_DIR}/src/zzip.c)
endif()

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

# Build shared library (parallel to autotools .so)
add_library(getdata SHARED ${gd_sources} ${gd_headers} ${CMAKE_BINARY_DIR}/getdata.h)
set_target_properties(getdata PROPERTIES
  VERSION 11.0.0
  SOVERSION 11
  WINDOWS_EXPORT_ALL_SYMBOLS ON
)

# Build static library (parallel to autotools .a)
add_library(getdata_static STATIC ${gd_sources} ${gd_headers} ${CMAKE_BINARY_DIR}/getdata.h)
set_target_properties(getdata_static PROPERTIES
  OUTPUT_NAME getdata
  POSITION_INDEPENDENT_CODE ON
)

# Macro to configure both shared and static libraries with the same dependencies
macro(configure_getdata_target target)
  if(BZIP2_FOUND)
    target_include_directories(${target} PRIVATE ${BZIP2_INCLUDE_DIRS})
    target_link_libraries(${target} PRIVATE BZip2::BZip2)
    target_compile_definitions(${target} PRIVATE HAVE_LIBBZ2 HAVE_BZLIB_H USE_BZIP2)
  endif()
  if(ZLIB_FOUND)
    target_include_directories(${target} PRIVATE ${ZLIB_INCLUDE_DIRS})
    target_link_libraries(${target} PRIVATE ZLIB::ZLIB)
    target_compile_definitions(${target} PRIVATE HAVE_LIBZ HAVE_ZLIB_H USE_GZIP)
  endif()
  if(LIBLZMA_FOUND)
    target_include_directories(${target} PRIVATE ${LIBLZMA_INCLUDE_DIRS})
    target_link_libraries(${target} PRIVATE LibLZMA::LibLZMA)
    target_compile_definitions(${target} PRIVATE HAVE_LIBLZMA HAVE_LZMA_H USE_LZMA)
  endif()
  if(FLAC_FOUND)
    target_include_directories(${target} PRIVATE ${FLAC_INCLUDE_DIRS})
    target_link_libraries(${target} PRIVATE ${FLAC_LIBRARIES})
    target_compile_definitions(${target} PRIVATE HAVE_LIBFLAC HAVE_FLAC_ALL_H USE_FLAC)
  endif()
  if(ZZIP_FOUND)
    target_include_directories(${target} PRIVATE ${ZZIP_INCLUDE_DIRS})
    target_link_libraries(${target} PRIVATE ${ZZIP_LIBRARIES})
    target_compile_definitions(${target} PRIVATE HAVE_LIBZZIP HAVE_ZZIP_LIB_H USE_ZZIP)
  endif()
  if(PCRE_FOUND)
    target_include_directories(${target} PRIVATE ${PCRE_INCLUDE_DIRS})
    target_link_libraries(${target} PRIVATE ${PCRE_LIBRARIES})
    target_compile_definitions(${target} PRIVATE HAVE_LIBPCRE HAVE_PCRE_H)
  endif()
endmacro()

configure_getdata_target(getdata)
configure_getdata_target(getdata_static)

install(FILES ${CMAKE_BINARY_DIR}/getdata.h DESTINATION include)

# Install both shared and static libraries
install(TARGETS getdata getdata_static
  LIBRARY DESTINATION lib
  RUNTIME DESTINATION bin
  ARCHIVE DESTINATION lib)
