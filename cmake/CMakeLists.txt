cmake_minimum_required(VERSION 2.6.4)

project(getdata)


#set(CMAKE_RUNTIME_OUTPUT_DIRECTORY  ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY  ${CMAKE_BINARY_DIR}/lib) 
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY  ${CMAKE_BINARY_DIR}/lib) 


if(NOT GD_DIR)
	set(GD_DIR ${CMAKE_SOURCE_DIR}/..)
endif()


macro(GD_FILES name folder)
	file(GLOB ${name}_folder_sources ${GD_DIR}/${folder}/*.c) 
	file(GLOB ${name}_folder_sources_cpp ${GD_DIR}/${folder}/*.cpp)
	file(GLOB ${name}_folder_headers ${GD_DIR}/${folder}/*.h) 
	set(${name}_sources ${${name}_sources} ${${name}_folder_sources} ${${name}_folder_sources_cpp})
	set(${name}_headers ${${name}_headers} ${${name}_folder_headers})
	include_directories(${CMAKE_SOURCE_DIR}/${folder})
endmacro()

#TODO add configure
add_definitions(
	-DPACKAGE_NAME=\"GetData\"
	-DPACKAGE_VERSION=\"0.7.2\"
	-DPACKAGE_BUGREPORT=\"getdata-devel@lists.sourceforge.net\")


## Substitutions needed to build getdata.h
# build in ANSI C mode
set(DEFINE_GD_NO_C99_API "#define GD_NO_C99_API")
 
# kst2 doesn't need the legacy API
set(DEFINE_GD_LEGACY_API "/* #undef GD_LEGACY_API */")

# MSVCRT integer types
set(DEFINE_gd_int16_t  "#define gd_int16_t short int")
set(DEFINE_gd_uint16_t "#define gd_uint16_t unsigned short int")
set(DEFINE_gd_int64_t  "#define gd_int64_t __int64")



if(MSVC)
	add_definitions(-D_CRT_SECURE_NO_WARNINGS -D_CRT_NONSTDC_NO_WARNINGS)
	add_definitions(
		-D__MSVCRT__
		-D_USE_MATH_DEFINES
		-DHAVE_DIRENT_H
		-DHAVE_DIRECT_H
		-DHAVE_LIBGEN_H
		-DSTDC_HEADERS
		-DHAVE_INTTYPES_H
		-DHAVE_IO_H
		-DHAVE_STDINT_H
		-DHAVE__STAT64
		-DHAVE_STRUCT__STAT64
		-DHAVE__COMMIT
		-DHAVE__STRTOI64
		-DHAVE__STRTOUI64
		-DHAVE__GETCWD
		)
	set(CMAKE_DEBUG_POSTFIX d)
	
	# ANSI C
	#set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /Za")
	
	# Suppress warnings
	string(REGEX REPLACE "/W[0-4]" "/W0" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
	
	include_directories(${GD_DIR}/src/msvc)
endif()

if(MINGW)
	add_definitions(
			-D__MSVCRT__
			-D__MSVCRT_VERSION__=0x0601
			-DSTDC_HEADERS
			-DHAVE_IO_H
			-DHAVE_STDINT_H
			-DHAVE_COMPLEX_H
			-DHAVE_OFF64_T
			-DHAVE_LIBGEN_H
			-DHAVE_STRTOLL
			-DHAVE_DIRENT_H
			-DHAVE_DIRECT_H
			-DHAVE__COMMIT
			-DHAVE__STAT64
			-DHAVE__STRTOI64
			-DHAVE__STRTOUI64
			-DHAVE_STRUCT___STAT64
			-DHAVE__GETCWD
			-DMKDIR_NO_MODE
			)
endif()

set(GD_ENABLE_C99_API 0)
configure_file(${GD_DIR}/src/getdata.h.in ${CMAKE_BINARY_DIR}/getdata.h @ONLY)

include_directories(${GD_DIR}/src ${CMAKE_CURRENT_BINARY_DIR})

if (CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
	set(CMAKE_INSTALL_PREFIX ${CMAKE_BINARY_DIR}/INSTALLED CACHE PATH "install path" FORCE)
endif()

add_subdirectory(src)
add_subdirectory(bindings)

if(GD_TEST)
	enable_testing()
	add_subdirectory(test)
endif()



