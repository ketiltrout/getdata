cmake_minimum_required(VERSION 2.6.4)

project(getdata)



set(CMAKE_RUNTIME_OUTPUT_DIRECTORY  ${CMAKE_BINARY_DIR}/bin)
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
  -DPACKAGE_VERSION=\"0.10.x\"
  -DGETDATA_LIB_VERSION=\"0.10.x\"
  -DPACKAGE_BUGREPORT=\"https://github.com/ketiltrout/getdata/issues\"
  -DUNALIGNED_ACCESS_OK
  )

# kst2 doesn't need the legacy API
set(DEFINE_GD_LEGACY_API "/* #undef GD_LEGACY_API */")

# Disable REGEX support
set(DEFINE_GD_NO_REGEX "#define GD_NO_REGEX 1")
set(DEFINE_GD_NO_PCRE "#define GD_NO_PCRE 1")

# Version macros
set(DEFINE_GD_GETDATA_VERSION  "#define GD_GETDATA_VERSION \"0.10.x\"")
set(DEFINE_GD_GETDATA_INT_VERSION  "#define GD_GETDATA_INT_VERSION 1000")

if(MSVC)
  ## Substitutions needed to build getdata.h
  # build in ANSI C mode
  set(DEFINE_GD_NO_C99_API "#define GD_NO_C99_API")

  # MSVCRT integer types
  set(DEFINE_gd_int64_t  "#define gd_int64_t  __int64")
  set(DEFINE_gd_uint64_t "#define gd_uint64_t unsigned __int64")

  add_definitions(-D_CRT_SECURE_NO_WARNINGS -D_CRT_NONSTDC_NO_WARNINGS)
  add_definitions(
    -D__MSVCRT__
    -D_USE_MATH_DEFINES
    -DHAVE__CHSIZE
    -DHAVE__COMMIT
    -DHAVE__FINITE
    -DHAVE__FSTAT64
    -DHAVE__GETCWD
    -DHAVE__ISNAN
    -DHAVE__MKDIR
    -DHAVE__RMDIR
    -DHAVE__SNPRINTF
    -DHAVE__STAT64
    -DHAVE__STRTOI64
    -DHAVE__STRTOUI64
    -DHAVE_DECL__STRTOI64=0
    -DHAVE_DECL__STRTOUI64=0
    -DHAVE_DIRENT_H
    -DHAVE_DIRECT_H
    -DHAVE_FCNTL_H
    -DHAVE_FLOAT_H
    -DHAVE_LIBGEN_H
    -DHAVE_INTTYPES_H
    -DHAVE_IO_H
    -DHAVE_STDINT_H
    -DHAVE_STRUCT___STAT64
    -DHAVE_SYS_STAT_H
    -DGD_DIRSEP='\\\\'
    -DMKDIR_NO_MODE
    -Dmode_t=int
    -Drestrict=
    -DSIZEOF_INT=4
    -DSIZEOF_UNSIGNED_INT=4
    )
  set(CMAKE_DEBUG_POSTFIX d)

  # ANSI C
  #set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /Za")

  # Suppress warnings
  string(REGEX REPLACE "/W[0-4]" "/W0" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")

  include_directories(${GD_DIR}/src/msvc)
endif()

if(MINGW)
  set(DEFINE_gd_int64_t  "#define gd_int64_t  __int64")
  set(DEFINE_gd_uint64_t "#define gd_uint64_t unsigned __int64")

  add_definitions(
    -D__MSVCRT_VERSION__=0x0601
    -DGD_DIRSEP='\\\\'
    -DGD_RESTRICT_ARRAY_OK=1
    -DHAVE_BASENAME
    -DHAVE_CABS
    -DHAVE_COMPLEX_H
    -DHAVE_DIRECT_H
    -DHAVE_DIRENT_H
    -DHAVE_FCNTL_H
    -DHAVE_FLOAT_H
    -DHAVE_FSEEKO64
    -DHAVE_FTELLO64
    -DHAVE_FTRUNCATE
    -DHAVE_GETCWD
    -DHAVE_INTTYPES_H
    -DHAVE_IO_H
    -DHAVE_ISNAN
    -DHAVE_LIBGEN_H
    -DHAVE_LSEEK64
    -DHAVE_MEMORY_H
    -DHAVE_NAN
    -DHAVE_OFF64_T
    -DHAVE_SNPRINTF
    -DHAVE_STDDEF_H
    -DHAVE_STDINT_H
    -DHAVE_STDLIB_H
    -DHAVE_STRINGS_H
    -DHAVE_STRING_H
    -DHAVE_STRTOLL
    -DHAVE_STRTOULL
    -DHAVE_STRUCT___STAT64
    -DHAVE_SYS_FILE_H
    -DHAVE_SYS_PARAM_H
    -DHAVE_SYS_STAT_H
    -DHAVE_SYS_TYPES_H
    -DHAVE_UNISTD_H
    -DHAVE__CHSIZE
    -DHAVE__COMMIT
    -DHAVE__COMPLEX_DOUBLE
    -DHAVE__COMPLEX_FLOAT
    -DHAVE__FDOPEN
    -DHAVE__FSTAT
    -DHAVE__FSTAT64
    -DHAVE__GETCWD
    -DHAVE__ISNAN
    -DHAVE__LSEEKI64
    -DHAVE__MKDIR
    -DHAVE__OPEN
    -DHAVE__READ
    -DHAVE__RMDIR
    -DHAVE__SNPRINTF
    -DHAVE__STAT64
    -DHAVE__STRTOI64
    -DHAVE__STRTOUI64
    -DHAVE__UNLINK
    -DHAVE__WRITE
    -DMKDIR_NO_MODE
    -DSIZEOF_INT=4
    -DSIZEOF_UNSIGNED_INT=4
    -DSTDC_HEADERS
    -D_BSD_SOURCE=1
    -D_GNU_SOURCE=1
    -D_POSIX_C_SOURCE=1200809L
    -D_POSIX_SOURCE=1
    -D_SVID_SOURCE=0
    -Drestrict=__restrict
    )
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=gnu99")
endif()

if(GD_TEST)
  enable_testing()
endif()

configure_file(${GD_DIR}/src/getdata.h.in ${CMAKE_BINARY_DIR}/getdata.h @ONLY)

include_directories(${GD_DIR}/src ${CMAKE_CURRENT_BINARY_DIR})

if (CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX ${CMAKE_BINARY_DIR}/INSTALLED CACHE PATH "install path" FORCE)
endif()

add_subdirectory(src)
add_subdirectory(bindings)

if(GD_UTIL)
  add_subdirectory(util)
endif()

if(GD_TEST)
  add_subdirectory(test)
endif()



