cmake_minimum_required(VERSION 3.15)

project(getdata)

option(BUILD_PYTHON "Build Python bindings" ON)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY  ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY  ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY  ${CMAKE_BINARY_DIR}/lib)

if(BUILD_PYTHON)
  set(BUILD_SHARED_LIBS ON CACHE BOOL "Build shared libraries" FORCE)
  find_package(Python3 REQUIRED COMPONENTS Interpreter Development.Module NumPy)
  set(HAVE_NUMPY_ARRAYOBJECT_H TRUE)
endif()

# Optional encoding libraries
find_package(BZip2)
find_package(ZLIB)
find_package(LibLZMA)
find_package(PkgConfig)
if(PKG_CONFIG_FOUND)
  pkg_check_modules(PCRE libpcre)
  pkg_check_modules(FLAC flac)
  pkg_check_modules(ZZIP zziplib)
endif()

# Find external compression tools for tests
find_program(BZIP2 bzip2)
find_program(BUNZIP2 bunzip2)
find_program(FLAC flac)
find_program(GZIP gzip)
find_program(GUNZIP gunzip)
find_program(UNZIP unzip)
find_program(XZ xz)
find_program(ZIP zip)

if(BZIP2_FOUND)
  include(CheckIncludeFile)
  check_include_file(bzlib.h HAVE_BZLIB_H)
  if(NOT HAVE_BZLIB_H)
    set(BZIP2_FOUND FALSE)
  endif()
endif()

if(ZLIB_FOUND)
  check_include_file(zlib.h HAVE_ZLIB_H)
  if(NOT HAVE_ZLIB_H)
    set(ZLIB_FOUND FALSE)
  endif()
endif()

if(LIBLZMA_FOUND)
  check_include_file(lzma.h HAVE_LZMA_H)
  if(NOT HAVE_LZMA_H)
    set(LIBLZMA_FOUND FALSE)
  endif()
endif()

if(FLAC_FOUND)
  check_include_file(FLAC/all.h HAVE_FLAC_ALL_H)
  if(NOT HAVE_FLAC_ALL_H)
    set(FLAC_FOUND FALSE)
  endif()
endif()

if(ZZIP_FOUND)
  check_include_file(zzip/lib.h HAVE_ZZIP_LIB_H)
  if(NOT HAVE_ZZIP_LIB_H)
    set(ZZIP_FOUND FALSE)
  endif()
endif()

if(PCRE_FOUND)
  check_include_file(pcre.h HAVE_PCRE_H)
  if(NOT HAVE_PCRE_H)
    set(PCRE_FOUND FALSE)
  endif()
endif()

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

add_definitions(
  -DUNALIGNED_ACCESS_OK
  )

# kst2 doesn't need the legacy API
set(DEFINE_GD_LEGACY_API "/* #undef GD_LEGACY_API */")

# Version macros
set(DEFINE_GD_GETDATA_VERSION  "#define GD_GETDATA_VERSION \"0.11.0\"")
set(DEFINE_GD_GETDATA_INT_VERSION  "#define GD_GETDATA_INT_VERSION 1100")

# Platform-independent configuration checks
include(CheckIncludeFile)
include(CheckSymbolExists)
include(CheckTypeSize)
include(CheckFunctionExists)
include(CheckCSourceCompiles)

# Check for headers
check_include_file(asm/unaligned.h HAVE_ASM_UNALIGNED_H)
check_include_file(Availability.h HAVE_AVAILABILITY_H)
check_include_file(byteswap.h HAVE_BYTESWAP_H)
check_include_file(complex.h HAVE_COMPLEX_H)
check_include_file(crtdefs.h HAVE_CRTDEFS_H)
check_include_file(ctype.h HAVE_CTYPE_H)
check_include_file(direct.h HAVE_DIRECT_H)
check_include_file(dirent.h HAVE_DIRENT_H)
check_include_file(errno.h HAVE_ERRNO_H)
check_include_file(fcntl.h HAVE_FCNTL_H)
check_include_file(features.h HAVE_FEATURES_H)
check_include_file(float.h HAVE_FLOAT_H)
check_include_file(inttypes.h HAVE_INTTYPES_H)
check_include_file(io.h HAVE_IO_H)
check_include_file(libgen.h HAVE_LIBGEN_H)
check_include_file(libkern/OSByteOrder.h HAVE_LIBKERN_OSBYTEORDER_H)
check_include_file(limits.h HAVE_LIMITS_H)
check_include_file(math.h HAVE_MATH_H)
check_include_file(memory.h HAVE_MEMORY_H)
check_include_file(regex.h HAVE_REGEX_H)
check_include_file(stddef.h HAVE_STDDEF_H)
check_include_file(stdint.h HAVE_STDINT_H)
check_include_file(stdlib.h HAVE_STDLIB_H)
check_include_file(string.h HAVE_STRING_H)
check_include_file(strings.h HAVE_STRINGS_H)
check_include_file(sys/endian.h HAVE_SYS_ENDIAN_H)
check_include_file(sys/file.h HAVE_SYS_FILE_H)
check_include_file(sys/param.h HAVE_SYS_PARAM_H)
check_include_file(sys/stat.h HAVE_SYS_STAT_H)
check_include_file(sys/types.h HAVE_SYS_TYPES_H)
check_include_file(time.h HAVE_TIME_H)
check_include_file(unistd.h HAVE_UNISTD_H)

# Check for functions
check_symbol_exists(basename "string.h" HAVE_BASENAME)
check_symbol_exists(dirname "libgen.h" HAVE_DIRNAME)
check_function_exists(fchmod HAVE_FCHMOD)
check_function_exists(fdopendir HAVE_FDOPENDIR)
check_function_exists(fseeko HAVE_FSEEKO)
check_function_exists(fseeko64 HAVE_FSEEKO64)
check_function_exists(fstatat HAVE_FSTATAT)
check_function_exists(fsync HAVE_FSYNC)
check_function_exists(ftello HAVE_FTELLO)
check_function_exists(ftello64 HAVE_FTELLO64)
check_function_exists(ftruncate HAVE_FTRUNCATE)
check_function_exists(getcwd HAVE_GETCWD)
check_function_exists(gmtime HAVE_GMTIME)
check_function_exists(gmtime_r HAVE_GMTIME_R)
check_function_exists(lseek64 HAVE_LSEEK64)
check_function_exists(lstat HAVE_LSTAT)
check_function_exists(openat HAVE_OPENAT)
check_function_exists(readdir_r HAVE_READDIR_R)
check_function_exists(readlink HAVE_READLINK)
check_function_exists(regcomp HAVE_REGCOMP)
check_function_exists(renameat HAVE_RENAMEAT)
check_function_exists(snprintf HAVE_SNPRINTF)
check_function_exists(strtoll HAVE_STRTOLL)
check_function_exists(strtoull HAVE_STRTOULL)
check_function_exists(symlink HAVE_SYMLINK)
check_function_exists(unlinkat HAVE_UNLINKAT)

if(UNIX)
  set(CMAKE_REQUIRED_LIBRARIES m)
endif()
check_function_exists(cabs HAVE_CABS)
check_function_exists(isnan HAVE_ISNAN)
check_function_exists(nan HAVE_NAN)
unset(CMAKE_REQUIRED_LIBRARIES)

# Check for strerror_r (GNU vs XSI versions)
# GNU version returns char*, XSI returns int - prefer XSI
set(CMAKE_REQUIRED_FLAGS "-Werror")
check_c_source_compiles("
  #include <string.h>
  int main() {
    char buf[100];
    char *p = strerror_r(0, buf, sizeof(buf));
    return 0;
  }
" HAVE_STRERROR_R_GNU)
unset(CMAKE_REQUIRED_FLAGS)

if(NOT HAVE_STRERROR_R_GNU)
  check_function_exists(strerror_r HAVE_STRERROR_R)
endif()

# Check for type sizes (all platforms)
set(CMAKE_EXTRA_INCLUDE_FILES "stdint.h")
check_type_size(int SIZEOF_INT)
check_type_size("unsigned int" SIZEOF_UNSIGNED_INT)
check_type_size("short int" SIZEOF_SHORT_INT)
check_type_size("unsigned short int" SIZEOF_UNSIGNED_SHORT_INT)
check_type_size("long int" SIZEOF_LONG_INT)
check_type_size("unsigned long int" SIZEOF_UNSIGNED_LONG_INT)
check_type_size("long long int" SIZEOF_LONG_LONG_INT)
check_type_size("unsigned long long int" SIZEOF_UNSIGNED_LONG_LONG_INT)
check_type_size(size_t SIZEOF_SIZE_T)
check_type_size("void*" SIZEOF_VOID_P)
check_type_size(off64_t OFF64_T)
set(CMAKE_EXTRA_INCLUDE_FILES)

# Determine 64-bit integer types (matching autotools logic)
if(SIZEOF_INT EQUAL 8)
  set(gd_int64_t "int")
elseif(SIZEOF_SHORT_INT EQUAL 8)
  set(gd_int64_t "short int")
elseif(SIZEOF_LONG_INT EQUAL 8)
  set(gd_int64_t "long int")
elseif(SIZEOF_LONG_LONG_INT EQUAL 8)
  set(gd_int64_t "long long int")
endif()

if(SIZEOF_UNSIGNED_INT EQUAL 8)
  set(gd_uint64_t "unsigned int")
elseif(SIZEOF_UNSIGNED_SHORT_INT EQUAL 8)
  set(gd_uint64_t "unsigned short int")
elseif(SIZEOF_UNSIGNED_LONG_INT EQUAL 8)
  set(gd_uint64_t "unsigned long int")
elseif(SIZEOF_UNSIGNED_LONG_LONG_INT EQUAL 8)
  set(gd_uint64_t "unsigned long long int")
endif()

if(MSVC)
  # build in ANSI C mode
  set(DEFINE_GD_NO_C99_API "#define GD_NO_C99_API")

  # MSVCRT integer types
  set(DEFINE_gd_int64_t  "#define gd_int64_t  __int64")
  set(DEFINE_gd_uint64_t "#define gd_uint64_t unsigned __int64")

  # Check if _strtoi64 and _strtoui64 are declared in modern SDK
  include(CheckSymbolExists)
  check_symbol_exists(_strtoi64 "stdlib.h" HAVE_DECL__STRTOI64)
  check_symbol_exists(_strtoui64 "stdlib.h" HAVE_DECL__STRTOUI64)

  # Check if isfinite is declared in modern SDK (C99 function)
  check_symbol_exists(isfinite "math.h" HAVE_DECL_ISFINITE)

  add_definitions(
    -D_CRT_SECURE_NO_WARNINGS
    -D_CRT_NONSTDC_NO_WARNINGS
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
    -DHAVE_STRUCT___STAT64
    -DHAVE_ISNAN
    -DHAVE_DIRENT_H
    -DHAVE_DIRNAME
    -DHAVE_LIBGEN_H
    -DGD_DIRSEP='\\\\'
    -DMKDIR_NO_MODE
    -Dmode_t=int
    -Drestrict=
  )

  if(HAVE_DECL__STRTOI64)
    add_definitions(-DHAVE_DECL__STRTOI64=1)
  else()
    add_definitions(-DHAVE_DECL__STRTOI64=0)
  endif()
  if(HAVE_DECL__STRTOUI64)
    add_definitions(-DHAVE_DECL__STRTOUI64=1)
  else()
    add_definitions(-DHAVE_DECL__STRTOUI64=0)
  endif()
  if(HAVE_DECL_ISFINITE)
    add_definitions(-DHAVE_DECL_ISFINITE=1)
  else()
    add_definitions(-DHAVE_DECL_ISFINITE=0)
  endif()

  set(CMAKE_DEBUG_POSTFIX d)
  string(REGEX REPLACE "/W[0-4]" "/W0" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
  include_directories(${GD_DIR}/src/msvc)

elseif(MINGW)

  set(DEFINE_gd_int64_t  "#define gd_int64_t  __int64")
  set(DEFINE_gd_uint64_t "#define gd_uint64_t unsigned __int64")

  add_definitions(
    -D__MSVCRT_VERSION__=0x0601
    -DGD_DIRSEP='\\\\'
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
    -DHAVE_STRUCT___STAT64
    -DMKDIR_NO_MODE
    -Drestrict=__restrict
  )
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=gnu99")

else() # Unix/Linux/macOS
  set(DEFINE_gd_int64_t  "#define gd_int64_t  ${gd_int64_t}")
  set(DEFINE_gd_uint64_t "#define gd_uint64_t ${gd_uint64_t}")

  set(CMAKE_C_STANDARD 99)
  # _DEFAULT_SOURCE provides BSD extensions (M_PI, mktemp) with XSI strerror_r (glibc >= 2.19)
  # _BSD_SOURCE needed for older glibc (e.g. CentOS 7 with glibc 2.17)
  add_definitions(-DGD_DIRSEP='/' -D_POSIX_C_SOURCE=200809L -D_DEFAULT_SOURCE -D_BSD_SOURCE)

  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Drestrict=")
endif()

# HAVE_* macros are defined in gd_config.h via #cmakedefine
# Only special logic for getdata.h template variables remains here

# Set template variables for getdata.h (these control API availability)
if(HAVE_REGEX_H AND HAVE_REGCOMP)
  set(DEFINE_GD_NO_REGEX "/* #undef GD_NO_REGEX */")
else()
  set(DEFINE_GD_NO_REGEX "#define GD_NO_REGEX 1")
endif()

if(PCRE_FOUND AND HAVE_PCRE_H)
  set(DEFINE_GD_NO_PCRE "/* #undef GD_NO_PCRE */")
else()
  set(DEFINE_GD_NO_PCRE "#define GD_NO_PCRE 1")
endif()

add_definitions(
  -DSIZEOF_INT=${SIZEOF_INT}
  -DSIZEOF_UNSIGNED_INT=${SIZEOF_UNSIGNED_INT}
  -DSIZEOF_SIZE_T=${SIZEOF_SIZE_T}
  -DSIZEOF_VOID_P=${SIZEOF_VOID_P}
  -DGETDATA_MAJOR=0
  -DGETDATA_MINOR=11
  -DGETDATA_REVISION=0
  -DSTDC_HEADERS
  -DGD_RESTRICT_ARRAY_OK=1
)

if(GD_TEST)
  enable_testing()
endif()

configure_file(${GD_DIR}/src/getdata.h.in ${CMAKE_BINARY_DIR}/getdata.h @ONLY)

include_directories(${GD_DIR}/src ${CMAKE_CURRENT_BINARY_DIR})

if (CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX ${CMAKE_BINARY_DIR}/INSTALLED CACHE PATH "install path" FORCE)
endif()

# Generate gd_config.h
configure_file(${CMAKE_SOURCE_DIR}/gd_config.h.in ${GD_DIR}/src/gd_config.h @ONLY)

add_subdirectory(src)
add_subdirectory(bindings)

if(GD_UTIL)
  add_subdirectory(util)
endif()

if(GD_TEST)
  add_subdirectory(test)
endif()
