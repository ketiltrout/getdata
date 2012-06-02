dnl Copyright (C) 2008-2011 D. V. Wiebe
dnl
dnllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllll
dnl
dnl This file is part of the GetData project.
dnl
dnl GetData is free software; you can redistribute it and/or modify it under
dnl the terms of the GNU Lesser General Public License as published by the
dnl Free Software Foundation; either version 2.1 of the License, or (at your
dnl option) any later version.
dnl
dnl GetData is distributed in the hope that it will be useful, but WITHOUT
dnl ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
dnl FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
dnl License for more details.
dnl
dnl You should have received a copy of the GNU Lesser General Public License
dnl along with GetData; if not, write to the Free Software Foundation, Inc.,
dnl 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
dnl

m4_include([m4/version.m4])

AC_INIT([GetData],[getdata_version],[getdata-devel@lists.sourceforge.net])

AC_DEFINE([GETDATA_MAJOR], [getdata_major], [ The major version number ])
AC_DEFINE([GETDATA_MINOR], [getdata_minor], [ The minor version number ])
AC_DEFINE([GETDATA_REVISION], [getdata_revision], [ The revision number ])
AC_DEFINE([GETDATA_VERSION_SUFFIX], ["getdata_extra"],
          [ Other qualifiers on the version number triplet (if any) ])

AC_PREREQ([2.63])
AC_CONFIG_MACRO_DIR([m4])

LT_PREREQ([2.2.6b])

AC_ARG_VAR([BUILDCC],
           [C compiler command on the build system (only needed if cross-compiling)])

dnl export version information from m4/version.m4
AC_SUBST(GETDATA_IFACE_VERSION, [getdata_iface_version])
AC_SUBST(GETDATA_IMPL_REVISION, [getdata_impl_revision])
AC_SUBST(GETDATA_IFACE_AGE, [getdata_iface_age])
AC_SUBST(GETDATAXX_VERSION, [getdataxx_version])
AC_SUBST(FGETDATA_VERSION, [fgetdata_version])
AC_SUBST(F95GETDATA_VERSION, [f95getdata_version])

AC_CONFIG_SRCDIR([src/getdata.c])
AC_CONFIG_HEADER([src/gd_config.h])

dnl Features
BUINDINGS_BUILT=
BINDINGS_LEFT=
ENCODINGS_BUILT=" raw ascii sie"
ENCODINGS_LEFT=
PRIVATE_LIBS=

echo "*** Checking GetData configuration"
echo

dnl Build/Host triplets
AC_CANONICAL_BUILD
AC_CANONICAL_HOST

dnl Legacy API
AC_ARG_ENABLE(legacy-api, AS_HELP_STRING([--disable-legacy-api],
              [don't include the legacy API wrapper in the library]), dnl'
              [
               case "${enableval}" in
                 no) include_legacy_api="no" ;;
                 *) include_legacy_api="yes" ;;
               esac
              ], [ include_legacy_api="yes" ])
AC_MSG_CHECKING([whether to include the legacy API])
AC_MSG_RESULT([$include_legacy_api])

if test "x$include_legacy_api" != "xno"; then
  DEFINE_GD_LEGACY_API="#define GD_LEGACY_API"
else
  DEFINE_GD_LEGACY_API="/* #undef GD_LEGACY_API */"
fi
AC_SUBST([DEFINE_GD_LEGACY_API])

dnl debugging
AC_ARG_ENABLE(assert, AS_HELP_STRING([--enable-assert],
              [enable assertions]),
              [
               case "${enableval}" in
                 yes) enable_assert="yes" ;;
                 *) enable_assert="no" ;;
               esac
              ], [ enable_assert="no" ])
AC_ARG_ENABLE(debug, AS_HELP_STRING([--enable-debug],
              [enable debugging messages]),
              [
               case "${enableval}" in
                 yes) enable_debug="yes" ;;
                 *) enable_debug="no" ;;
               esac
              ], [ enable_debug="no" ])

AC_MSG_CHECKING([whether to enable debugging messages])
AC_MSG_RESULT([$enable_debug])
if test "x$enable_debug" = "xyes"; then
  AC_DEFINE([GETDATA_DEBUG], [],
            [ Define to enable debugging messages ])
  enable_assert="yes"
  GETDATA_DEBUG=1
else
  GETDATA_DEBUG=0
fi
AC_SUBST([GETDATA_DEBUG])

AC_MSG_CHECKING([whether to enable assertions])
AC_MSG_RESULT([$enable_assert])
if test "x$enable_assert" = "xno"; then
  AC_DEFINE(NDEBUG, 1, [ Define to disable assertions ])
fi

dnl C99 stuff
AC_ARG_ENABLE(ansi-c, AS_HELP_STRING([--enable-ansi-c],
              [use ANSI C (C89) work-arounds for C99 code]),
              [
               if test "x${enableval}" != "xno"; then
                 disable_c99="yes";
               else
                 disable_c99="no";
               fi
              ], [disable_c99="no"])
AC_MSG_CHECKING([whether to build in ANSI C compliance mode])
AC_MSG_RESULT([$disable_c99])

dnl bindings, globally
make_cxxbindings="yes"
make_f77bindings="yes"
make_pybindings="yes"
make_perlbindings="yes"
make_idlbindings="yes"
AC_ARG_ENABLE(bindings, AS_HELP_STRING([--disable-bindings],
              [don't build any bindings, just build the C library]),
              [
               if test "x${enableval}" = "xno"; then
                 make_cxxbindings="no"
                 make_f77bindings="no"
                 make_pybindings="no"
                 make_idlbindings="no"
                 make_perlbindings="no"
               fi
              ])

dnl C++ bindings
AC_ARG_ENABLE(cplusplus, AS_HELP_STRING([--disable-cplusplus],
              [don't build the C++ bindings (libgetdata++)]),
              [
               case "${enableval}" in
                 no) make_cxxbindings="no" ;;
                 *) make_cxxbindings="yes" ;;
               esac
              ])
AC_MSG_CHECKING([whether to include the C++ bindings])
AC_MSG_RESULT([$make_cxxbindings])

dnl Fortran 77 bindings
AC_ARG_ENABLE(fortran, AS_HELP_STRING([--disable-fortran],
              [don't build the Fortran 77 bindings (libfgetdata) nor the
               Fortran 95 bindings (libf95getdata)]),
              [
               case "${enableval}" in
                 no) make_f77bindings="no" ;;
                 *) make_f77bindings="yes" ;;
               esac
              ])

dnl Fortran 95 bindings
AC_ARG_ENABLE(fortran95, AS_HELP_STRING([--disable-fortran95],
              [don't build the Fortran 95 bindings (libf95getdata)]),
              [
               case "${enableval}" in
                 no) make_f95bindings="no" ;;
                 *) make_f95bindings="yes" ;;
               esac
              ], [ make_f95bindings="$make_f77bindings" ])

AC_MSG_CHECKING([whether to include the Fortran 95 bindings])
AC_MSG_RESULT([$make_f95bindings])

if test "x$make_f77bindings$make_f95bindings" = "xnoyes"; then
  make_f77bindings="yes (required by Fortran 95 bindings)"
fi

AC_MSG_CHECKING([whether to include the Fortran 77 bindings])
AC_MSG_RESULT([$make_f77bindings])

dnl DL library path munging for test suite
case "${host}" in
  *-apple-darwin*) DL_LIBRARY_PATH="DYLD_LIBRARY_PATH" ;;
  *) DL_LIBRARY_PATH="LD_LIBRARY_PATH" ;;
esac
AC_SUBST([DL_LIBRARY_PATH])

dnl Python bindings
AC_ARG_ENABLE(python, AS_HELP_STRING([--disable-python],
              [don't build the Python bindings (pygetdata)]),
              [
               case "${enableval}" in
                 no) make_pybindings="no" ;;
                 *) make_pybindings="yes" ;;
               esac
              ])

AC_MSG_CHECKING([whether to include the Python bindings])
if test "x$disable_c99" = "xyes"; then
  make_pybindings="no";
  AC_MSG_RESULT([no (requires C99 compiler)])
else
  AC_MSG_RESULT([$make_pybindings])
fi

dnl IDL bindings
AC_ARG_ENABLE(idl, AS_HELP_STRING([--disable-idl],
       [don't build the Interactive Data Language (IDL) bindings (IDL_GetData)]),
              [
               case "${enableval}" in
                 no) make_idlbindings="no" ;;
                 *) make_idlbindings="yes" ;;
               esac
              ])

AC_MSG_CHECKING([whether to include the IDL bindings])
if test "x$disable_c99" = "xyes"; then
  make_idlbindings="no";
  AC_MSG_RESULT([no (requires C99 compiler)])
else
  AC_MSG_RESULT([$make_idlbindings])
fi

dnl Perl bindings
AC_ARG_ENABLE(perl, AS_HELP_STRING([--disable-perl],
              [don't build the Perl bindings (GetData)]),
              [
               case "${enableval}" in
                 no) make_perlbindings="no" ;;
                 *) make_perlbindings="yes" ;;
               esac
              ])

AC_MSG_CHECKING([whether to include the Perl bindings])
if test "x$disable_c99" = "xyes"; then
  make_perlbindings="no";
  AC_MSG_RESULT([no (requires C99 compiler)])
else
  AC_MSG_RESULT([$make_perlbindings])
fi

dnl Modules
AC_ARG_ENABLE(modules, AS_HELP_STRING([--enable-modules],
              [build external encodings in dynamically loaded modules instead
               of directly into the core GetData library]),
               [
                case "${enableval}" in
                  no) use_modules="no" ;;
                  *) use_modules="yes" ;;
                esac
                ], [ use_modules="no" ])
AC_MSG_CHECKING([whether to enable getdata modules])
AC_MSG_RESULT([$use_modules])
if test "x$use_modules" != "xno"; then
AC_DEFINE([USE_MODULES], [], [ Define if building with modules ])
fi

AC_ARG_WITH(module-dir, AS_HELP_STRING([--with-module-dir=DIR],
            [if building modules, install them in DIR.  This is ignored if
             --enable-modules isn't specified [default: LIBDIR/getdata]]),dnl'
            [
             case "${withval}" in
               no) moduledir="$libdir/getdata" ;;
               *) moduledir="${withval}" ;;
             esac
             ], [ moduledir="$libdir/getdata" ])
AC_SUBST([moduledir])

dnl LTDL
AC_ARG_WITH([ltdl], AS_HELP_STRING([--with-ltdl=PREFIX],
              [if building modules, use the GNU ltdl library located in
               PREFIX [autodetect]]),
              [
               case "${withval}" in
                 no) ltdl_prefix= ;;
                 *) ltdl_prefix="${withval}" ;;
               esac
              ], [ ltdl_prefix= ])

dnl Kill switch for external libraries
AC_ARG_WITH([external-libraries], AS_HELP_STRING([--without-external-libraries],
            [disable all features relying on external libraries]),
            [
             case "${withval}" in
               no) no_extern="yes";;
               *) no_extern="no";;
             esac
             ], [ no_extern="no" ])

dnl Programs
echo
echo "*** Initialising build system"
echo
AM_INIT_AUTOMAKE
AC_PROG_LN_S
AC_PROG_GREP
AC_PROG_AWK
AC_PROG_SED
AC_CHECK_PROGS([DIFF], [diff], [])
GD_MSYS_SHELL

AC_MSG_CHECKING([whether ${host} supports fast unaligned memory access])
case "${host}" in
  i?86-*-*|powerpc-*-*|x86_64-*-*) gd_unaligned_ok=yes;
  AC_DEFINE([UNALIGNED_ACCESS_OK], [1],
            [Define to 1 if the platform supports fast unaligned memory access])
  ;;
  *) gd_unaligned_ok=no ;;
esac
AC_MSG_RESULT([$gd_unaligned_ok])

AC_MSG_CHECKING([the directory separator])
case "${host}" in
  *-*-djgpp|*-*-mingw32) GD_FDIRSEP=\\; GD_DIRSEP=\\\\ ;;
  *) GD_FDIRSEP=/; GD_DIRSEP=/ ;;
esac
AC_SUBST([GD_FDIRSEP])
AC_SUBST([GD_DIRSEP])
AC_DEFINE_UNQUOTED([GD_DIRSEP], ['$GD_DIRSEP'],
                   [ The separator between directory elements ])
AC_MSG_RESULT([$GD_FDIRSEP])

echo
echo "*** Checking C compiler characteristics"
echo
AC_PROG_CC
AC_PROG_CC_C_O

if test "x$disable_c99" != "xyes"; then
  AC_PROG_CC_C99
  if test "x$ac_cv_prog_cc_c99" = "xno"; then
    disable_c99="yes"
    make_pybindings="no"
    make_idlbindings="no"
  fi
fi
if test "x$disable_c99" = "xyes"; then
  DEFINE_GD_NO_C99_API="#define GD_NO_C99_API"
else
  DEFINE_GD_NO_C99_API="/* #undef GD_NO_C99_API */"
fi
AC_SUBST([DEFINE_GD_NO_C99_API])

AC_HEADER_STDC
AM_PROG_AS

echo
echo "*** Checking C++ compiler characteristics"
echo
AC_PROG_CXX
AC_PROG_CXX_C_O

if test "x$make_f77bindings" != "xno"; then
  echo
  echo "*** Checking Fortran 77 compiler characteristics"
  echo
  AC_PROG_F77
  AC_PROG_F77_C_O
fi
if test "x$make_f95bindings" != "xno"; then
  echo
  echo "*** Checking Fortran 90 compiler characteristics"
  echo
  AC_PROG_FC
  AC_PROG_FC_C_O
  # does $FC accept fortran code with .f90 extension?
  AC_MSG_CHECKING([whether the Fortran 90 compiler works])
  AC_LANG_PUSH([Fortran])
  ac_ext="f90"
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM()],
  [AC_MSG_RESULT([yes])],
  [AC_MSG_RESULT([no]); FC=;])
  AC_LANG_POP
fi
if test "x${cross_compiling}" != "xno"; then
  echo
  echo "*** Checking the build environment"
  echo
  if test -z "$BUILDCC"; then
    if test -n "$build_alias"; then
      AC_CHECK_PROGS([BUILDCC],
                     [${build_alias}-gcc ${build_alias}-cc ${build_alias}-cl])
    fi
  fi
  if test -z "$BUILDCC"; then
    AC_CHECK_PROGS([BUILDCC], [gcc cc cl])
  fi
else
  BUILDCC=$CC
  AC_SUBST([BUILDCC])
fi

echo
echo "*** Building libtool"
echo
LT_INIT([dlopen])
AC_SUBST([LIBTOOL_DEPS])

case "${host}" in
  *-pc-mingw*|*-pc-cygwin*)
  NO_UNDEFINED=" -no-undefined -enable-auto-import";
  NO_DLOPEN_TESTS=1 ;;
  *) NO_DLOPEN_TESTS=0 ;;
esac

LT_OUTPUT

dnl libltdl stuff

if test "x${use_modules}" != "xno"; then
  echo
  echo "*** Configuring the dynamic loader"
  echo
  saved_ldflags=$LDFLAGS
  saved_libs=$LIBS
  if test "x$ltdl_prefix" != "x"; then
    LDFLAGS="$LDFLAGS -L$ltdl_prefix/lib"
  fi
  AC_CHECK_LIB([ltdl],[lt_dlinit], [have_ltdl=yes], [have_ltdl=no])
  LDFLAGS=$saved_ldflags
  LIBS=$saved_libs

  if test "x$have_ltdl" == "xno"; then
    AC_MSG_ERROR([libltdl is required to build modules.  Please either specify the location of the libltdl library using --with-ldtl=PREFIX or else disable the building of modules by using --disable-modules.])
  fi

  if test "x$ltdl_prefix" != "x"; then
    AC_CHECK_HEADERS(ltdl.h)
    LIBLTDL="-L${ltdl_prefix}/lib -lltdl"
    LTDLINC="-I${ltdl_prefix}/include"
  else
    AC_CHECK_HEADERS(ltdl.h)
    LIBLTDL="-lltdl"
  fi
fi
AC_SUBST([LIBLTDL])
AC_SUBST([LTDLINCL])

dnl check if we found a C++ compiler
if test "x$CXX" == "x"; then
  make_cxxbindings=no
fi

dnl check if we found a Fortran 77 compiler
if test "x$F77" == "x"; then
  make_f77bindings=no
  make_f95bindings=no
fi

dnl check if we found a free-form Fortran compiler
if test "x$FC" == "x"; then
  make_f95bindings=no
fi

dnl compiler checks
echo
echo "*** Checking additional compiler characteristics"
echo
AC_C_INLINE
AC_C_RESTRICT
GD_C_RESTRICT_ARRAY
GD_PROG_CC_WALL
GD_PROG_CC_WEXTRA

if test "x$make_cxxbindings" != "xno"; then
  GD_PROG_CXX_WALL
  GD_PROG_CXX_WEXTRA
fi

if test "x$make_f77bindings" != "xno"; then
  GD_PROG_F77_WALL
  GD_PROG_F77_WEXTRA
  GD_PROG_F77_FNO_BACKSLASH
  GD_LANG_F77_COMPILER_INTEL
fi

dnl F77 stuff
if test "x$make_f77bindings" != "xno"; then
  AC_F77_WRAPPERS
  if test "x$ac_cv_f77_mangling" == "xunknown"; then
    make_f77bindings=no
    make_f95bindings=no
  fi
fi

if test "x$make_f95bindings" != "xno"; then
  GD_PROG_FC_WALL
  GD_PROG_FC_WEXTRA
  GD_LANG_FC_COMPILER_INTEL
fi

AC_MSG_CHECKING([if we're linking against the Microsoft C Run-Time]) #'
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([], [[#ifndef __MSVCRT__
       choke me
#endif
]])],
[gd_msvcrt=yes],
[gd_msvcrt=no])
AC_MSG_RESULT([$gd_msvcrt])

dnl 64-bit stat() requires MSVCRT 6.1 or later.  This may prohibit using
dnl GetData on Windows95.  Oh no!
if test "$gd_msvcrt" = "yes"; then
  AC_DEFINE([__MSVCRT_VERSION__], [0x601],
            [ If compiling aginst the MSVCRT, define to 0x601 ])
fi

dnl Declare a few feature test macros
AC_DEFINE([_GNU_SOURCE], [1], [ Expose GNU extensions ])
AC_DEFINE([_BSD_SOURCE], [1], [ Expose BSD-derived definitions ])
AC_DEFINE([_SVID_SOURCE], [1], [ Expose System V-derived definitions ])
AC_DEFINE([_POSIX_SOURCE], [1], [ Expose POSIX.1-1990 conforming definitions ])
AC_DEFINE([_POSIX_C_SOURCE], [200809L],
          [ Expose POSIX:2008 conforming definitions ])

dnl libraries
echo
echo "*** Looking for additional libraries"
echo
AC_SEARCH_LIBS([ceil],[m])

dnl pthread
if test "x${use_modules}" != "xno"; then
  AC_SEARCH_LIBS([pthread_mutex_lock],[pthread],
                 [use_pthread=yes
                  AC_DEFINE([USE_PTHREAD], [],
                          [Define if you have a POSIX compliant thread library])
                  ],
                  [use_pthread=no])
  AC_CHECK_HEADERS(pthread.h)
fi

dnl headers
echo
echo "*** Looking for additional header files"
echo
AC_CHECK_HEADERS([asm/unaligned.h Availability.h byteswap.h direct.h fcntl.h \
                  float.h inttypes.h io.h libgen.h libkern/OSByteOrder.h \
                  stddef.h stdint.h sys/endian.h sys/file.h sys/param.h \
                  sys/stat.h sys/types.h sys/wait.h unistd.h])
if test "x$disable_c99" = "xno"; then
AC_CHECK_HEADERS([complex.h])
fi
AC_HEADER_DIRENT

dnl types
echo
echo "*** Checking data types"
echo
AC_C_CONST
AC_C_BIGENDIAN
GD_C_FLOATORDERING
AC_TYPE_SIZE_T
AC_CHECK_SIZEOF([size_t])
AC_CHECK_TYPES([ssize_t])
AC_TYPE_OFF_T
AC_CHECK_SIZEOF([off_t])
AC_TYPE_UINT8_T
AC_TYPE_INT8_T
AC_TYPE_UINT16_T
AC_TYPE_INT16_T
AC_TYPE_UINT32_T
AC_TYPE_INT32_T
AC_TYPE_UINT64_T
AC_TYPE_INT64_T

if test "x$disable_c99" = "xno"; then
AC_CHECK_TYPES([_Complex float, _Complex double])
fi

GD_CHECK_LFS_TYPE([struct stat64],[
#ifdef STDC_HEADERS
#include <sys/types.h>
#include <sys/stat.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
])

GD_CHECK_LFS_TYPE([struct _stat64],[
#ifdef STDC_HEADERS
#include <sys/types.h>
#include <sys/stat.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
])

GD_CHECK_LFS_TYPE([struct __stat64],[
#ifdef STDC_HEADERS
#include <sys/types.h>
#include <sys/stat.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
])

GD_CHECK_LFS_TYPE([off64_t],[
#ifdef STDC_HEADERS
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
])

dnl integer sizes
AC_CHECK_SIZEOF([short int])
AC_CHECK_SIZEOF([int])
AC_CHECK_SIZEOF([long int])
AC_CHECK_SIZEOF([long long int])
AC_CHECK_SIZEOF([unsigned short int])
AC_CHECK_SIZEOF([unsigned int])
AC_CHECK_SIZEOF([unsigned long int])
AC_CHECK_SIZEOF([unsigned long long int])
AC_CHECK_SIZEOF([void *])

dnl C89 API types
if test $ac_cv_sizeof_int -eq 2; then
  gd_int16_t="int"
elif test $ac_cv_sizeof_short_int -eq 2; then
  gd_int16_t="short int"
elif test $ac_cv_sizeof_long_int -eq 2; then
  gd_int16_t="long int"
elif test $ac_cv_sizeof_long_long_int -eq 2; then
  gd_int16_t="long long int"
fi

if test $ac_cv_sizeof_unsigned_int -eq 2; then
  gd_uint16_t="unsigned int"
elif test $ac_cv_sizeof_unsigned_short_int -eq 2; then
  gd_uint16_t="unsigned short int"
elif test $ac_cv_sizeof_unsigned_long_int -eq 2; then
  gd_uint16_t="unsigned long int"
elif test $ac_cv_sizeof_unsigned_long_long_int -eq 2; then
  gd_uint16_t="unsigned long long int"
fi

if test $ac_cv_sizeof_int -eq 8; then
  gd_int64_t="int"
elif test $ac_cv_sizeof_short_int -eq 8; then
  gd_int64_t="short int"
elif test $ac_cv_sizeof_long_int -eq 8; then
  gd_int64_t="long int"
elif test $ac_cv_sizeof_long_long_int -eq 8; then
  gd_int64_t="long long int"
fi

if test $ac_cv_sizeof_unsigned_int -eq 8; then
  gd_uint64_t="int"
elif test $ac_cv_sizeof_unsigned_short_int -eq 8; then
  gd_uint64_t="short int"
elif test $ac_cv_sizeof_unsigned_long_int -eq 8; then
  gd_uint64_t="long int"
elif test $ac_cv_sizeof_unsigned_long_long_int -eq 8; then
  gd_uint64_t="long long int"
fi

if test "x$gd_cv_type_off64_t" = "xyes"; then
  dnl Assume we have the entire LFS defined transitional API
  AC_DEFINE([HAVE_OFF64_T], [1],
            [ Define to 1 if the system has the type `off64_t'. ]) dnl `
  define_lf64s="#ifndef _LARGEFILE64_SOURCE
#define _LARGEFILE64_SOURCE
#endif
"
  gd_off64_t="off64_t"
elif test $ac_cv_sizeof_off_t -eq 8;then
  gd_off64_t="off_t"
else
  gd_off64_t=$gd_int64_t
fi

DEFINE_gd_int16_t="# define gd_int16_t $gd_int16_t";
DEFINE_gd_uint16_t="# define gd_uint16_t $gd_uint16_t";
DEFINE_gd_int64_t="# define gd_int64_t $gd_int64_t";
DEFINE_gd_uint64_t="# define gd_uint64_t $gd_uint64_t";
DEFINE_gd_off64_t="$define_lf64s# define gd_off64_t $gd_off64_t";
AC_SUBST([DEFINE_gd_int16_t])
AC_SUBST([DEFINE_gd_uint16_t])
AC_SUBST([DEFINE_gd_int64_t])
AC_SUBST([DEFINE_gd_uint64_t])
AC_SUBST([DEFINE_gd_off64_t])

dnl library functions
echo
echo "*** Looking for additional library functions"
echo
AC_CHECK_FUNCS([basename _chsize _chsize_s _commit fchmod _fdopen fdopendir \
                fpathconf fseeko fseeko64 _fstat fstat64 _fstat64 fstatat \
                fstatat64 fsync ftello ftello64 ftruncate ftruncate64 getcwd \
                _getcwd getdelim gmtime_r isnan _isnan lseek64 _lseeki64 lstat \
                lstat64 _mkdir mkfifo nan _open openat pathconf _read \
                readdir_r readlink renameat _rmdir snprintf _snprintf stat64 \
                _stat64 _strtoi64 strtoll _strtoui64 strtoull _unlink unlinkat \
                _write])
if test "x$disable_c99" = "xno"; then
  AC_CHECK_FUNCS([cabs])
fi
AC_FUNC_STRERROR_R
AC_FUNC_FORK

AC_MSG_CHECKING([whether mkdir accepts two arguments])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#if HAVE_IO_H
#include <io.h>
#endif
], [[
#ifdef HAVE__MKDIR
_mkdir("x", 00777);
#else
mkdir("x", 00777);
#endif
]])],
[gd_2arg_mkdir=yes],
[gd_2arg_mkdir=no])
AC_MSG_RESULT([$gd_2arg_mkdir])

if test $gd_2arg_mkdir = "no"; then
  AC_DEFINE([MKDIR_NO_MODE], [1],
            [ Define to 1 if the `mkdir' function takes only one argument.])
fi dnl`

AC_CHECK_DECLS([bswap16, bswap_16, OSSwapInt16],,,
              [
#ifdef HAVE_BYTESWAP_H
#include <byteswap.h>
#endif
#ifdef HAVE_SYS_ENDIAN_H
#include <sys/endian.h>
#endif
#ifdef HAVE_LIBKERN_OSBYTEORDER_H
#include <libkern/OSByteOrder.h>
#endif
])

AC_CHECK_DECLS([get_unaligned, put_unaligned],,,
              [
#ifdef HAVE_ASM_UNALIGNED_H
#include <asm/unaligned.h>
#endif
])

dnl python
if test "x$make_pybindings" = "xyes"; then
  echo
  echo "*** Configuring python bindings"
  echo
  GD_PYTHON([2.3])
  have_numpy="no"
  if test "x$have_python" = "xno"; then
    make_pybindings="no"
  else
    AC_MSG_CHECKING([for NumPy])
    cat > conftest.py << EOF
import sys
try:
  import numpy
except ImportError:
  sys.exit(1)
EOF
    if $PYTHON conftest.py > /dev/null 2>&1; then
      have_numpy="yes"
    fi
    AC_MSG_RESULT([$have_numpy])
  fi
  if test "x$have_numpy" = "xyes"; then
    AC_MSG_CHECKING([NumPy includes])
    NUMPY_CPPFLAGS=-I`$PYTHON -c "import numpy; print numpy.get_include()"`
    AC_MSG_RESULT([$NUMPY_CPPFLAGS])
    AC_SUBST([NUMPY_CPPFLAGS])

    saved_cppflags=$CPPFLAGS
    CPPFLAGS="${CPPFLAGS} ${PYTHON_CPPFLAGS} ${NUMPY_CPPFLAGS}"
    AC_CHECK_HEADERS([numpy/arrayobject.h],,[have_numpy="no"],[
#include<Python.h>
])
    CPPFLAGS=$saved_cppflags
  fi

  if test "x$have_numpy" = "xyes"; then
    AC_DEFINE([USE_NUMPY], [],
              [ Define to enable NumPy support in the Python bindings ])
  fi
fi

dnl idl
if test "x$make_idlbindings" = "xyes"; then
  echo
  echo "*** Configuring interactive data language (IDL) bindings"
  echo
  GD_IDL([5.5])
  if test "x$have_idl" = "xno"; then
    make_idlbindings="no"
  else
    dnl build date
    AC_CHECK_PROGS([DATE], [date], [not found])
    if test "x$DATE" == "xnot found"; then
      BUILD_DATE="Build date unspecified"
    else
      BUILD_DATE=`$DATE`
    fi
    AC_SUBST([BUILD_DATE])
  fi
fi

dnl perl
if test "x$make_perlbindings" = "xyes"; then
  echo
  echo "*** Configuring Perl bindings"
  echo
  GD_PERL
  if test "x$have_perl" = "xno"; then
    make_perlbindings=no
  fi
fi

dnl external encodings
GD_CHECK_ENCODING([bzip2],[bz2],[BZ2_bzReadOpen],[bzlib.h],[bzip2],[bunzip2],[])
GD_CHECK_ENCODING([gzip],[z],[gzopen],[zlib.h],[gzip],[gunzip],[])
GD_CHECK_ENCODING([lzma],[lzma],[lzma_auto_decoder],[lzma.h],[xz],[],[])
GD_CHECK_ENCODING([slim],[slim],[slimopen],[slimlib.h], [slim slimdata],
                  [unslim],[slimdopen])
GD_CHECK_ENCODING([zzip],[zzip],[zzip_open],[zzip/lib.h],[zip],[unzip],[])

dnl zzslim hackery -- there's no easy way to check whether slim supports zzip
AC_ARG_ENABLE(zzslim, AS_HELP_STRING([--enable-zzslim],
              [skip probing for zzslim required features and enable the zzslim
               encoding.  The default is to autodetect support.  Note: zzslim
               encoding support requires both slim and zzip encoding support.]),
              [
               case "${enableval}" in
                 no) zzslim_override="no" ;;
                 *) zzslim_override="yes" ;;
               esac
              ], [ zzslim_override="auto" ])
m4_divert_once([HELP_ENABLE], AS_HELP_STRING([--disable-zzslim],
            [disable zzslim encodings support]))

echo
echo "*** Configuring zzslim support "
echo
if test $zzslim_override = "no"; then
  AC_MSG_CHECKING([for zzslim support])
  AC_MSG_RESULT([no (forced)])
  use_zzslim=no
else
  AC_MSG_CHECKING([for slim support])
  AC_MSG_RESULT([$use_slim])
  AC_MSG_CHECKING([for zzip support])
  AC_MSG_RESULT([$use_zzip])
  if test "$use_slim$use_zzip" == "yesyes"; then
    if test $zzslim_override = "yes"; then
      AC_MSG_CHECKING([for zzslim support])
      AC_MSG_RESULT([yes (forced)])
      use_zzslim=yes
    elif test "x$CXX" = "x"; then
      AC_MSG_CHECKING([for zzslim support])
      AC_MSG_RESULT([no (no C++ compiler available for configure test)])
      use_zzslim=no
    else
      dnl try linking to the slim library to see if it contains the unprototyped
      dnl   ibitstream::ibitstream(ZZIP_FILE *file, int buffersize)
      dnl given that this function isn't public, this is probably easily broken
      AC_MSG_CHECKING([whether -lslim contains zzip capability])
      AC_LANG_PUSH([C++])
      saved_ldflags=$LDFLAGS
      saved_libs=$LIBS
      LDFLAGS="$SLIM_LDFLAGS $LDFLAGS"
      LIBS="$SLIM_LIBS $LIBS"
      AC_LINK_IFELSE([AC_LANG_PROGRAM(
                      [[
                       struct zzip_file;
                       typedef struct zzip_file ZZIP_FILE;
                       class ibitstream {
                       public:
                       ibitstream(ZZIP_FILE *file, int buffersize);
                     };
                     ]],
                     [[
                      ZZIP_FILE *file;
                      ibitstream foo(file, 0);
                      ]]
                     )], [slim_zzip="yes"], [slim_zzip="no"])
      AC_MSG_RESULT([$slim_zzip])
      LDFLAGS=$saved_ldflags
      LIBS=$saved_libs
      AC_LANG_POP

      use_zzslim=$slim_zzip
    fi
  else
    use_zzslim="no"
  fi
fi

dnl add to summary and private lib list
if test "x$use_zzslim" != "xno"; then
  AC_DEFINE([USE_ZZSLIM], [], [ Define to enable zzslim support ])
  if test "x$use_modules" != "xno"; then
    ENCODINGS_MODS="${ENCODINGS_MODS} zzslim";
  else
    ENCODINGS_BUILT="${ENCODINGS_BUILT} zzslim";
  fi
else
  ENCODINGS_LEFT="${ENCODINGS_LEFT} zzslim";
fi

if test ! -z "${LIBS}"; then
  PRIVATE_LIBS="${LIBS} ${PRIVATE_LIBS}"
fi
AC_SUBST([PRIVATE_LIBS])

dnl Calculate absolute docdir
cat <<EOF > conftest.sh
prefix=$prefix
test "x$prefix" = xNONE && prefix=$ac_default_prefix
test "x$exec_prefix" = xNONE && exec_prefix=\$prefix
exec_prefix=$exec_prefix
datarootdir=$datarootdir
PACKAGE_TARNAME=$PACKAGE_TARNAME
echo $docdir
EOF
absolute_docdir=`$SHELL conftest.sh`
AC_SUBST([absolute_docdir])

dnl output
echo
echo "*** Writing configure output"
echo
AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([bindings/Makefile])
AC_CONFIG_FILES([bindings/cxx/Makefile])
AC_CONFIG_FILES([bindings/cxx/test/Makefile])
AC_CONFIG_FILES([bindings/f77/Makefile])
AC_CONFIG_FILES([bindings/f77/test/Makefile])
AC_CONFIG_FILES([bindings/f77/test/test_getdata.f])
AC_CONFIG_FILES([bindings/idl/Makefile])
AC_CONFIG_FILES([bindings/idl/makedlm.sh])
AC_CONFIG_FILES([bindings/idl/test/Makefile])
AC_CONFIG_FILES([bindings/perl/Makefile])
AC_CONFIG_FILES([bindings/perl/Build.PL])
AC_CONFIG_FILES([bindings/python/Makefile])
AC_CONFIG_FILES([bindings/python/test/Makefile])
AC_CONFIG_FILES([doc/Makefile])
AC_CONFIG_FILES([man/Makefile])
AC_CONFIG_FILES([man/gd_alter_encoding.3])
AC_CONFIG_FILES([man/gd_alter_endianness.3])
AC_CONFIG_FILES([man/gd_alter_frameoffset.3])
AC_CONFIG_FILES([src/Makefile])
dnl This doubling allows us to build getdata.h as a normal AC_CONFIG_FILE, but
dnl use AC_CONFIG_HEADERS to check whether it has changed.
AC_CONFIG_FILES([src/getdata.ah:src/getdata.h.in])
AC_CONFIG_HEADERS([src/getdata.h:src/getdata.ah], [chmod a-w src/getdata.h])
AC_CONFIG_FILES([src/getdata.pc])
AC_CONFIG_FILES([test/Makefile])
AC_CONFIG_FILES([util/Makefile])

dnl Automake conditionals
AM_CONDITIONAL(GETDATA_DEBUG, [test "x$enable_debug" == "xyes"])
AM_CONDITIONAL(CC_WALL, [test "x$gd_cv_prog_cc_wall" == "xyes"])
AM_CONDITIONAL(CXX_WALL, [test "x$gd_cv_prog_cxx_wall" == "xyes"])
AM_CONDITIONAL(F77_WALL, [test "x$gd_cv_prog_f77_wall" == "xyes"])
AM_CONDITIONAL(FC_WALL, [test "x$gd_cv_prog_fc_wall" == "xyes"])
AM_CONDITIONAL(CC_WEXTRA, [test "x$gd_cv_prog_cc_wextra" == "xyes"])
AM_CONDITIONAL(CXX_WEXTRA, [test "x$gd_cv_prog_cxx_wextra" == "xyes"])
AM_CONDITIONAL(F77_WEXTRA, [test "x$gd_cv_prog_f77_wextra" == "xyes"])
AM_CONDITIONAL(F77_FNO_BACKSLASH,
               [test "x$gd_cv_prog_f77_fno_backslash" == "xyes"])
AM_CONDITIONAL(FC_WEXTRA, [test "x$gd_cv_prog_fc_wextra" == "xyes"])
AM_CONDITIONAL(INTEL_F77_COMPILER,
               [test "x$gd_cv_f77_compiler_intel" == "xyes"])
AM_CONDITIONAL(INTEL_FC_COMPILER, [test "x$gd_cv_fc_compiler_intel" == "xyes"])
AM_CONDITIONAL(INCLUDE_LEGACY_API, [test "x$include_legacy_api" != "xno"])
AM_CONDITIONAL(MAKE_CXXBINDINGS, [test "x$make_cxxbindings" = "xyes"])
AM_CONDITIONAL(MAKE_F77BINDINGS, [test "x$make_f77bindings" != "xno"])
AM_CONDITIONAL(MAKE_F95BINDINGS, [test "x$make_f95bindings" = "xyes"])
AM_CONDITIONAL(MAKE_PYBINDINGS, [test "x$make_pybindings" = "xyes"])
AM_CONDITIONAL(MAKE_IDLBINDINGS, [test "x$make_idlbindings" = "xyes"])
AM_CONDITIONAL(MAKE_PERLBINDINGS, [test "x$make_perlbindings" = "xyes"])
AM_CONDITIONAL(USE_MODULES, [test "x$use_modules" != "xno"])
AM_CONDITIONAL(TEST_PYTHON, [test "x$NO_DLOPEN_TESTS" = "x0"])
AM_CONDITIONAL(TEST_IDL, [test "x$NO_DLOPEN_TESTS" = "x0"])
AM_CONDITIONAL(TEST_PERL, [test "x$HAVE_Test__Harness" = "xyes"])
AM_CONDITIONAL(HAVE_DIFF, [test "x$DIFF" != "x"])
AM_CONDITIONAL(USE_ZZSLIM, [test "x$use_zzslim" = "xyes"])
AM_CONDITIONAL(GDIDL_EXTERNAL, [false])


dnl we do this here to avoid screwing up other tests
LDFLAGS="${LDFLAGS}${NO_UNDEFINED}"

AC_OUTPUT

dnl Handy summary
if test "x$make_cxxbindings" != "xno"; then
  BINDINGS_BUILT="${BINDINGS_BUILT} C++"
else
  BINDINGS_LEFT="${BINDINGS_LEFT} C++"
fi

if test "x$make_f77bindings" != "xno"; then
  BINDINGS_BUILT="${BINDINGS_BUILT} Fortran-77"
else
  BINDINGS_LEFT="${BINDINGS_LEFT} Fortran-77"
fi

if test "x$make_f95bindings" != "xno"; then
  BINDINGS_BUILT="${BINDINGS_BUILT} Fortran-95"
else
  BINDINGS_LEFT="${BINDINGS_LEFT} Fortran-95"
fi

if test "x$make_idlbindings" != "xno"; then
  BINDINGS_BUILT="${BINDINGS_BUILT} IDL"
else
  BINDINGS_LEFT="${BINDINGS_LEFT} IDL"
fi

if test "x$make_perlbindings" != "xno"; then
  BINDINGS_BUILT="${BINDINGS_BUILT} Perl"
else
  BINDINGS_LEFT="${BINDINGS_LEFT} Perl"
fi

if test "x$make_pybindings" != "xno"; then
  BINDINGS_BUILT="${BINDINGS_BUILT} Python"
else
  BINDINGS_LEFT="${BINDINGS_LEFT} Python"
fi

if test "x$BINDINGS_BUILT" = "x"; then
  BINDINGS_BUILT=" none"
fi

if test "x$BINDINGS_LEFT" = "x"; then
  BINDINGS_LEFT=" none"
fi

if test "x$ENCODINGS_MODS" = "x"; then
  ENCODINGS_MODS=" none"
fi

if test "x$ENCODINGS_LEFT" = "x"; then
  ENCODINGS_LEFT=" none"
fi

if test "x$enable_debug$enable_assert" = "xnoyes"; then
  enable_debug="assert only"
fi

echo
echo "Configuration summary: "
echo "  ANSI C compatibility mode:     ${disable_c99}"
echo "  Legacy API:                    ${include_legacy_api}"
echo "  Verbose debugging:             ${enable_debug}"
echo "  Use modules:                   ${use_modules}"
if test "x${use_modules}" != "xno"; then
  echo "  Thread-safe dynamic loading:   ${use_pthread}"
fi
echo
echo "  Supported internal encodings: ${ENCODINGS_BUILT}"
if test "x${use_modules}" != "xno"; then
  echo "  Supported external encodings: ${ENCODINGS_MODS}"
fi
echo "  Unsupported encodings:        ${ENCODINGS_LEFT}"
echo
echo "  Bindings built:               ${BINDINGS_BUILT}"
echo "  Bindings not built:           ${BINDINGS_LEFT}"
if test "x$make_pybindings" != "xno"; then
  echo "  NumPy support in Python:       ${have_numpy}"
fi
echo
