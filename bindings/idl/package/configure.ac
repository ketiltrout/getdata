dnl Copyright (C) 2008-2011 D. V. Wiebe
dnl
dnl llllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllll
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

AC_INIT([IDL_GetData],[getdata_version],[getdata-devel@lists.sourceforge.net])

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

AC_CONFIG_AUX_DIR([.])
AC_CONFIG_SRCDIR([src/getdata.c])
AC_CONFIG_HEADER([src/config.h])

dnl Features
echo "*** Checking GetData configuration"
echo

dnl Build/Host triplets
AC_CANONICAL_HOST

dnl DL library path munging for test suite
case "${host}" in
  *-apple-darwin*) DL_LIBRARY_PATH="DYLD_LIBRARY_PATH" ;;
  *) DL_LIBRARY_PATH="LD_LIBRARY_PATH" ;;
esac
AC_SUBST([DL_LIBRARY_PATH])

dnl Programs
echo
echo "*** Initialising build system"
echo
AM_INIT_AUTOMAKE
AC_PROG_LN_S
AC_PROG_GREP
AC_PROG_AWK
AC_CHECK_PROGS([DIFF], [diff], [])
AC_PROG_SED
echo
echo "*** Checking C compiler characteristics"
echo
AC_PROG_CC
AC_PROG_CC_C_O
AC_PROG_CC_C99

AC_HEADER_STDC

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
  NO_UNDEFINED=" -no-undefined --enable-auto-import";
  NO_DLOPEN_TESTS=1 ;;
  *) NO_DLOPEN_TESTS=0 ;;
esac

LT_OUTPUT

dnl compiler checks
echo
echo "*** Checking additional compiler characteristics"
echo
AC_C_INLINE
GD_PROG_CC_WALL

dnl Declare a few feature test macros
AC_DEFINE([GDIDL_EXTERNAL], [1], [ Define to 1 if building the GetData IDL bindings outside the GetData source tree ])
AC_DEFINE([_BSD_SOURCE], [], [ Expose BSD-derived definitions ])
AC_DEFINE([_SVID_SOURCE], [], [ Expose System V-derived definitions ])
AC_DEFINE([_POSIX_SOURCE], [], [ Expose POSIX.1-1990 conforming definitions ])
AC_DEFINE([_POSIX_C_SOURCE], [200809L],
          [ Expose POSIX:2008 conforming definitions ])

dnl libraries
echo
echo "*** Looking for getdata"
echo
PKG_CHECK_MODULES([GETDATA], [getdata >= 0.7.0])

dnl headers
echo
echo "*** Looking for additional header files"
echo
AC_CHECK_HEADERS([complex.h])

dnl types
echo
echo "*** Checking data types"
echo
AC_C_CONST
AC_TYPE_UINT8_T
AC_TYPE_INT8_T
AC_TYPE_UINT16_T
AC_TYPE_INT16_T
AC_TYPE_UINT32_T
AC_TYPE_INT32_T
AC_TYPE_UINT64_T
AC_TYPE_INT64_T
AC_CHECK_TYPES([_Complex float, _Complex double])

GD_CHECK_LFS_TYPE([off64_t],[
#ifdef STDC_HEADERS
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
])

if test "x$gd_cv_c_need_lfs_defined" = "xyes"; then
  AC_DEFINE([_LARGEFILE64_SOURCE], [1],
            [ Additional functionality from LFS for large files. ])
fi

if test "x$gd_cv_type_off64_t" = "xyes"; then
  dnl Assume we have the entire LFS defined transitional API
  AC_DEFINE([HAVE_OFF64_T], [1],
            [ Define to 1 if the system has the type `off64_t'. ]) dnl `
fi

echo
echo "*** Configuring interactive data language (IDL) bindings"
echo
GD_IDL([5.5])
if test "x$have_idl" = "xno"; then
  AC_MSG_ERROR([Cannot find an IDL interpreter!  Maybe you need to use --with-idl?])
fi

dnl build date
AC_CHECK_PROGS([DATE], [date], [not found])
if test "x$DATE" == "xnot found"; then
  BUILD_DATE="Build date unspecified"
else
  BUILD_DATE=`date`
fi
AC_SUBST([BUILD_DATE])

dnl output
echo
echo "*** Writing configure output"
echo
AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([src/Makefile])
AC_CONFIG_FILES([src/makedlm.sh])
AC_CONFIG_FILES([src/test/Makefile])

dnl Automake conditionals
AM_CONDITIONAL(GETDATA_DEBUG, [false])
AM_CONDITIONAL(CC_WALL, [test "x$gd_cv_prog_cc_wall" == "xyes"])
AM_CONDITIONAL(INTEL_F77_COMPILER,
               [test "x$gd_cv_f77_compiler_intel" == "xyes"])
AM_CONDITIONAL(INTEL_FC_COMPILER, [test "x$gd_cv_fc_compiler_intel" == "xyes"])
AM_CONDITIONAL(LFS_TRANSITIONAL_API, [test "x$gd_cv_type_off64_t" = "xyes"])
AM_CONDITIONAL(TEST_IDL, [test "x$NO_DLOPEN_TESTS" = "x0"])
AM_CONDITIONAL(HAVE_DIFF, [test "x$DIFF" != "x"])
AM_CONDITIONAL(GDIDL_EXTERNAL, [true])

dnl we do this here to avoid screwing up other tests
LDFLAGS="${LDFLAGS}${NO_UNDEFINED}"

AC_OUTPUT
