dnl (C) 2008 D. V. Wiebe
dnl
dnllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllll
dnl
dnl This file is part of the GetData project.
dnl
dnl This program is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 2 of the License, or
dnl (at your option) any later version.
dnl
dnl The GNU C Library is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl Lesser General Public License for more details.
dnl
dnl You should have received a copy of the GNU Lesser General Public
dnl License along with the GNU C Library; if not, write to the Free
dnl Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
dnl 02111-1307 USA.
dnl
AC_INIT([getdata], [0.3.0], [dwiebe@physics.utoronto.ca])
AM_INIT_AUTOMAKE([getdata], [0.3.0])

dnl libgetdata current interface version
AC_SUBST(GETDATA_IFACE_VERSION, [1])
dnl libgetdata current interface implementation revision
AC_SUBST(GETDATA_IMPL_REVISION, [0])
dnl libgetdata intefrace age (current interface - oldest supported interface)
dnl   this must be 0 and not 1 because, although we still support the old
dnl   interface, the error codes have changed.
AC_SUBST(GETDATA_IFACE_AGE, [0])

AC_CONFIG_SRCDIR([src/getdata.c])
AC_CONFIG_HEADER([src/config.h])

dnl Features
AC_ARG_ENABLE(legacy-api, AS_HELP_STRING([--disable-legacy-api],
              [Don't include the legacy API wrapper in the library]),
              [
               case "${enableval}" in
                 no) include_legacy_api="no" ;;
                 *) include_legacy_api="yes" ;;
               esac
              ], [ include_legacy_api="yes" ])
AC_MSG_CHECKING([whether to include the legacy API])
AC_MSG_RESULT([$include_legacy_api])
AM_CONDITIONAL(INCLUDE_LEGACY_API, test "x$include_legacy_api" != "xno")

if test "x$include_legacy_api" != "xno"; then
  AC_SUBST(INCLUDE_LEGACY, ["/* include the legacy API */
#include <getdata_legacy.h>"])
fi

AC_ARG_ENABLE(debug, AS_HELP_STRING([--enable-debug],
              [Enable debugging messages]),
              [
               case "${enableval}" in
                 yes) enable_debug="yes" ;;
                 *) enable_debug="no";
               esac
              ], [ enable_debug="no" ])

AC_MSG_CHECKING([whether to enable debugging messages])
AC_MSG_RESULT([$enable_debug])
if test "x$enable_debug" == "xyes"; then
  AC_DEFINE([GETDATA_DEBUG], [],
            [ Define to enable debugging messages ])
fi

dnl Programs
AC_PROG_CC
dnl Using C99 where possible defines NAN
AC_PROG_CC_C99
AC_HEADER_STDC
AC_PROG_CPP
AC_PROG_INSTALL
AC_ENABLE_STATIC
AC_ENABLE_SHARED
AC_PROG_LIBTOOL

dnl libraries
AC_CHECK_LIB(m,ceil,,AC_MSG_ERROR([Can't find libm!]))

dnl headers
AC_CHECK_HEADERS(fcntl.h libgen.h)
AC_HEADER_ASSERT
AC_HEADER_DIRENT

dnl types
AC_C_CONST
AC_C_BIGENDIAN
AC_TYPE_SIZE_T
AC_TYPE_OFF_T
AC_TYPE_UINT8_T
AC_TYPE_INT8_T
AC_TYPE_UINT16_T
AC_TYPE_INT16_T
AC_TYPE_UINT32_T
AC_TYPE_INT32_T
AC_TYPE_UINT64_T
AC_TYPE_INT64_T

dnl We define this to allow us to use off64_t in addition to the usual off_t
AC_DEFINE([_LARGEFILE64_SOURCE], [1],
          [ Additional functionality from LFS for large files. ])
dnl We use off64_t to probe whether to include the LFS transitional API.
AC_CHECK_TYPES([off64_t], [have_off64_t="yes"], [have_off64_t="no"])
AM_CONDITIONAL(HAVE_OFF64_T, test "x$have_off64_t" = "xyes")

if test "x$have_off64_t" == "xyes"; then
  dnl Assume we have the entire LFS defined transitional API
  AC_DEFINE([HAVE_OFF64_T], [1], [ Define to 1 if the system has the type `off64_t'. ]) dnl `
fi

dnl output
AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([man/Makefile])
AC_CONFIG_FILES([src/Makefile])
dnl (because I keep modifing getdata.h instead of getdata.h.in)
AC_CONFIG_FILES([src/getdata.h], [chmod a-w src/getdata.h])
AC_CONFIG_FILES([test/Makefile])
AC_OUTPUT
