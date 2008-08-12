AC_INIT([getdata], [0.2.0], [dwiebe@physics.utoronto.ca])
AM_INIT_AUTOMAKE([getdata], [0.2.0])

dnl libgetdata current interface version
AC_SUBST(GETDATA_IFACE_VERSION, [1])
dnl libgetdata current interface implementation revision
AC_SUBST(GETDATA_IMPL_REVISION, [0])
dnl libgetdata intefrace age (current interface - oldest supported interface)
AC_SUBST(GETDATA_IFACE_AGE, [1])

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
  AC_SUBST(INCLUDE_LEGACY, ["/*include the legacy API */
#include <getdata_legacy.h>"])
else
  AC_SUBST(GETDATA_IFACE_AGE, [0])
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
AC_SYS_LARGEFILE

dnl types
AC_C_BIGENDIAN
AC_TYPE_UINT8_T
AC_TYPE_INT8_T
AC_TYPE_UINT16_T
AC_TYPE_INT16_T
AC_TYPE_UINT32_T
AC_TYPE_INT32_T
AC_TYPE_UINT64_T
AC_TYPE_INT64_T

dnl output
AC_CONFIG_FILES([Makefile man/Makefile src/Makefile src/getdata.h])
AC_OUTPUT
