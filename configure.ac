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
dnl GetData is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public
dnl License along with GetData; if not, write to the Free Software
dnl Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
dnl 02111-1307 USA.
dnl
AC_INIT([getdata], [0.3.0], [dwiebe@physics.utoronto.ca])
AM_INIT_AUTOMAKE([getdata], [0.3.0])

dnl autoconf 2.60 or later is required to build configure
AC_PREREQ([2.60])

dnl libgetdata current interface version
AC_SUBST(GETDATA_IFACE_VERSION, [1])
dnl libgetdata current interface implementation revision
AC_SUBST(GETDATA_IMPL_REVISION, [0])
dnl libgetdata interface age (current interface - oldest supported interface)
dnl   this must be 0 and not 1 because, although we still support the old
dnl   interface, the error codes have changed.
AC_SUBST(GETDATA_IFACE_AGE, [0])

dnl libdirfile current interface version
AC_SUBST(DIRFILE_IFACE_VERSION, [0])
dnl libdirfile current interface implementation revision
AC_SUBST(DIRFILE_IMPL_REVISION, [0])
dnl libdirfile interface age (current interface - oldest supported interface)
AC_SUBST(DIRFILE_IFACE_AGE, [0])

dnl libfgetdata current interface version
AC_SUBST(FGETDATA_IFACE_VERSION, [0])
dnl libfgetdata current interface implementation revision
AC_SUBST(FGETDATA_IMPL_REVISION, [0])
dnl libfgetdata interface age (current interface - oldest supported interface)
AC_SUBST(FGETDATA_IFACE_AGE, [0])


AC_CONFIG_SRCDIR([src/getdata.c])
AC_CONFIG_HEADER([src/config.h])

dnl Features

dnl Legacy API
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
  AC_DEFINE([GETDATA_LEGACY_API], [], [ This symbol is defined whenever the library contains the legacy API ])
fi

dnl debugging
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
if test "x$enable_debug" = "xyes"; then
  AC_DEFINE([GETDATA_DEBUG], [],
            [ Define to enable debugging messages ])
fi
AM_CONDITIONAL(GETDATA_DEBUG, test "x$enable_debug" == "xyes")

dnl checkdirfile
AC_ARG_ENABLE(checkdirfile, AS_HELP_STRING([--disable-checkdirfile],
              [Don't build or install the checkdirfile utility]),
              [
               case "${enableval}" in
                 no) make_checkdirfile="no" ;;
                 *) make_checkdirfile="yes" ;;
               esac
              ], [ make_checkdirfile="yes" ])
AC_MSG_CHECKING([whether to compile and install checkdirfile])
AC_MSG_RESULT([$make_checkdirfile])
AM_CONDITIONAL(MAKE_CHECKDIRFILE, test "x$make_checkdirfile" = "xyes")

dnl C++ bindings
AC_ARG_ENABLE(cxx-bindings, AS_HELP_STRING([--disable-cxx-bindings],
              [Don't build the C++ bindings (libdirfile)]),
              [
               case "${enableval}" in
                 no) make_cxxbindings="no" ;;
                 *) make_cxxbindings="yes" ;;
               esac
              ], [ make_cxxbindings="yes" ])
AC_MSG_CHECKING([whether to include the C++ bindings])
AC_MSG_RESULT([$make_cxxbindings])
AM_CONDITIONAL(MAKE_CXXBINDINGS, test "x$make_cxxbindings" = "xyes")

dnl Fortran 77 bindings
AC_ARG_ENABLE(f77-bindings, AS_HELP_STRING([--disable-f77-bindings],
              [Don't build the Fortran 77 bindings (libfgetdata)]),
              [
               case "${enableval}" in
                 no) make_f77bindings="no" ;;
                 *) make_f77bindings="yes" ;;
               esac
              ], [ make_f77bindings="yes" ])
AC_MSG_CHECKING([whether to include the Fortran 77 bindings])
AC_MSG_RESULT([$make_f77bindings])
AM_CONDITIONAL(MAKE_F77BINDINGS, test "x$make_f77bindings" = "xyes")

dnl Programs
AC_PROG_CC
dnl Using C99 where possible defines NAN
AC_PROG_CC_C99
AC_PROG_CXX
AC_PROG_FC
AC_HEADER_STDC
AC_PROG_INSTALL
AC_ENABLE_STATIC
AC_ENABLE_SHARED
AC_PROG_LIBTOOL

dnl F77 stuff
if test "x$make_f77bindings" = "xyes"; then
  AC_F77_WRAPPERS
fi

dnl libraries
AC_CHECK_LIB(m,ceil)

dnl headers
AC_CHECK_HEADERS(fcntl.h libgen.h)
AC_HEADER_ASSERT
AC_HEADER_DIRENT

dnl declarations
AC_CHECK_DECLS([strdup, strerror_r])

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

if test "x$have_off64_t" = "xyes"; then
  dnl Assume we have the entire LFS defined transitional API
  AC_DEFINE([HAVE_OFF64_T], [1], [ Define to 1 if the system has the type `off64_t'. ]) dnl `
fi

dnl output
AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([bin/Makefile])
AC_CONFIG_FILES([bindings/Makefile])
if test "x$make_cxxbindings" = "xyes"; then
  AC_CONFIG_FILES([bindings/cxx/Makefile])
  AC_CONFIG_FILES([bindings/cxx/test/Makefile])
fi
if test "x$make_f77bindings" = "xyes"; then
  AC_CONFIG_FILES([bindings/f77/Makefile])
  AC_CONFIG_FILES([bindings/f77/test/Makefile])
fi
AC_CONFIG_FILES([man/Makefile])
AC_CONFIG_FILES([src/Makefile])
dnl (because I keep modifying getdata.h instead of getdata.h.in)
AC_CONFIG_HEADERS([src/getdata.h], [chmod a-w src/getdata.h])
AC_CONFIG_FILES([test/Makefile])
AC_OUTPUT
