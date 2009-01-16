dnl (C) 2008-2009 D. V. Wiebe
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
dnl You should have received a copy of the GNU General Public License
dnl along with GetData; if not, write to the Free SoftwareFoundation, Inc.,
dnl 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
dnl
AC_INIT([GetData], [0.5.0], [getdata-devel@lists.sourceforge.net])

AC_PREREQ([2.63])

dnl libgetdata current interface version
AC_SUBST(GETDATA_IFACE_VERSION, [1])
dnl libgetdata current interface implementation revision
AC_SUBST(GETDATA_IMPL_REVISION, [0])
dnl libgetdata interface age (current interface - oldest supported interface)
AC_SUBST(GETDATA_IFACE_AGE, [0])

dnl libgetdata++ interface version info
AC_SUBST(GETDATAXX_VERSION, [0:0:0])

dnl libfgetdata interface version info
AC_SUBST(FGETDATA_VERSION, [0:0:0])

dnl libf95getdata interface version info
AC_SUBST(F95GETDATA_VERSION, [0:0:0])

AC_CONFIG_SRCDIR([src/getdata.c])
AC_CONFIG_HEADER([src/config.h])

dnl Features
BUINDINGS_BUILT=
BINDINGS_LEFT=
ENCODINGS_BUILT=" raw ascii"
ENCODINGS_LEFT=

echo "*** Checking GetData configuration"
echo
dnl Legacy API
AC_ARG_ENABLE(legacy-api, AS_HELP_STRING([--disable-legacy-api],
              [don't include the legacy API wrapper in the library]),
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
fi
AM_CONDITIONAL(GETDATA_DEBUG, [test "x$enable_debug" == "xyes"])

AC_MSG_CHECKING([whether to enable assertions])
AC_MSG_RESULT([$enable_assert])
if test "x$enable_assert" = "xno"; then
  AC_DEFINE(NDEBUG, 1, [ Define to disable assertions ])
fi

dnl checkdirfile
AC_ARG_ENABLE(checkdirfile, AS_HELP_STRING([--disable-checkdirfile],
              [don't build or install the checkdirfile utility]),
              [
               case "${enableval}" in
                 no) make_checkdirfile="don't build" ;;
                 *) make_checkdirfile="build" ;;
               esac
              ], [ make_checkdirfile="build" ])
AC_MSG_CHECKING([whether to compile and install checkdirfile])
AC_MSG_RESULT([$make_checkdirfile])
AM_CONDITIONAL(MAKE_CHECKDIRFILE, test "x$make_checkdirfile" = "xbuild")

dnl C++ bindings
AC_ARG_ENABLE(cplusplus, AS_HELP_STRING([--disable-cplusplus],
              [don't build the C++ bindings (libgetdata++)]),
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
AC_ARG_ENABLE(fortran, AS_HELP_STRING([--disable-fortran],
              [don't build the Fortran 77 bindings (libfgetdata) nor the
               Fortran 95 bindings (libf95getdata)]),
              [
               case "${enableval}" in
                 no) make_f77bindings="no" ;;
                 *) make_f77bindings="yes" ;;
               esac
              ], [ make_f77bindings="yes" ])

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
AM_CONDITIONAL(MAKE_F95BINDINGS, test "x$make_f95bindings" = "xyes")

if test "x$make_f77bindings$make_f95bindings" = "xnoyes"; then
  make_f77bindings="yes (required by Fortran 95 bindings)"
fi

AC_MSG_CHECKING([whether to include the Fortran 77 bindings])
AC_MSG_RESULT([$make_f77bindings])
AM_CONDITIONAL(MAKE_F77BINDINGS, test "x$make_f77bindings" != "xno")

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
AM_CONDITIONAL(USE_MODULES, test "x$use_modules" != "xno")
if test "x$use_modules" != "xno"; then
AC_DEFINE([USE_MODULES], [], [ Define if building with modules ])
fi

dnl LTDL
AC_ARG_WITH([system-ltdl], AS_HELP_STRING([--with-system-ltdl=PREFIX],
              [if building modules, use the GNU ltdl library located in PREFIX [[autodetect]]]),
              [
               case "${withval}" in
                 no) use_system_ltdl="no" ;; 
                 *) use_system_ltdl="yes" ; ltdl_prefix="${withval}" ;;
               esac
              ], [ use_system_ltdl="yes" ; ltdl_prefix= ])
m4_divert_once([HELP_WITH], AS_HELP_STRING([--without-system-ltdl],
               [if building modules, always use the internal ltdl library, even if an external one is present]))

dnl Programs
echo
echo "*** Initialising build system"
echo
AM_INIT_AUTOMAKE
AC_PROG_LN_S
echo
echo "*** Checking C compiler characteristics"
echo
AC_PROG_CC
AC_PROG_CC_C_O
AC_PROG_CC_C99
AC_HEADER_STDC
echo
echo "*** Checking C++ compiler characteristics"
echo
AC_PROG_CXX
AC_PROG_CXX_C_O
echo
echo "*** Checking Fortran 77 compiler characteristics"
echo
AC_PROG_F77
AC_PROG_F77_C_O
echo
echo "*** Checking Fortran 90 compiler characteristics"
echo
AC_PROG_FC
AC_PROG_FC_C_O
echo
echo "*** Building libtool"
echo
AC_ENABLE_STATIC
AC_ENABLE_SHARED
AC_LIBTOOL_DLOPEN
AC_PROG_LIBTOOL

dnl libltdl stuff

if test "x${use_modules}" != "xno"; then
  echo
  echo "*** Configuring the dynamic loader"
  echo
  if test "x${use_system_ltdl}" = "xyes"; then
    saved_ldflags=$LDFLAGS
    saved_libs=$LIBS
    if test "x$ltdl_prefix" != "x"; then
      LDFLAGS="$LDFLAGS -L$ltdl_prefix/lib"
    fi
    AC_CHECK_LIB([ltdl],[lt_dlinit], [internal_ltdl=no], [internal_ltdl=yes])
    LDFLAGS=$saved_ldflags
    LIBS=$saved_libs
  else
    internal_ltdl=yes
  fi

  if test "x$internal_ltdl" == "xyes"; then
    AC_DEFINE([INTERNAL_LTDL], 1,
              [Define to 1 if using the internal ltdl library])
    use_system_ltdl=no
    AC_LTDL_SHLIBEXT
    AC_LTDL_SHLIBPATH
    AC_LTDL_SYSSEARCHPATH
    AC_LTDL_OBJDIR
    AC_LTDL_DLPREOPEN
    AC_LTDL_DLLIB
    AC_LTDL_SYMBOL_USCORE
    AC_LTDL_DLSYM_USCORE
    AC_LTDL_SYS_DLOPEN_DEPLIBS
    AC_LTDL_FUNC_ARGZ
    AC_CHECK_HEADERS([assert.h ctype.h errno.h malloc.h memory.h stdlib.h \
                            stdio.h unistd.h])
    AC_CHECK_HEADERS([dl.h sys/dl.h dld.h mach-o/dyld.h])
    AC_CHECK_HEADERS([string.h strings.h], [break])
    AC_CHECK_FUNCS([strchr index], [break])
    AC_CHECK_FUNCS([strrchr rindex], [break])
    AC_CHECK_FUNCS([memcpy bcopy], [break])
    AC_CHECK_FUNCS([memmove strcmp])
    AC_CHECK_FUNCS([closedir opendir readdir])

    GD_HIDE_LTDL_SYM(lt_dladderror)
    GD_HIDE_LTDL_SYM(lt_dladdsearchdir)
    GD_HIDE_LTDL_SYM(lt_dlcaller_get_data)
    GD_HIDE_LTDL_SYM(lt_dlcaller_register)
    GD_HIDE_LTDL_SYM(lt_dlcaller_set_data)
    GD_HIDE_LTDL_SYM(lt_dlclose)
    GD_HIDE_LTDL_SYM(lt_dlerror)
    GD_HIDE_LTDL_SYM(lt_dlexit)
    GD_HIDE_LTDL_SYM(lt_dlforeach)
    GD_HIDE_LTDL_SYM(lt_dlforeachfile)
    GD_HIDE_LTDL_SYM(lt_dlfree)
    GD_HIDE_LTDL_SYM(lt_dlgetinfo)
    GD_HIDE_LTDL_SYM(lt_dlgetsearchpath)
    GD_HIDE_LTDL_SYM(lt_dlhandle_next)
    GD_HIDE_LTDL_SYM(lt_dlinit)
    GD_HIDE_LTDL_SYM(lt_dlinsertsearchdir)
    GD_HIDE_LTDL_SYM(lt_dlisresident)
    GD_HIDE_LTDL_SYM(lt_dlloader_add)
    GD_HIDE_LTDL_SYM(lt_dlloader_data)
    GD_HIDE_LTDL_SYM(lt_dlloader_find)
    GD_HIDE_LTDL_SYM(lt_dlloader_name)
    GD_HIDE_LTDL_SYM(lt_dlloader_next)
    GD_HIDE_LTDL_SYM(lt_dlloader_remove)
    GD_HIDE_LTDL_SYM(lt_dlmakeresident)
    GD_HIDE_LTDL_SYM(lt_dlmalloc)
    GD_HIDE_LTDL_SYM(lt_dlmutex_register)
    GD_HIDE_LTDL_SYM(lt_dlopen)
    GD_HIDE_LTDL_SYM(lt_dlopenext)
    GD_HIDE_LTDL_SYM(lt_dlpreload)
    GD_HIDE_LTDL_SYM(lt_dlpreload_default)
    GD_HIDE_LTDL_SYM(lt_dlrealloc)
    GD_HIDE_LTDL_SYM(lt_dlseterror)
    GD_HIDE_LTDL_SYM(lt_dlsetsearchpath)
    GD_HIDE_LTDL_SYM(lt_dlsym)
    LIBLTDL="$LIBADD_DL"
  elif test "x$ltdl_prefix" != "x"; then
    AC_CHECK_HEADERS(ltdl.h)
    LIBLTDL="-L${ltdl_prefix}/lib -lltdl"
    LTDLINC="-I${ltdl_prefix}/include"
  else
    AC_CHECK_HEADERS(ltdl.h)
    LIBLTDL="-lltdl"
  fi
else
  internal_ltdl=no
fi
AC_SUBST([LIBLTDL])
AC_SUBST([LTDLINCL])
AM_CONDITIONAL(INTERNAL_LTDL, test "x${internal_ltdl}" = xyes)

dnl ICC checks
echo
echo "*** Checking additional compiler characteristics"
echo
GD_LANG_COMPILER_INTEL
AM_CONDITIONAL(GNU_C_COMPILER, test "x$gd_cv_c_compiler_intel" == "xno" -a "x$ac_cv_c_compiler_gnu" == "xyes")
AC_LANG_PUSH([C++])
GD_LANG_COMPILER_INTEL
AM_CONDITIONAL(GNU_CXX_COMPILER, test "x$gd_cv_cxx_compiler_intel" == "xno" -a "x$ac_cv_cxx_compiler_gnu" == "xyes")
AC_LANG([Fortran 77])
GD_LANG_COMPILER_INTEL
AM_CONDITIONAL(GNU_F77_COMPILER, test "x$gd_cv_f77_compiler_intel" == "xno" -a "x$ac_cv_f77_compiler_gnu" == "xyes")
AC_LANG([Fortran])
GD_LANG_COMPILER_INTEL
AM_CONDITIONAL(GNU_FC_COMPILER, test "x$gd_cv_fc_compiler_intel" == "xno" -a "x$ac_cv_fc_compiler_gnu" == "xyes")
AC_LANG_POP([Fortran])

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

dnl F77 stuff
if test "x$make_f77bindings" != "xno"; then
  AC_PROG_SED
  AC_CHECK_PROGS([DIFF], [diff], [not found])
  if test "x$DIFF" == "xnot found"; then
    AC_MSG_ERROR([`diff' is needed to build the Fortran bindings]) dnl`
  fi
  AC_F77_WRAPPERS
fi

dnl libraries
echo
echo "*** Looking for library functions"
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

dnl functions
AC_CHECK_FUNCS([atoll atoq])

dnl headers
echo
echo "*** Looking for additional header files"
echo
AC_CHECK_HEADERS(fcntl.h libgen.h)
AC_HEADER_DIRENT

dnl Declare a few feature test macros
AC_DEFINE([_BSD_SOURCE], [], [ Expose BSD-derived definitions ])
AC_DEFINE([_SVID_SOURCE], [], [ Expose System V-derived definitions ])
AC_DEFINE([_POSIX_SOURCE], [], [ Expose POSIX.1-1990 conforming definitions ])
AC_DEFINE([_POSIX_C_SOURCE], [200112L],
          [ Expose POSIX.1-2001 conforming definitions ])

dnl types
echo
echo "*** Checking data types"
echo
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
  AC_DEFINE([HAVE_OFF64_T], [1],
            [ Define to 1 if the system has the type `off64_t'. ]) dnl `
fi

AC_CHECK_SIZEOF([int])

dnl external encodings
GD_CHECK_ENCODING([bzip2],[bz2],[BZ2_bzReadOpen],[bzlib.h],[bzip2],[bunzip2])
GD_CHECK_ENCODING([slim],[slim],[slimopen],[slimlib.h],[slim],[unslim])
GD_CHECK_ENCODING([gzip],[z],[gzopen],[zlib.h],[gzip],[gunzip])

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
AC_CONFIG_FILES([doc/Makefile])
AC_CONFIG_FILES([man/Makefile])
AC_CONFIG_FILES([man/dirfile_alter_encoding.3])
AC_CONFIG_FILES([man/dirfile_alter_endianness.3])
AC_CONFIG_FILES([man/dirfile_alter_frameoffset.3])
AC_CONFIG_FILES([src/Makefile])
AC_CONFIG_HEADERS([src/getdata.h], [chmod a-w src/getdata.h])
AC_CONFIG_FILES([src/getdata.pc])
AC_CONFIG_FILES([test/Makefile])

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
  enable_debug = "assert only"
fi

echo
echo "Configuration summary: "
echo "  checkdirfile utility:         ${make_checkdirfile}"
echo "  Include legacy API:           ${include_legacy_api}"
echo "  Verbose debugging:            ${enable_debug}"
echo "  Use modules:                  ${use_modules}"
if test "x${use_modules}" != "xno"; then
  echo "  Use system libltdl:           ${use_system_ltdl}"
  echo "  Thread-safe dynamic loading:  ${use_pthread}"
fi
echo
echo "  Supported internal encodings:${ENCODINGS_BUILT}"
if test "x${use_modules}" != "xno"; then
echo "  Supported external encodings:${ENCODINGS_MODS}"
fi
echo "  Unsupported encodings:       ${ENCODINGS_LEFT}"
echo
echo "  Bindings built:              ${BINDINGS_BUILT}"
echo "  Bindings not built:          ${BINDINGS_LEFT}"
echo
