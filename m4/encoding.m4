dnl Copyright (C) 2008-2013 D. V. Wiebe
dnl
dnl llllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllll
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

dnl GD_CHECK_ENCODING
dnl -------------------------------------------------------------
dnl Run a bunch of checks to see if we can build and test encodings
dnl based on an external library.  This function is easily broken.
AC_DEFUN([GD_CHECK_ENCODING],
[
have_this_header=
have_this_lib=
m4_define([gd_encoding], [$1])
AC_ARG_WITH([lib$2], AS_HELP_STRING([--with-lib$2=PREFIX],
            [use the lib$2 installed in PREFIX [default: autodetect]]),
            [
             case "${withval}" in
               no) use_[]gd_encoding="no" ;;
               yes) use_[]gd_encoding="yes"; gd_encoding[]_prefix= ;;
               *) use_[]gd_encoding="yes"; gd_encoding[]_prefix="${withval}" ;;
             esac
             ], [ use_[]gd_encoding="yes"; gd_encoding[]_prefix=; ])
m4_divert_once([HELP_WITH], AS_HELP_STRING([--without-lib$2],
            [disable encodings supported by lib$2, even if the library is ]
            [present]))

echo
echo "*** Configuring gd_encoding support"
echo

if test "x$no_extern" = "xyes"; then
  use_[]gd_encoding="no";
fi

if test "x$use_[]gd_encoding" = "xyes"; then
  dnl search for library
  saved_ldflags=$LDFLAGS
  saved_libs=$LIBS
  if test "x$gd_encoding[]_prefix" != "x"; then
    LDFLAGS="$LDFLAGS -L$gd_encoding[]_prefix/lib"
  fi
  AC_CHECK_LIB([$2],[$3],[have_this_lib=yes
  LIBS="$LIBS -l$2"]
  AC_DEFINE(AS_TR_CPP([HAVE_LIB$2]), 1,
    [Define to 1 if you have the `$2' library (-l$2).]))

ifelse(`x$7', `x',,[
  AC_CHECK_FUNCS([$7])
])
  LDFLAGS=$saved_ldflags
  LIBS=$saved_libs

dnl search for header
  saved_cppflags=$CPPFLAGS
  if test "x$gd_encoding[]_prefix" != "x"; then
    CPPFLAGS="$CPPFLAGS -I$gd_encoding[]_prefix/include"
  fi
  AC_CHECK_HEADERS([$4],[have_this_header=yes])
  CPPFLAGS=$saved_cppflags
fi

dnl cleanup
AS_TR_CPP(gd_encoding[_CPPFLAGS])=
AS_TR_CPP(gd_encoding[_LDFLAGS])=
AS_TR_CPP(gd_encoding[_LIBS])=
if test "x$have_this_header" = "xyes" -a "x$have_this_lib" = "xyes"; then
  if test "x$gd_encoding[]_prefix" = "x"; then
    AS_TR_CPP(gd_encoding[_LIBS])="-l$2"
    AS_TR_CPP(gd_encoding[_SEARCHPATH])="$PATH"
  else 
    AS_TR_CPP(gd_encoding[_CPPFLAGS])="-I$gd_encoding[]_prefix/include"
    AS_TR_CPP(gd_encoding[_LDFLAGS])="-L$gd_encoding[]_prefix/lib"
    AS_TR_CPP(gd_encoding[_LIBS])="-l$2"
    AS_TR_CPP(gd_encoding[_SEARCHPATH])="$gd_encoding[]_prefix/bin:$PATH"
  fi
  AC_DEFINE(AS_TR_CPP([USE_]gd_encoding), [], [ Define to ]
  [enable ]gd_encoding[ support ])
else
  use_[]gd_encoding="no";
  AS_TR_CPP(gd_encoding[_SEARCHPATH])="$PATH"
fi
AC_SUBST(AS_TR_CPP(gd_encoding[_CPPFLAGS]))
AC_SUBST(AS_TR_CPP(gd_encoding[_LDFLAGS]))
AC_SUBST(AS_TR_CPP(gd_encoding[_LIBS]))

dnl executables needed for tests
m4_define([gd_progname], regexp([$5 ], [^\([^ ]*\) ], [\1]))
AC_PATH_PROGS([path_]gd_progname, [$5], [not found],
  [$AS_TR_CPP(gd_encoding[_SEARCHPATH])])

if test "x$path_[]gd_progname" != "xnot found"; then
  AC_DEFINE_UNQUOTED(AS_TR_CPP(gd_progname), ["$path_]gd_progname["],
                     [ Define to the full path to the `]gd_progname[' binary ])
fi

ifelse(`x$6', `x',,[
m4_define([gd_unprogname], regexp([$6 ], [^\([^ ]*\) ], [\1]))
AC_PATH_PROGS([path_]gd_unprogname, [$6], [not found],
  [$AS_TR_CPP(gd_encoding[_SEARCHPATH])])

if test "x$path_[]gd_unprogname" != "xnot found"; then
  AC_DEFINE_UNQUOTED(AS_TR_CPP(gd_unprogname), ["$path_]gd_unprogname["],
    [ Define to the full path to the `]gd_unprogname[' binary ])
fi
])
AM_CONDITIONAL(AS_TR_CPP([USE_]gd_encoding),
    [test "x$use_]gd_encoding[" = "xyes"])

if test "x$path_[]gd_progname" != "xnot found" -a \
  "x$path_[]gd_unprogname" != "xnot found"; then
  AC_DEFINE(AS_TR_CPP([TEST_]gd_encoding), [],
      [ Define to enable ]gd_encoding[ tests ])
fi

dnl add to summary and private lib list
if test "x$use_[]gd_encoding" != "xno"; then
  if test "x$use_modules" != "xno"; then
    ENCODINGS_MODS="${ENCODINGS_MODS} gd_encoding";
  else
    ENCODINGS_BUILT="${ENCODINGS_BUILT} gd_encoding";
    if test -z "$PRIVATE_LIBS"; then
      PRIVATE_LIBS="[$]AS_TR_CPP(gd_encoding[_LDFLAGS]) [$]AS_TR_CPP(gd_encoding[_LIBS])"
    else
      PRIVATE_LIBS="[$]AS_TR_CPP(gd_encoding[_LDFLAGS]) $PRIVATE_LIBS [$]AS_TR_CPP(gd_encoding[_LIBS])"
    fi
  fi
else
  ENCODINGS_LEFT="${ENCODINGS_LEFT} gd_encoding";
fi
])
