dnl Copyright (C) 2009-2011 D. V. Wiebe
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

dnl GD_PERL_CONFIG
dnl ---------------------------------------------------------------
dnl query perl for the specified configuration value and store it in the
dnl supplied local variable
AC_DEFUN([GD_PERL_CONFIG],
[
ifelse(`$#', `2', [perl_int=$PERL], [perl_int=$3])
# 3=$3
$1=`$perl_int -V::$2: | sed -e "s/'//g" | sed -e "s/[ \t]*$//"`
])

dnl GD_PERL_CHECK_VERSION
dnl ---------------------------------------------------------------
dnl Define PERL_VERSION to the version of the idl interpreter
AC_DEFUN([GD_PERL_CHECK_VERSION],
[
perl_version_ok="no"
GD_PERL_CONFIG([PERL_VERSION], [version], [$1])
AX_COMPARE_VERSION([$PERL_VERSION],[ge],[$2],[perl_version_ok="yes"])
if test "x$perl_version_ok" = "xyes"; then
  $3
  true
else
  $4
  true
fi
])

dnl GD_PERL
dnl ---------------------------------------------------------------
dnl Look for perl5.  Then determine whether we can build XSUBs.
AC_DEFUN([GD_PERL],
[
first_perl=5.8.0
perl_prog_list="perl perl5 \
perl5.14 perl5.12 perl5.10 perl5.8 \
perl5.14.0 \
perl5.12.3 perl5.12.2 perl5.12.1 perl5.12.0 \
perl5.10.1 perl5.10.0 \
perl5.8.9 perl5.8.8 perl5.8.7 perl5.8.6 perl5.8.5 perl5.8.4 perl5.8.3 \
perl5.8.2 perl5.8.1 perl5.8.0"

dnl --without-perl basically does the same as --disable-perl
AC_ARG_WITH([perl], AS_HELP_STRING([--with-perl=PATH],
            [use the Perl interpreter located in PATH.  If this is something
            unusual, you may also need to specify the location of xsubpp via
            the XSUBPP variable. [autodetect]]),
            [
              case "${withval}" in
                no) have_perl="no" ;;
                yes) user_perl= ; have_perl= ;;
                *) user_perl="${withval}"; have_perl= ;;
              esac
            ], [ user_perl=; have_perl= ])

if test "x${have_perl}" != "xno"; then

  dnl try to find a sufficiently new perl.
  if test "x$user_perl" != "x"; then
    AC_MSG_CHECKING([whether $user_perl version >= $first_perl])
    GD_PERL_CHECK_VERSION([$user_perl], [$first_perl],
    [AC_MSG_RESULT([yes])
    PERL=$user_perl],
    [AC_MSG_RESULT([no])
    PERL="not found"])
  else
    AC_MSG_CHECKING([for Perl interpreter version >= $first_perl])
    PERL="not found"
    for perl in $perl_prog_list; do
    _AS_PATH_WALK([$PATH],
    [for exec_ext in '' $ac_executable_extensions; do
      if AS_EXECUTABLE_P(["$as_dir/$perl$exec_ext"]); then
        GD_PERL_CHECK_VERSION( ["$as_dir/$perl$exec_ext"],
        [$first_perl], [ PERL="$as_dir/$perl$exec_ext"; break 3] )
      fi
    done])
    done
    AC_MSG_RESULT([$PERL])
  fi

  if test "x$PERL" = "xnot found"; then
    have_perl="no"
    PERL=
  fi
  AC_SUBST([PERL])

  fi

  if test "x${have_perl}" != "xno"; then
  dnl perl version
  AC_MSG_CHECKING([$PERL version])
  GD_PERL_CONFIG([PERL_VERSION], [version])
  AC_MSG_RESULT([$PERL_VERSION])

  dnl calculate build flags
  GD_PERL_CONFIG([perl_archdir], [archlibexp])
  PERL_CPPFLAGS="-I${perl_archdir}/CORE"
  AC_MSG_CHECKING([Perl includes])
  AC_MSG_RESULT([$PERL_CPPFLAGS])
  AC_SUBST([PERL_CPPFLAGS])

  GD_PERL_CONFIG([PERL_CCCDLFLAGS], [cccdlflags])
  GD_PERL_CONFIG([PERL_CCFLAGS], [ccflags])
  PERL_CFLAGS="${PERL_CCCDLFLAGS} ${PERL_CCFLAGS}"
  AC_MSG_CHECKING([Perl compiler flags])
  AC_MSG_RESULT([$PERL_CFLAGS])
  AC_SUBST([PERL_CFLAGS])

  GD_PERL_CONFIG([PERL_LDFLAGS], [lddlflags])
  AC_MSG_CHECKING([Perl linker flags])
  AC_MSG_RESULT([$PERL_LDFLAGS])
  AC_SUBST([PERL_LDFLAGS])

  dnl header check
  saved_CPPFLAGS=${CPPFLAGS}
  CPPFLAGS="${CPPFLAGS} ${PERL_CPPFLAGS}"
  AC_CHECK_HEADERS([EXTERN.h XSUB.h],,[have_perl="no"])
  if test "x$have_perl" != "xno"; then
    AC_CHECK_HEADERS([perl.h],,[have_perl="no"],[
  #include <EXTERN.h>
  ])
  fi
  CPPFLAGS=${saved_CPPFLAGS}

fi

AC_ARG_WITH([perl-dir], AS_HELP_STRING([--with-perl-dir=PATH],
[ Install Perl bindings in PATH.  If PATH is the special word `vendor', install
Perl bindings into the default vendor-specific module directory (if present).
If PATH is the special word `site', install Perl bindings into the default
site-specific module directory.  [site] ]),
[
case "${withval}" in
vendor|site) perl_inst_type="${withval}" ;;
no) perl_inst_type="site" ;;
*) perl_inst_type="local"; local_perl_path="${withval}"
esac
], [ perl_inst_type="site" ])

if test "x${have_perl}" != "xno"; then

  dnl calculate the extension module directory
  AC_MSG_CHECKING([Perl module directory])
  if test $perl_inst_type = "vendor"; then
    GD_PERL_CONFIG([perldir], [vendorarchexp])
    GD_PERL_CONFIG([perlmandir], [vendorman3direxp])
    if test perldir = "UNKNOWN"; then
      perl_inst_type = "site";
    fi
  fi

  if test $perl_inst_type = "site"; then
    GD_PERL_CONFIG([perldir], [sitearchexp])
    GD_PERL_CONFIG([perlmandir], [siteman3direxp])
  elif test $perl_inst_type != "vendor"; then
    perldir="${local_perl_path}"
    perlmandir="${mandir}"
  fi

  if test $perlmandir = "UNKNOWN"; then
    perlmandir="${mandir}"
  fi

  AC_SUBST([perldir])
  AC_MSG_RESULT([$perldir])

  AC_ARG_VAR([XSUBPP],
      [Command to compile XSUBs into C files, if building the Perl bindings])
  AC_PATH_PROGS([XSUBPP], [xsubpp], [not found])
  if test "x$XSUBPP" = "xnot found"; then
    have_perl="no"
  fi

fi
])
