dnl Copyright (C) 2013, 2015 D. V. Wiebe
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

dnl GD_PHP_CONFIG
dnl ---------------------------------------------------------------
dnl Get a PHP configuration option and store it in the supplied local
dnl variable.
AC_DEFUN([GD_PHP_CONFIG],
[
  $1=`${PHP_CONFIG} --$2`
ifelse(`x$3', `x',,[
  if test "x${$1}" = "x"; then
    $1="$3";
    have_php="no";
  elif test "x${$1}" = "xNONE"; then
    have_php="no";
  fi
])
])

dnl GD_PHP
dnl ---------------------------------------------------------------
dnl Look for PHP.
AC_DEFUN([GD_PHP],
[

have_php="yes"
AC_ARG_WITH([php-config], AS_HELP_STRING([--with-php-config=PATH],
            [use PATH as php-config.  [default: autodetect]]),
            [
              case "${withval}" in
                no) have_php="no" ;;
                yes) user_php_config= ;;
                *) user_php_config="${withval}" ;;
              esac
            ], [ user_php_config= ])

if test "x${have_php}" != "xno"; then
  dnl try to find php
  AC_PATH_PROGS(PHP_CONFIG, [$user_php_config php5-config php-config],
  [not found])

  if test "x$PHP_CONFIG" = "xnot found"; then
    have_php="no"
    PHP_CONFIG=
  fi
  AC_SUBST([PHP_CONFIG])
fi

dnl php CLI
if test "x${have_php}" != "xno"; then
  AC_MSG_CHECKING([PHP interpreter path])
  GD_PHP_CONFIG([PHP], [php-binary], [UNKNOWN])
  AC_MSG_RESULT([$PHP])
fi

dnl extension dir
AC_ARG_WITH([php-dir], AS_HELP_STRING([--with-php-dir=DIR],
      [install the GetData PHP extension into DIR [default: autodetect]]),
      [
      if test "x${withval}" = "xno"; then
        phpdir=UNKNOWN;
      else
        phpdir=${withval};
      fi
      ], [phpdir=UNKNOWN])

if test "x${have_php}" != "xno"; then
  AC_SUBST([PHP])
  AC_MSG_CHECKING([the PHP extension directory])
  if test "x${phpdir}" = "xUNKNOWN"; then
    GD_PHP_CONFIG([phpprefix], [prefix], [UNKNOWN])
    GD_PHP_CONFIG([prefixed_phpdir], [extension-dir], [UNKNOWN])
    if test "x${prefixed_phpdir}" = "xUNKNOWN"; then
      phpdir=${libdir}/php/extensions
    elif test "x${SED}" != "x"; then
      esc_phpprefix=$(echo ${phpprefix} | ${SED} -e 's/\//\\\//g')
      phpdir=$(echo ${prefixed_phpdir} | \
          ${SED} -e "s/^${esc_phpprefix}/\${exec_prefix}/")
    fi
  fi
  AC_MSG_RESULT([$phpdir])
  AC_SUBST([phpdir])

  AC_MSG_CHECKING([PHP CPPFLAGS])
  GD_PHP_CONFIG([PHP_CPPFLAGS], [includes])
  AC_MSG_RESULT([$PHP_CPPFLAGS])
  AC_SUBST([PHP_CPPFLAGS])

  AC_MSG_CHECKING([PHP LDFLAGS])
  GD_PHP_CONFIG([PHP_LDFLAGS], [ldflags])
  AC_MSG_RESULT([$PHP_LDFLAGS])
  AC_SUBST([PHP_ldflags])

  AC_MSG_CHECKING([PHP LIBS])
  GD_PHP_CONFIG([PHP_LIBS], [libs])
  AC_MSG_RESULT([$PHP_LIBS])
  AC_SUBST([PHP_libs])
fi
])
