dnl Copyright (C) 2013 D. V. Wiebe
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

dnl GD_PHP_INI_GET
dnl ---------------------------------------------------------------
dnl Get a PHP configuration option and store it in the supplied local
dnl variable.
AC_DEFUN([GD_PHP_INI_GET],
[
  $1=`${PHP} -r 'echo ini_get("$2");'`
  if test "x${$1}" = "x"; then
    $1="$3";
    have_php="no";
  fi
])

dnl GD_PHP
dnl ---------------------------------------------------------------
dnl Look for PHP.
AC_DEFUN([GD_PHP],
[

have_php="yes"
AC_ARG_WITH([php], AS_HELP_STRING([--with-php=PATH],
            [use the php interpreter at PATH.  [default: autodetect]]),
            [
              case "${withval}" in
                no) have_php="no" ;;
                yes) user_php= ;;
                *) user_php="${withval}" ;;
              esac
            ], [ user_php= ])

AC_ARG_WITH([phpize], AS_HELP_STRING([--with-phpize=PATH],
            [use the phpize script at PATH.  [default: autodetect]]),
            [
              case "${withval}" in
                no) have_php="no" ;;
                yes) user_phpize= ;;
                *) user_phpize="${withval}" ;;
              esac
            ], [ user_phpize= ])

if test "x${have_php}" != "xno"; then
  dnl try to find php
  AC_PATH_PROGS(PHP, [$user_php php5 php], [not found])

  if test "x$PHPIZE" = "xnot found"; then
    have_php="no"
    PHPIZE=
  fi
  AC_SUBST([PHPIZE])

  dnl try to find phpize
  AC_PATH_PROGS(PHPIZE, [$user_phpize phpize5 phpize], [not found])

  if test "x$PHPIZE" = "xnot found"; then
    have_php="no"
    PHPIZE=
  fi
  AC_SUBST([PHPIZE])
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
  AC_MSG_CHECKING([the PHP extension directory])
  if test "x${phpdir}" = "xUNKNOWN"; then
    GD_PHP_INI_GET([phpdir], [extension_dir], [UNKNOWN])
  fi
  AC_MSG_RESULT([$phpdir])
  AC_SUBST([phpdir])
fi
])
