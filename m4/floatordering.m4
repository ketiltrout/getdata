dnl Copyright (C) 2010 D. V. Wiebe
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

dnl GD_C_FLOATORDERING
dnl -----------------------------------------------------------
dnl Attempt to determine the ordering of double precision floats
AC_DEFUN([GD_C_FLOATORDERING],
  [AC_CACHE_CHECK([floating point endianness], [gd_cv_c_floatordering],
[gd_cv_c_floatordering=unknown
# check for arm middle endianness
AC_COMPILE_IFELSE([AC_LANG_SOURCE(
[[#if ! (defined __arm__ && ! (defined __VFP_FP__ || defined __MAVERICK___))
  not arm middle endian
#endif
]])],
[gd_cv_c_floatordering=arm],
[# not middle-endian arm, check for __FLOAT_WORD_ORDER
AC_COMPILE_IFELSE(
[AC_LANG_PROGRAM([[#include <endian.h>]],
[[#ifndef __FLOAT_WORD_ORDER
  no __FLOAT_WORD_ORDER defined
#endif
]])],
[# __FLOAT_WORD_ORDER is defined; is it BIG_ENDIAN?
AC_COMPILE_IFELSE(
[AC_LANG_PROGRAM([[#include <endian.h>]],
[[#if __FLOAT_WORD_ORDER != BIG_ENDIAN
  not big endian
#endif
]])],
[gd_cv_c_floatordering="big"],
[gd_cv_c_floatordering="little"])],
[# no __FLOAT_WORD_ORDER defined; so we just assume it's the same as the
 # integer ordering
AC_COMPILE_IFELSE(
[AC_LANG_SOURCE(
[[#ifndef WORDS_BIGENDIAN
  not big endian
#endif
]])],
[gd_cv_c_floatordering="big"],
[gd_cv_c_floatordering="little"])])])])

if test $gd_cv_c_floatordering = "arm"; then
AC_DEFINE([ARM_ENDIAN_DOUBLES], [1],
  [ Define to 1 if your processor stores double-precision floats in the old
    ARM middle-endian format ])
elif test $gd_cv_c_floatordering = "big"; then
AC_DEFINE([FLOATS_BIGENDIAN], [1],
  [ Define to 1 if your processor stores double-precision floats in big-endian
    order])
fi])
