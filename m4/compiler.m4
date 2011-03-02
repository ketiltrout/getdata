dnl Copyright (C) 2008, 2009, 2010 D. V. Wiebe
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

dnl GD_LANG_COMPILER_INTEL
dnl -------------------------------------------------------------
dnl Check whether the compiler for the current language is Intel.
dnl
dnl This is modelled after autoconf's _AC_LANG_COMPILER_GNU macro.
AC_DEFUN([GD_LANG_COMPILER_INTEL],
[AC_CACHE_CHECK([whether we are using the Intel _AC_LANG compiler],
[gd_cv_[]_AC_LANG_ABBREV[]_compiler_intel],
[AC_COMPILE_IFELSE([AC_LANG_PROGRAM([], [[#ifndef __INTEL_COMPILER
       choke me
#endif
]])],
[gd_compiler_intel=yes],
[gd_compiler_intel=no])
gd_cv_[]_AC_LANG_ABBREV[]_compiler_intel=$gd_compiler_intel
])])

dnl GD_LANG_F77_COMPILER_INTEL
dnl -------------------------------------------------------------
dnl Check whether the compiler for Fortran is Intel.
AC_DEFUN([GD_LANG_F77_COMPILER_INTEL],
[AC_CACHE_CHECK([whether we are using the Intel Fortran-77 compiler],
[gd_cv_f77_compiler_intel],
[if $F77 -help 2>/dev/null | grep -q 'Intel.R. Fortran Compiler'; then
gd_cv_f77_compiler_intel=yes
else
gd_cv_f77_compiler_intel=no
fi
])])

dnl GD_LANG_FC_COMPILER_INTEL
dnl -------------------------------------------------------------
dnl Check whether the compiler for free-form Fortran is Intel.
AC_DEFUN([GD_LANG_FC_COMPILER_INTEL],
[AC_CACHE_CHECK([whether we are using the Intel Fortran compiler],
[gd_cv_fc_compiler_intel],
[if $FC -help 2>/dev/null | grep -q 'Intel.R. Fortran Compiler'; then
gd_cv_fc_compiler_intel=yes
else
gd_cv_fc_compiler_intel=no
fi
])])

dnl GD_PROG_CC_WEXTRA
dnl -------------------------------------------------------------
dnl Check whether the C compiler accepts -Wextra
AC_DEFUN([GD_PROG_CC_WEXTRA],
[gd_saved_CFLAGS=$CFLAGS
AC_CACHE_CHECK([whether $CC accepts -Wextra], gd_cv_prog_cc_wextra,
[CFLAGS="-Wextra"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM()], [gd_cv_prog_cc_wextra=yes],
[gd_cv_prog_cc_wextra=no])])
CFLAGS=$gd_saved_CFLAGS])


dnl GD_PROG_CXX_WEXTRA
dnl -------------------------------------------------------------
dnl Check whether the C++ compiler accepts -Wextra
AC_DEFUN([GD_PROG_CXX_WEXTRA],
[gd_saved_CXXFLAGS=$CXXFLAGS
AC_CACHE_CHECK([whether $CXX accepts -Wextra], gd_cv_prog_cxx_wextra,
[CXXFLAGS="-Wextra"
AC_LANG_PUSH([C++])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM()], [gd_cv_prog_cxx_wextra=yes],
[gd_cv_prog_cxx_wextra=no])
AC_LANG_POP([C++])])
CXXFLAGS=$gd_saved_CXXFLAGS])


dnl GD_PROG_F77_WEXTRA
dnl -------------------------------------------------------------
dnl Check whether the Fotran-77 compiler accepts -Wextra
AC_DEFUN([GD_PROG_F77_WEXTRA],
[gd_saved_FFLAGS=$FFLAGS
AC_CACHE_CHECK([whether $F77 accepts -Wextra], gd_cv_prog_f77_wextra,
[FFLAGS="-Wextra"
AC_LANG_PUSH([Fortran 77])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM()], [gd_cv_prog_f77_wextra=yes],
[gd_cv_prog_f77_wextra=no])
AC_LANG_POP([Fortran 77])])
FFLAGS=$gd_saved_FFLAGS])


dnl GD_PROG_FC_WEXTRA
dnl -------------------------------------------------------------
dnl Check whether the free-form Fotran compiler accepts -Wextra
AC_DEFUN([GD_PROG_FC_WEXTRA],
[gd_saved_FCFLAGS=$FCFLAGS
AC_CACHE_CHECK([whether $FC accepts -Wextra], gd_cv_prog_fc_wextra,
[FCFLAGS="-Wextra"
AC_LANG_PUSH([Fortran 77])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM()], [gd_cv_prog_fc_wextra=yes],
[gd_cv_prog_fc_wextra=no])
AC_LANG_POP([Fortran 77])])
FCFLAGS=$gd_saved_FCFLAGS])

dnl GD_PROG_CC_WALL
dnl -------------------------------------------------------------
dnl Check whether the C compiler accepts -Wall
AC_DEFUN([GD_PROG_CC_WALL],
[gd_saved_CFLAGS=$CFLAGS
AC_CACHE_CHECK([whether $CC accepts -Wall], gd_cv_prog_cc_wall,
[CFLAGS="-Wall"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM()], [gd_cv_prog_cc_wall=yes],
[gd_cv_prog_cc_wall=no])])
CFLAGS=$gd_saved_CFLAGS])


dnl GD_PROG_CXX_WALL
dnl -------------------------------------------------------------
dnl Check whether the C++ compiler accepts -Wall
AC_DEFUN([GD_PROG_CXX_WALL],
[gd_saved_CXXFLAGS=$CXXFLAGS
AC_CACHE_CHECK([whether $CXX accepts -Wall], gd_cv_prog_cxx_wall,
[CXXFLAGS="-Wall"
AC_LANG_PUSH([C++])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM()], [gd_cv_prog_cxx_wall=yes],
[gd_cv_prog_cxx_wall=no])
AC_LANG_POP([C++])])
CXXFLAGS=$gd_saved_CXXFLAGS])


dnl GD_PROG_F77_WALL
dnl -------------------------------------------------------------
dnl Check whether the Fotran-77 compiler accepts -Wall
AC_DEFUN([GD_PROG_F77_WALL],
[gd_saved_FFLAGS=$FFLAGS
AC_CACHE_CHECK([whether $F77 accepts -Wall], gd_cv_prog_f77_wall,
[FFLAGS="-Wall"
AC_LANG_PUSH([Fortran 77])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM()], [gd_cv_prog_f77_wall=yes],
[gd_cv_prog_f77_wall=no])
AC_LANG_POP([Fortran 77])])
FFLAGS=$gd_saved_FFLAGS])


dnl GD_PROG_FC_WALL
dnl -------------------------------------------------------------
dnl Check whether the free-form Fotran compiler accepts -Wall
AC_DEFUN([GD_PROG_FC_WALL],
[gd_saved_FCFLAGS=$FCFLAGS
AC_CACHE_CHECK([whether $FC accepts -Wall], gd_cv_prog_fc_wall,
[FCFLAGS="-Wall"
AC_LANG_PUSH([Fortran 77])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM()], [gd_cv_prog_fc_wall=yes],
[gd_cv_prog_fc_wall=no])
AC_LANG_POP([Fortran 77])])
FCFLAGS=$gd_saved_FCFLAGS])

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

dnl GD_CHECK_LFS_TYPE
dnl -----------------------------------------------------------
dnl Check whether the specified type exists, and whether _LARGEFILE64_SOURCE
dnl must be defined.
AC_DEFUN([GD_CHECK_LFS_TYPE],[
AC_CACHE_CHECK([for $1], [AS_TR_SH([gd_cv_type_$1])],[
if test "x$gd_cv_c_need_lfs_defined" != "xyes"; then
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([$2],[
if (sizeof($1))
return 0;])],
[gd_cv_type_[]AS_TR_SH($1)="yes"])
fi

if test "x$AS_TR_SH([gd_cv_type_$1])" != "xyes"; then
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
#define _LARGEFILE64_SOURCE 1
$2],[
if (sizeof($1))
return 0;])],
[if test "x$gd_cv_c_need_lfs_defined" = "xyes"; then
AS_TR_SH([gd_cv_type_$1])="yes"
else
AS_TR_SH([gd_cv_type_$1])="yes, with _LARGEFILE64_SOURCE defined"
fi
gd_cv_c_need_lfs_defined="yes"],
[AS_TR_SH([gd_cv_type_$1])="no"])
fi
])

if test "x$AS_TR_SH([gd_cv_type_$1])" != "xno"; then
AC_DEFINE(AS_TR_CPP([HAVE_$1]), [1],
[ Define to 1 if you have the type `$1' ])
fi
])
