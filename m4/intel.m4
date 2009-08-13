dnl (C) 2008 D. V. Wiebe
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
