dnl Copyright (C) 2010, 2011, 2016 D. V. Wiebe
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

dnl GD_MSYS_SHELL
dnl ---------------------------------------------------------------
dnl Find the Win32 path to the MSYS shell.
AC_DEFUN([GD_MSYS_SHELL],
[
AC_MSG_CHECKING([whether we're running under Msys])
if test "x`uname -o 2> /dev/null`" = "xMsys"; then
this_is_msys=yes
else
this_is_msys=no
fi
AC_MSG_RESULT([$this_is_msys])

if test "$this_is_msys" = "yes"; then
AC_MSG_CHECKING([for the Win32 MSYS shell])
msys_root=`mount | ${AWK} '/on \/ / {print [$]1}'`
msys_shell1="$msys_root`echo $SHELL | ${SED} -e 's/\//\\\\/g'`.exe"
msys_shell=`echo "$msys_shell1" | ${SED} -e 's/\\\\/\\\\\\\\/g'`
AC_MSG_RESULT([$msys_shell1])
AC_DEFINE_UNQUOTED([MSYS_SHELL], ["$msys_shell"],
[ Define to the full Win32 path to the `sh.exe' binary ])
fi
])
