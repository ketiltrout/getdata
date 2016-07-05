dnl Copyright (C) 2013, 2014, 2016 D. V. Wiebe
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

dnl GD_MATLAB
dnl ---------------------------------------------------------------
dnl Look for mex; sets and substs MEX on success
AC_DEFUN([GD_MEX],
[
dnl --without-mex basically does the same as --disable-matlab
AC_ARG_WITH([mex], AS_HELP_STRING([--with-mex=PATH],
            [use the MATLAB MEX compiler located at PATH.  ]
            [[default: autodetect]]),
            [
              case "${withval}" in
                no) have_matlab="no" ;;
                yes) user_mex= ;;
                *) user_mex="${withval}";;
              esac
            ], [ user_mex= ])

dnl try to find mex
MEX="not found"
if test "x$user_mex" != "x"; then
  MEX=$user_mex
else
  AC_PATH_PROG([MEX], [mex], [not found])
fi

dnl TexLive contains a /usr/bin/mex
if test "x$MEX" != "xnot found"; then
  AC_MSG_CHECKING([if $MEX is a MATLAB mex compiler])
  mex_out=`$MEX 2>&1`
  mex_status=$?
  if test $mex_status -eq 1; then
    if ! echo $mex_out | grep 'consult the MATLAB External Interfaces Guide' \
      >/dev/null 2>/dev/null;
    then
      MEX="not found";
    fi
  fi
  if test "x$MEX" = "xnot found"; then
    AC_MSG_RESULT([no])
  else
    AC_MSG_RESULT([yes])
  fi
fi

if test "x$MEX" = "xnot found"; then
  have_matlab="no"
  MEX=
fi

AC_SUBST([MEX])
])

dnl GD_MEX_VAR
dnl ---------------------------------------------------------------
dnl Determine a MEX variable and store it in the supplied local
dnl variable
AC_DEFUN([GD_MEX_VAR],
[
$1=`$MEX -v 2>/dev/null | ${AWK} '/$2 *=/ { print @S|@4 }'`
])

dnl GD_MATLAB_EVAL
dnl ---------------------------------------------------------------
dnl Evaluate something in MATLAB and store it in the supplied local
dnl variable
AC_DEFUN([GD_MATLAB_EVAL],
[
ifelse(`$#', `2', [matlab_int=$MATLAB], [matlab_int=$3])
$1=`$matlab_int -nodisplay -nosplash -nojvm -nodesktop -r \
  "fprintf(2, '@@@%s@@@\n', $2); quit" 2>&1 >/dev/null | \
  ${AWK} 'BEGIN { FS="@@@" } /^@@@/ { print @S|@2 }'`
])

dnl GD_MATLAB
dnl ---------------------------------------------------------------
dnl Look for matlab, &c.
dnl
AC_DEFUN([GD_MATLAB],
[
dnl --without-matlab basically does the same as --disable-matlab
AC_ARG_WITH([matlab], AS_HELP_STRING([--with-matlab=PATH],
            [use the MATLAB interpreter located at PATH.  ]
            [[default: autodetect]]),
            [
              case "${withval}" in
                no) have_matlab="no" ;;
                yes) user_matlab= ; have_matlab= ;;
                *) user_matlab="${withval}"; have_matlab= ;;
              esac
            ], [ user_matlab=; have_matlab= ])

dnl find MEX first
if test "x${have_matlab}" != "xno"; then
  GD_MEX
fi

dnl find matlab
if test "x${have_matlab}" != "xno"; then
  dnl try to find matlab
  if test "x$user_matlab" != "x"; then
    MATLAB=$user_matlab;
  else
    AC_PATH_PROG([MATLAB], [matlab], [not found])
  fi

  if test "x$MATLAB" != "xnot found"; then
    AC_MSG_CHECKING([$MATLAB version])
    GD_MATLAB_EVAL([MATLAB_VERSION], [version], [$MATLAB])
    if test "x$MATLAB_VERSION" = "x"; then
      AC_MSG_RESULT([none])
      MATLAB="not found"
    else
      AC_MSG_RESULT([$MATLAB_VERSION])
    fi
  fi

  if test "x$MATLAB" = "xnot found"; then
    have_matlab=no
    MATLAB=
  fi
  AC_SUBST([MATLAB])
fi

if test "x${have_matlab}" != "xno"; then
  dnl installdir
  default_matlabbasedir=$libdir/getdata/matlab
  AC_ARG_WITH([matlab-dir], AS_HELP_STRING([--with-matlab-dir=PATH],
        [ Install Matlab bindings in PATH/getdata.  ]
        [[default: LIBDIR/getdata/matlab]]),
      [
      case "${withval}" in
      no) matlabbasedir=$default_matlabbasedir ;;
      *) matlabbasedir="${withval}" ;;
      esac
      ], [ matlabbasedir=$default_matlabbasedir ])
  AC_MSG_CHECKING([matlab install directory])
  AC_MSG_RESULT([$matlabbasedir])
  AC_SUBST([matlabbasedir])

  dnl mex extension
  AC_MSG_CHECKING([MEX extension])
  GD_MATLAB_EVAL([mexext], [mexext])
  AC_MSG_RESULT([.$mexext])
  AC_SUBST([mexext])

  dnl flags
  AC_MSG_CHECKING([MatLab CPPFLAGS])
  GD_MEX_VAR([matlab_prefix], [MATLAB])
  MATLAB_CPPFLAGS="-I${matlab_prefix}/extern/include"
  AC_MSG_RESULT([$MATLAB_CPPFLAGS])
  AC_SUBST([MATLAB_CPPFLAGS])
fi
])
