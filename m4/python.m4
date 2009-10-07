dnl (C) 2009 D. V. Wiebe
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

dnl GD_PYTHON
dnl ---------------------------------------------------------------
dnl Look for python.  Then determine whether we can build extension modules.
AC_DEFUN([GD_PYTHON],
[
last_python=2.6
first_python=$1

AC_CHECK_PROGS([SEQ], [seq], [not found])
if test "x$SEQ" == "xnot found"; then
  python_prog_list="python python2"
else
python_prog_list="python python2 dnl
`$SEQ -f 'python%.1f' $last_python -0.1 $first_python`" #'
fi

dnl --without-python basically does the same as --disable-python
AC_ARG_WITH([python], AS_HELP_STRING([--with-python=PATH],
            [use the Python interpreter PATH [[autodetect]]]),
            [
              case "${withval}" in
                no) have_python="no" ;;
                yes) user_python= ; have_python= ;;
                *) user_python="${withval}"; have_python= ;;
              esac
            ], [ user_python=; have_python= ])

if test "x${have_python}" != "xno"; then

dnl try to find a sufficiently new python.
if test "x$user_python" != "x"; then
  AC_MSG_CHECKING([whether $user_python version >= $first_python])
  AM_PYTHON_CHECK_VERSION([$user_python], [$first_python],
  [AC_MSG_RESULT([yes])
  PYTHON=$user_python],
  [AC_MSG_RESULT([no])
  PYTHON="not found"])
else
  AC_MSG_CHECKING([for Python interpreter version >= $first_python])
  PYTHON="not found"
  for py in $python_prog_list; do
  _AS_PATH_WALK([$PATH],
  [for exec_ext in '' $ac_executable_extensions; do
    if AS_EXECUTABLE_P(["$as_dir/$py$exec_ext"]); then
      AM_PYTHON_CHECK_VERSION( ["$as_dir/$py$exec_ext"],
      [$first_python], [ PYTHON="$as_dir/$py$exec_ext"; break 3] )
    fi
  done])
  done
  AC_MSG_RESULT([$PYTHON])
fi

if test "x$PYTHON" = "xnot found"; then
  have_python="no"
  PYTHON=
fi
AC_SUBST([PYTHON])

fi

if test "x${have_python}" != "xno"; then
dnl python version
AC_MSG_CHECKING([$PYTHON version])
PYTHON_VERSION=`$PYTHON -c "import sys; print sys.version[[:3]]"`
AC_MSG_RESULT([$PYTHON_VERSION])

dnl calculate python CPPFLAGS and LIBS
if test -x $PYTHON-config; then
  PYTHON_CPPFLAGS=`$PYTHON-config --includes 2>/dev/null`
  PYTHON_LIBS=`$PYTHON-config --ldflags 2>/dev/null`
else
  python_prefix=`$PYTHON -c "import sys; print sys.prefix"`
  python_exec_prefix=`$PYTHON -c "import sys; print sys.exec_prefix"`
  python_libdir=`$PYTHON -c "from distutils import sysconfig; print sysconfig.get_config_var('LIBDIR')"`
  python_syslibs=`$PYTHON -c "from distutils import sysconfig; print sysconfig.get_config_var('SYSLIBS')"`
  python_shlibs=`$PYTHON -c "from distutils import sysconfig; print sysconfig.get_config_var('SHLIBS')"`
  python_modlibs=`$PYTHON -c "from distutils import sysconfig; print sysconfig.get_config_var('MODLIBS')"`

  PYTHON_CPPFLAGS="-I${python_prefix}/include/python${PYTHON_VERSION} -I${python_exec_prefix}/include/python${PYTHON_VERSION}"
  PYTHON_LIBS="-L${python_libdir} $python_syslibs $python_shlibs $python_modlibs -lpython${PYTHON_VERSION}"
fi
AC_MSG_CHECKING([Python includes])
AC_MSG_RESULT([$PYTHON_CPPFLAGS])
AC_SUBST([PYTHON_CPPFLAGS])
AC_MSG_CHECKING([Python libraries])
AC_MSG_RESULT([$PYTHON_LIBS])
AC_SUBST([PYTHON_LIBS])

dnl header check
saved_CPPFLAGS=${CPPFLAGS}
CPPFLAGS="${CPPFLAGS} ${PYTHON_CPPFLAGS}"
AC_CHECK_HEADERS(Python.h,,[have_python="no"])
CPPFLAGS=${saved_CPPFLAGS}

fi

if test "x${have_python}" != "xno"; then

dnl calculate the exec prefix
pyexec_prefix=$exec_prefix
test "x$pyexec_prefix" = xNONE && pyexec_prefix=$prefix
test "x$pyexec_prefix" = xNONE && pyexec_prefix=$ac_default_prefix

dnl calculate the extension module directory
AC_MSG_CHECKING([for $PYTHON extension module directory])
pythondir=`$PYTHON -c "from distutils import sysconfig; print sysconfig.get_python_lib(1,0,prefix='${pyexec_prefix}')" 2>/dev/null || echo "${pyexec_prefix}/lib/python${PYTHON_VERSION}/site-packages"`
AC_SUBST([pythondir])
AC_MSG_RESULT([$pythondir])

saved_CPPFLAGS=${CPPFLAGS}
CPPFLAGS="${CPPFLAGS} ${PYTHON_CPPFLAGS}"
saved_LIBS=$LIBS
LIBS="$LIBS ${PYTHON_LIBS}"
dnl try to compile a module
AC_MSG_CHECKING([if we can compile a simple Python extension module])
AC_TRY_LINK_FUNC([Py_Initialize], [ AC_MSG_RESULT([yes]) ],
[ AC_MSG_RESULT([no]); have_python="no" ])
CPPFLAGS=$saved_CPPFLAGS
LIBS=$saved_LIBS

fi
])
