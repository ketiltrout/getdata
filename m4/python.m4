dnl Copyright (C) 2009, 2011, 2013, 2016 D. V. Wiebe
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

dnl GD_LOG_SHELL(STUFF)
dnl ---------------------------------------------------------------
dnl Run STUFF as a shell command and log it
AC_DEFUN([GD_LOG_SHELL],
[{ echo "$as_me:$LINENO: $1" >&AS_MESSAGE_LOG_FD
($1) >&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD
ac_status=$?
echo "$as_me:$LINENO: \$? = $ac_status" >&AS_MESSAGE_LOG_FD
(exit $ac_status); }])

dnl GD_PYTHON_CONFIGVAR(VAR, KEY)
dnl ---------------------------------------------------------------
dnl Store the value of config_var KEY in VAR
dnl
dnl the superfluous exec call here is to work around a bug in
dnl bash-4.1's parser. (See change 1.hh in bash-4.2-alpha)
AC_DEFUN([GD_PYTHON_CONFIGVAR],
[
  $1=$(exec ${PYTHON} - <<EOF 2>/dev/null
try:
  import sysconfig
except ImportError:
  from distutils import sysconfig

print (sysconfig.get_config_var("$2"))
EOF
)

  if test "x${$1}" = "x"; then $1=None; fi
])

dnl GD_PYTHON3(PROG)
dnl ---------------------------------------------------------------
dnl If PROG is a Python3 interpreter, set have_python3 to "yes"
dnl otherwise set it to "no"
AC_DEFUN([GD_PYTHON3],
[
AS_IF(GD_LOG_SHELL(
    [$1 -c 'import sys; sys.exit(int(sys.version@<:@:1@:>@) - 3)']
  ), [have_python3=yes],[have_python3=no])
])

dnl GD_PYTHON_MIN_VERSION(PROG,VERSION2,VERSION3,ACTION-IF-TRUE,ACTION-IF_FALSE)
dnl ---------------------------------------------------------------
dnl Make sure Python interpreter PROG is at least VERSION2 for Python2 or
dnl VERSION3 for Python3.  If new enough, run ACTION-IF-TRUE, otherwise
dnl run ACTION-IF-FALSE.
AC_DEFUN([GD_PYTHON_MIN_VERSION],
[
GD_PYTHON3([$1])
if test "$have_python3" = "no"; then
  AM_PYTHON_CHECK_VERSION([$1],[$2],[$4],[$5])
else
  AM_PYTHON_CHECK_VERSION([$1],[$3],[$4],[$5])
fi
])

dnl GD_PYTHON
dnl ---------------------------------------------------------------
dnl Look for Python.  Then determine whether we can build extension modules.
AC_DEFUN([GD_PYTHON],
[
first_python2=$1
last_python2=2.7

first_python3=$2
last_python3=3.9 dnl have to stop somewhere

if test "x$SEQ" == "xnot found"; then
  if test "x$JOT" == "xnot found"; then
    python_prog_list="python python3 python2"
  else
    python_prog_list="python dnl
    python3 `$JOT -w 'python%.1f' - $last_python3 $first_python3 -0.1` dnl
    python2 `$JOT -w 'python%.1f' - $last_python2 $first_python2 -0.1`" #'
  fi
else
  python_prog_list="python dnl
  python3 `$SEQ -f 'python%.1f' $last_python3 -0.1 $first_python3` dnl
  python2 `$SEQ -f 'python%.1f' $last_python2 -0.1 $first_python2`" #'
fi

dnl --without-python basically does the same as --disable-python
AC_ARG_WITH([python], AS_HELP_STRING([--with-python=PATH],
            [use the Python interpreter located in PATH [default: autodetect]]),
            [
              case "${withval}" in
                no) have_python="no" ;;
                yes) user_python= ; have_python= ;;
                *) user_python="${withval}"; have_python= ;;
              esac
            ], [ user_python=; have_python= ])

AC_ARG_WITH([python-module-dir], AS_HELP_STRING([--with-python-module-dir=PATH],
      [install the Python bindings into PATH [default: autodetect]]),
      [
        case "${withval}" in
          no) local_python_modpath= ;;
          *) local_python_modpath="${withval}"
        esac
      ], [ local_python_modpath= ])

if test "x${have_python}" != "xno"; then

dnl try to find a sufficiently new Python - it would be nice to use
dnl AM_PATH_PYTHON, but it's buggy.
if test "x$user_python" != "x"; then
  AC_MSG_CHECKING([whether $user_python is new enough])
  GD_PYTHON_MIN_VERSION([$user_python], [$first_python2], [$first_python3],
  [AC_MSG_RESULT([yes])
  PYTHON=$user_python],
  [AC_MSG_RESULT([no])
  PYTHON="not found"])
else
  AC_MSG_CHECKING([for Python interpreter version >= $first_python2])
  PYTHON="not found"
  for py in $python_prog_list; do
  _AS_PATH_WALK([$PATH],
  [for exec_ext in '' $ac_executable_extensions; do
    if AS_EXECUTABLE_P(["$as_dir/$py$exec_ext"]); then
      GD_PYTHON_MIN_VERSION( ["$as_dir/$py$exec_ext"], [$first_python2],
      [$first_python3], [ PYTHON="$as_dir/$py$exec_ext"; break 3] )
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

dnl Python version
AC_MSG_CHECKING([if we're using a Python3 interpreter])
AC_MSG_RESULT([$have_python3])

AC_MSG_CHECKING([$PYTHON version])
PYTHON_VERSION=`$PYTHON -c "import sys; print (sys.version[[:3]])"`
AC_MSG_RESULT([$PYTHON_VERSION])
AC_SUBST([PYTHON_VERSION])

dnl Python ABI version (which can be different than VERISON in python3)
AC_MSG_CHECKING([Python ABI version])
GD_PYTHON_CONFIGVAR([PYTHON_LDVERSION], [LDVERSION])
if test "${PYTHON_LDVERSION}" = "None"; then
  PYTHON_LDVERSION=${PYTHON_VERSION}
fi
AC_MSG_RESULT([$PYTHON_LDVERSION])

dnl calculate Python CPPFLAGS
AC_MSG_CHECKING([Python CPPFLAGS])
python_prefix=`$PYTHON -c "import sys; print (sys.prefix)"`
python_exec_prefix=`$PYTHON -c "import sys; print (sys.exec_prefix)"`
PYTHON_CPPFLAGS="-I${python_prefix}/include/python${PYTHON_LDVERSION}"
if test "x${python_prefix}" != "x${python_exec_prefix}"; then
  PYTHON_CPPFLAGS="${PYTHON_CPPFLAGS} -I${python_exec_prefix}/include/python${PYTHON_LDVERSION}"
fi
AC_MSG_RESULT([$PYTHON_CPPFLAGS])

dnl calculate Python LDFLAGS -- only needed during configure
AC_MSG_CHECKING([Python LDFLAGS])
GD_PYTHON_CONFIGVAR([PYTHON_LIBDIR], [LIBDIR])
PYTHON_LDFLAGS="-L${PYTHON_LIBDIR} -lpython${PYTHON_LDVERSION}"
AC_MSG_RESULT([$PYTHON_LDFLAGS])

dnl figure out the platform name
AC_MSG_CHECKING([Python platform name])
PYTHON_PLATFORM=`$PYTHON -c \
  "from distutils import util; print (util.get_platform())"`
AC_MSG_RESULT([$PYTHON_PLATFORM])
AC_SUBST([PYTHON_PLATFORM])

dnl calculate the extension module directory
dnl
dnl Debian's version of distutils is too clever.  Its install paths change
dnl based on the prefix you provide.  We pass the current best-guess for
dnl ${exec_prefix} as a result.  This may lead to incorrect behaviour at
dnl make install time, if the user changes PREFIX then, but this should do
dnl the right thing in the common case, where prefix doesn't change after
dnl configure-time.
dnl
dnl See the comment under GD_PYTHON_CONFIGVAR for the reason for the
dnl exec call here
AC_MSG_CHECKING([Python extension module directory])
if test "x${local_python_modpath}" = "x"; then

dnl Calculate current exec_prefix
  pyexec_prefix=$exec_prefix
  test "x$pyexec_prefix" = xNONE && pyexec_prefix=$prefix
  test "x$pyexec_prefix" = xNONE && pyexec_prefix=$ac_default_prefix

  prefixed_pythondir=$(exec ${PYTHON} - <<EOF 2>/dev/null
import sys
if sys.version[[:1]] == '3':
  import sysconfig
  print (sysconfig.get_path('platlib', vars={'platbase': "${pyexec_prefix}"}))
else:
  from distutils import sysconfig
  print (sysconfig.get_python_lib(1,0,prefix="${pyexec_prefix}"))
EOF
)
  if test "x${prefixed_pythondir}" = "xNone" -o "x${prefixed_pythondir}" = "x";
  then
    pythondir='\${exec_prefix}/lib/python${PYTHON_LDVERSION}/site-packages'
  else
    esc_pyexec_prefix=$(echo ${pyexec_prefix} | ${SED} -e 's/\//\\\//g')
    pythondir=$(echo ${prefixed_pythondir} | \
      ${SED} -e "s/^${esc_pyexec_prefix}/\${exec_prefix}/")
  fi
else
  pythondir=$local_python_modpath
fi
AC_MSG_RESULT([$pythondir])
AC_SUBST([pythondir])

dnl figure out object suffix
AC_MSG_CHECKING([Python object suffix])
GD_PYTHON_CONFIGVAR([PYTHON_OBJECT_SUFFIX], [SO])
if test "x${PYTHON_OBJECT_SUFFIX}" = "xNone"; then
  GD_PYTHON_CONFIGVAR([PYTHON_OBJECT_SUFFIX], [EXT_SUFFIX])
fi
AC_MSG_RESULT([$PYTHON_OBJECT_SUFFIX])
AC_SUBST([PYTHON_OBJECT_SUFFIX])

if test "$have_python3" = "no"; then
saved_CPPFLAGS=${CPPFLAGS}
CPPFLAGS="${PYTHON_CPPFLAGS} ${CPPFLAGS}"
AC_CHECK_DECLS([Py_USING_UNICODE],[have_pyunicode=yes],
    [have_pyunicode="no (required)"], [
#include <Python.h>
    ])
CPPFLAGS=${saved_CPPFLAGS}
else
have_pyunicode=yes
fi

AC_MSG_CHECKING([for Unicode support in Python])
if test "${have_pyunicode}" != "yes"; then
  have_python=no
fi
AC_MSG_RESULT([$have_pyunicode])

fi
])
