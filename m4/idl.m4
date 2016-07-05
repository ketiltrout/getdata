dnl Copyright (C) 2009, 2011, 2012, 2013, 2016 D. V. Wiebe
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

dnl GD_IDL_VAR(NAME, IDLVAR, [SEDEXPR])
dnl ---------------------------------------------------------
dnl Assign the value of IDL variable IDLVAR to the variable NAME after
dnl optionally processing it with the sed script SEDEXPR
AC_DEFUN([GD_IDL_VAR],
[
gd_idl_var=$(echo 'print,"@@@"+$2' | $IDL 2>/dev/null | \
      grep '@@@' | sed -e 's/^@@@//')
ifelse(`$#', `2', [$1=${gd_idl_var}],
  [$1=$(echo $gd_idl_var | sed -e '$3')])
])

dnl GD_IDL_CHECK_VERSION
dnl ---------------------------------------------------------------
dnl Define IDL_VERSION to the version of the idl interpreter
AC_DEFUN([GD_IDL_CHECK_VERSION],
[
idl_version_ok="no"
idl_header=`( echo | $1 2>&1 )` #''
IDL_VERSION="unknown"
if echo $idl_header | grep "IDL Version" >/dev/null 2>/dev/null; then
  IDL_VERSION=$(echo $idl_header | grep Version | \
    sed -e 's/IDL Version \(@<:@^ @:>@*\).*/\1/')
  AX_COMPARE_VERSION([$IDL_VERSION],[ge],[$2],[idl_version_ok="yes"])
fi
if test "x$idl_version_ok" = "xyes"; then
  $3
  true
else
  $4
  true
fi
])

dnl GD_IDL
dnl ---------------------------------------------------------------
dnl Look for idl.  Then determine whether we can build an IDL module.
AC_DEFUN([GD_IDL],
[
idl_min_version=$1
AC_ARG_WITH([idl], AS_HELP_STRING([--with-idl=PATH],
            [use the IDL interpreter located in PATH [default: autodetect]]),
            [
              case "${withval}" in
                no) have_idl="no" ;;
                yes) user_idl= ; have_idl= ;;
                *) user_idl="${withval}"; have_idl= ;;
              esac
            ], [ user_idl=; have_idl= ])


AC_ARG_WITH([idl-dlm-dir], AS_HELP_STRING([--with-idl-dlm-dir=PATH],
      [install the IDL bindings into PATH [default: autodetect]]),
      [
        case "${withval}" in
          no) local_idl_dlm_path= ;;
          *) local_idl_dlm_path="${withval}"
        esac
      ], [ local_idl_dlm_path= ])

if test "x${have_idl}" != "xno"; then

dnl try to find a sufficiently new IDL.
if test "x$user_idl" != "x"; then
  AC_MSG_CHECKING([whether $user_idl is an IDL interpreter version >= ]
      [$idl_min_version])
  GD_IDL_CHECK_VERSION([$user_idl], [$idl_min_version],
  [AC_MSG_RESULT([yes])
  IDL=$user_idl],
  [AC_MSG_RESULT([no])
  IDL="not found"])
else
  AC_MSG_CHECKING([for an IDL interpreter version >= $idl_min_version])
  IDL="not found"
  for prog in idl idl7 idl6 idl5; do
  _AS_PATH_WALK([$PATH],
  [for exec_ext in '' $ac_executable_extensions; do
    if AS_EXECUTABLE_P(["$as_dir/$prog$exec_ext"]); then
      GD_IDL_CHECK_VERSION( ["$as_dir/$prog$exec_ext"],
      [$idl_min_version], [ IDL="$as_dir/$prog$exec_ext"; break 3] )
    fi
  done])
  done
  AC_MSG_RESULT([$IDL])
fi

if test "x$IDL" = "xnot found"; then
  have_idl="no"
  IDL=
fi
AC_SUBST([IDL])

fi

if test "x${have_idl}" != "xno"; then
dnl idl version
AC_MSG_CHECKING([$IDL version])
AC_MSG_RESULT([$IDL_VERSION])

dnl IDL prefix
AC_MSG_CHECKING([the IDL prefix])
GD_IDL_VAR([idl_prefix], [!DIR])
AC_MSG_RESULT([${idl_prefix}])
AC_SUBST([idl_prefix])

dnl calculate idl CPPFLAGS and LIBS
AC_MSG_CHECKING([for $IDL DLM directory])
if test "x${local_idl_dlm_path}" = "x"; then
  GD_IDL_VAR([prefixed_idldir], [!DLM_PATH], [s/^\(@<:@^:@:>@*\)/\1/])

  esc_idlprefix=$(echo ${idl_prefix} | sed -e 's/\//\\\//g')

  dnl if ${idl_prefix} isn't in contained in ${exec_prefix}, then replace
  dnl idl_prefix with exec_prefix in the DLM path, so that IDL is installed
  dnl under ${exec_prefix}.

  dnl We really should do this at build time, I suppose...
  config_eprefix=${exec_prefix}
  if test "x${exec_prefix}" = "xNONE"; then
    if test "x${prefix}" = "xNONE"; then
      config_prefix=$ac_default_prefix
    else
      config_prefix=${prefix}
    fi
  else
    config_prefix=${exec_prefix}
  fi

  if echo $prefixed_idldir | \
    grep "^$config_prefix" >/dev/null 2>/dev/null; then
    idldir=$(echo ${prefixed_idldir} | \
        sed -e "s/^${esc_idlprefix}/\${idl_prefix}/")
  else
    idldir=$(echo ${prefixed_idldir} | \
        sed -e "s/^${esc_idlprefix}/\${exec_prefix}/")
  fi
else
  idldir="$local_idl_dlm_path"
fi
AC_MSG_RESULT([$idldir])
AC_SUBST([idldir])

AC_MSG_CHECKING([IDL compiler flags])
GD_IDL_VAR([IDL_CFLAGS],[!MAKE_DLL.CC],
    [s/^.*%X \(.*\) %C.*/\1/; s/\(.* \)-c\(.*\)/\1\2/; s/"//g])
AC_MSG_RESULT([$IDL_CFLAGS])
AC_SUBST([IDL_CFLAGS])

AC_MSG_CHECKING([IDL linker flags])
GD_IDL_VAR([IDL_LIBS],[!MAKE_DLL.LD],
    [s/^@<:@^ @:>@* \(.*\?\) -o.*/\1/; s/-m \?[\w]*//g])
AC_MSG_RESULT([$IDL_LIBS])
AC_SUBST([IDL_LIBS])

dnl header check
saved_CPPFLAGS=${CPPFLAGS}
CPPFLAGS="${CPPFLAGS} ${IDL_CFLAGS}"
AC_CHECK_HEADERS(idl_export.h,,[have_idl="no"])
CPPFLAGS=${saved_CPPFLAGS}

fi
])
