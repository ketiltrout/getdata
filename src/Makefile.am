# (C) 2008 D. V. Wiebe
#
##########################################################################
#
# This file is part of the GetData project.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# GetData is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with GetData; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
#
AUTOMAKE_OPTIONS = foreign
AM_CFLAGS = -Wall -Wextra @SLIMLIB_CPPFLAGS@
pkgconfigdir = @libdir@/pkgconfig

if USE_SLIMLIB
SLIM_C=slim.c
endif

if INCLUDE_LEGACY_API
LEGACY_C = legacy.c
GETDATA_LEGACY_H = getdata_legacy.h
endif

if GETDATA_DEBUG
DEBUG_C = debug.c
endif

BUILT_SOURCES = getdata.h

nodist_include_HEADERS = getdata.h
include_HEADERS = ${GETDATA_LEGACY_H}

if INSTALL_PKG_CONFIG
pkgconfig_DATA = getdata.pc
endif

lib_LTLIBRARIES = libgetdata.la
libgetdata_la_SOURCES = add.c ascii.c close.c common.c constant.c ${DEBUG_C} \
												entry.c errors.c field_list.c flush.c format_list.c \
												getdata.c ${LEGACY_C} meta_list.c nfields.c nframes.c \
												open.c putdata.c raw.c ${SLIM_C} spf.c string.c \
												types.c ${GETDATA_LEGACY_H} internal.h
libgetdata_la_LDFLAGS = $(SLIMLIB_LDFLAGS) -export-symbols-regex '^[^_]' \
												-version-info @GETDATA_IFACE_VERSION@:@GETDATA_IMPL_REVISION@:@GETDATA_IFACE_AGE@


clean-local:
	rm -rf *~ 
