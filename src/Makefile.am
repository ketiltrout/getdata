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

if GNU_C_COMPILER
WEXTRA=-Wextra
endif

if USE_SLIMLIB
LIBGETDATASLIM_LA=libgetdataslim.la
endif

if USE_ZLIB
LIBGETDATAZLIB_LA=libgetdatagzip.la
endif

if INCLUDE_LEGACY_API
LEGACY_C = legacy.c
GETDATA_LEGACY_H = getdata_legacy.h

install-data-hook:
	cd $(DESTDIR)$(includedir) && \
		$(LN_S) -f getdata.h getdata_struct.h
endif

if GETDATA_DEBUG
DEBUG_C = debug.c
endif

pkgconfigdir = @libdir@/pkgconfig
AM_CFLAGS = -Wall ${WEXTRA}

BUILT_SOURCES = getdata.h

nodist_include_HEADERS = getdata.h
include_HEADERS = ${GETDATA_LEGACY_H}

if INSTALL_PKG_CONFIG
pkgconfig_DATA = getdata.pc
endif

lib_LTLIBRARIES = libgetdata.la ${LIBGETDATASLIM_LA} ${LIBGETDATAZLIB_LA}
libgetdata_la_SOURCES = add.c ascii.c close.c common.c constant.c ${DEBUG_C} \
												del.c encoding.c endian.c entry.c errors.c \
												field_list.c flush.c foffs.c format_list.c getdata.c \
												globals.c include.c ${LEGACY_C} meta_list.c mod.c \
												move.c nfields.c nframes.c nmeta.c open.c putdata.c \
												raw.c spf.c string.c types.c ${GETDATA_LEGACY_H} \
												internal.h
libgetdata_la_LDFLAGS = -export-symbols-regex '^[^_]' \
												-version-info @GETDATA_IFACE_VERSION@:@GETDATA_IMPL_REVISION@:@GETDATA_IFACE_AGE@

libgetdataslim_la_SOURCES = slim.c ${DEBUG_C}
libgetdataslim_la_LDFLAGS = $(SLIMLIB_LDFLAGS) -release $(PACKAGE_VERSION)
libgetdataslim_la_CPPFLAGS = $(AM_CPPFLAGS) $(SLIMLIB_CPPFLAGS)

libgetdatagzip_la_SOURCES = gzip.c ${DEBUG_C}
libgetdatagzip_la_LDFLAGS = $(ZLIB_LDFLAGS) -release $(PACKAGE_VERSION)
libgetdatagzip_la_CPPFLAGS = $(AM_CPPFLAGS) $(ZLIB_CPPFLAGS)

clean-local:
	rm -rf *~ 
