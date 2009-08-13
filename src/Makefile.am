# (C) 2008-2009 D. V. Wiebe
#
##########################################################################
#
# This file is part of the GetData project.
#
# GetData is free software; you can redistribute it and/or modify it under
# the terms of the GNU Lesser General Public License as published by the
# Free Software Foundation; either version 2.1 of the License, or (at your
# option) any later version.
#
# GetData is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
# License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with GetData; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
#
AUTOMAKE_OPTIONS = foreign

if GNU_C_COMPILER
WEXTRA=-Wextra
endif

if USE_SLIM
if USE_MODULES
LIBGETDATASLIM_LA=libgetdataslim.la
DLOPEN_LIBGETDATASLIM_LA=-dlopen libgetdataslim.la
else
SLIM_C=slim.c
endif
endif

if USE_GZIP
if USE_MODULES
LIBGETDATAGZIP_LA=libgetdatagzip.la
DLOPEN_LIBGETDATAGZIP_LA=-dlopen libgetdatagzip.la
else
GZIP_C=gzip.c
endif
endif

if USE_BZIP2
if USE_MODULES
LIBGETDATABZIP2_LA=libgetdatabzip2.la
DLOPEN_LIBGETDATABZIP2_LA=-dlopen libgetdatabzip2.la
else
BZIP2_C=bzip.c
endif
endif

if USE_LZMA
if USE_MODULES
LIBGETDATALZMA_LA=libgetdatalzma.la
DLOPEN_LIBGETDATALZMA_LA=-dlopen libgetdatalzma.la
else
LZMA_C=lzma.c
endif
endif

if USE_MODULES
EXPORT_DYNAMIC=-export-dynamic
if INTERNAL_LTDL
GD_LTDL=gd_ltdl.c gd_ltdl.h
endif
else
EXTERNAL_CPPFLAGS=$(SLIM_CPPFLAGS) $(GZIP_CPPFLAGS) $(BZIP2_CPPFLAGS) \
									$(LZMA_CPPFLAGS)
EXTERNAL_LDFLAGS=$(SLIM_LDFLAGS) $(GZIP_LDFLAGS) $(BZIP2_LDFLAGS) \
								 $(LZMA_LDFLAGS)
endif

if INCLUDE_LEGACY_API
LEGACY_C = legacy.c
GETDATA_LEGACY_H = getdata_legacy.h

install-data-hook:
	cd $(DESTDIR)$(includedir) && \
		$(LN_S) -f getdata.h getdata_struct.h

uninstall-hook:
	cd $(DESTDIR)$(includedir) && rm -rf getdata_struct.h
endif

if GETDATA_DEBUG
DEBUG_C = debug.c
endif

pkgconfigdir = @libdir@/pkgconfig
AM_CFLAGS = -Wall ${WEXTRA} 
AM_CPPFLAGS = $(LTDLINCL) $(EXTERNAL_CPPFLAGS)

BUILT_SOURCES = getdata.h

nodist_include_HEADERS = getdata.h
include_HEADERS = ${GETDATA_LEGACY_H}
pkgconfig_DATA = getdata.pc

lib_LTLIBRARIES = libgetdata.la ${LIBGETDATASLIM_LA} ${LIBGETDATAGZIP_LA} \
									${LIBGETDATABZIP2_LA} ${LIBGETDATALZMA_LA}
libgetdata_la_SOURCES = add.c ascii.c ${BZIP2_C} close.c common.c constant.c \
												${DEBUG_C} del.c encoding.c endian.c entry.c errors.c \
												field_list.c flush.c foffs.c fragment.c getdata.c \
												globals.c ${GZIP_C} include.c ${LEGACY_C} ${LZMA_C} \
												meta_list.c mod.c move.c name.c nfields.c nframes.c \
												nmeta.c open.c parse.c protect.c putdata.c raw.c \
												${SLIM_C} spf.c string.c types.c ${GD_LTDL} \
												${GETDATA_LEGACY_H} internal.h
libgetdata_la_LDFLAGS = $(EXPORT_DYNAMIC) -export-symbols-regex '^[^_]' \
												-version-info \
												@GETDATA_IFACE_VERSION@:@GETDATA_IMPL_REVISION@:@GETDATA_IFACE_AGE@ \
												$(EXTERNAL_LDFLAGS)
libgetdata_la_LIBADD = $(LIBLTDL) $(DLOPEN_LIBGETDATASLIM_LA) \
											 $(DLOPEN_LIBGETDATAGZIP_LA) \
											 $(DLOPEN_LIBGETDATABZIP2_LA) $(DLOPEN_LIBGETDATALZMA_LA)

libgetdataslim_la_SOURCES = slim.c ${DEBUG_C}
libgetdataslim_la_LDFLAGS = $(SLIM_LDFLAGS) -release $(PACKAGE_VERSION) \
														-module
libgetdataslim_la_CPPFLAGS = $(AM_CPPFLAGS) $(SLIM_CPPFLAGS)

libgetdatagzip_la_SOURCES = gzip.c ${DEBUG_C}
libgetdatagzip_la_LDFLAGS = $(GZIP_LDFLAGS) -release $(PACKAGE_VERSION) -module
libgetdatagzip_la_CPPFLAGS = $(AM_CPPFLAGS) $(GZIP_CPPFLAGS)

libgetdatabzip2_la_SOURCES = bzip.c ${DEBUG_C}
libgetdatabzip2_la_LDFLAGS = $(BZIP2_LDFLAGS) -release $(PACKAGE_VERSION) \
														 -module
libgetdatabzip2_la_CPPFLAGS = $(AM_CPPFLAGS) $(BZIP2_CPPFLAGS)

libgetdatalzma_la_SOURCES = lzma.c ${DEBUG_C}
libgetdatalzma_la_LDFLAGS = $(LZMA_LDFLAGS) -release $(PACKAGE_VERSION) \
														 -module
libgetdatalzma_la_CPPFLAGS = $(AM_CPPFLAGS) $(LZMA_CPPFLAGS)

clean-local:
	rm -rf *~ 
