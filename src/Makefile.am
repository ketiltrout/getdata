# Copyright (C) 2008-2013, 2015, 2016, 2018 D. V. Wiebe
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

if USE_SLIM
if USE_MODULES
LIBGETDATASLIM_LA=libgetdataslim.la
else
SLIM_C=slim.c
endif
endif

if USE_FLAC
if USE_MODULES
LIBGETDATAFLAC_LA=libgetdataflac.la
else
FLAC_C=flac.c
endif
endif

if USE_GZIP
if USE_MODULES
LIBGETDATAGZIP_LA=libgetdatagzip.la
else
GZIP_C=gzip.c
endif
endif

if USE_BZIP2
if USE_MODULES
LIBGETDATABZIP2_LA=libgetdatabzip2.la
else
BZIP2_C=bzip.c
endif
endif

if USE_LZMA
if USE_MODULES
LIBGETDATALZMA_LA=libgetdatalzma.la
else
LZMA_C=lzma.c
endif
endif

if USE_ZZIP
if USE_MODULES
LIBGETDATAZZIP_LA=libgetdatazzip.la
else
ZZIP_C=zzip.c
endif
endif

if USE_ZZSLIM
if USE_MODULES
LIBGETDATAZZSLIM_LA=libgetdatazzslim.la
else
ZZSLIM_C=zzslim.c
endif
endif

MODULE_LIBS=libgetdata.la
if USE_MODULES
EXPORT_DYNAMIC=-export-dynamic
DGETDATA_MODULEDIR=-DGETDATA_MODULEDIR="\"$(moduledir)\""
else
EXTERNAL_CPPFLAGS=$(SLIM_CPPFLAGS) $(GZIP_CPPFLAGS) $(BZIP2_CPPFLAGS) \
									$(LZMA_CPPFLAGS) $(ZZIP_CPPFLAGS) $(FLAC_CPPFLAGS)
EXTERNAL_LDFLAGS=$(SLIM_LDFLAGS) $(GZIP_LDFLAGS) $(BZIP2_LDFLAGS) \
								 $(LZMA_LDFLAGS) $(ZZIP_LDFLAGS) $(FLAC_LDFLAGS)
EXTERNAL_LIBS=$(SLIM_LIBS) $(GZIP_LIBS) $(BZIP2_LIBS) $(LZMA_LIBS) \
							$(ZZIP_LIBS) $(FLAC_LIBS)
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
AM_CFLAGS = ${GD_CC_WALL} ${GD_CC_WEXTRA} $(DGETDATA_MODULEDIR)
AM_CPPFLAGS = $(LTDLINCL) $(EXTERNAL_CPPFLAGS)

BUILT_SOURCES = getdata.h

nodist_include_HEADERS = getdata.h
include_HEADERS = ${GETDATA_LEGACY_H}
pkgconfig_DATA = getdata.pc

lib_LTLIBRARIES = libgetdata.la
module_LTLIBRARIES = ${LIBGETDATABZIP2_LA} ${LIBGETDATAFLAC_LA} \
										 ${LIBGETDATAGZIP_LA} ${LIBGETDATALZMA_LA} \
										 ${LIBGETDATASLIM_LA} ${LIBGETDATAZZIP_LA} \
										 ${LIBGETDATAZZSLIM_LA}
libgetdata_la_SOURCES = add.c ascii.c ${BZIP2_C} close.c common.c compat.c \
												constant.c ${DEBUG_C} del.c encoding.c endian.c \
												entry.c errors.c field_list.c ${FLAC_C} flimits.c \
												flush.c fragment.c getdata.c globals.c ${GZIP_C} \
												index.c include.c iopos.c ${LEGACY_C} ${LZMA_C} \
												mod.c move.c name.c native.c nfields.c nframes.c \
												open.c parse.c protect.c putdata.c raw.c sie.c \
												${SLIM_C} spf.c string.c types.c ${ZZIP_C} ${ZZSLIM_C} \
												${GETDATA_LEGACY_H} gd_extra_config.h internal.h
libgetdata_la_LDFLAGS = $(EXPORT_DYNAMIC) -export-symbols-regex '^[^_]' \
												-version-info \
												${GETDATA_IFACE_VERSION}:${GETDATA_IMPL_REVISION}:${GETDATA_IFACE_AGE} \
												$(EXTERNAL_LDFLAGS)
libgetdata_la_LIBADD = $(LIBLTDL) $(EXTERNAL_LIBS)

libgetdataslim_la_SOURCES = slim.c
libgetdataslim_la_LDFLAGS = $(SLIM_LDFLAGS) -release $(GETDATA_LIB_VERSION) \
														-module
libgetdataslim_la_LIBADD = $(SLIM_LIBS) $(MODULE_LIBS)
libgetdataslim_la_CPPFLAGS = $(AM_CPPFLAGS) $(SLIM_CPPFLAGS)

libgetdataflac_la_SOURCES = flac.c
libgetdataflac_la_LDFLAGS = $(FLAC_LDFLAGS) -release $(GETDATA_LIB_VERSION) \
														-module
libgetdataflac_la_LIBADD = $(FLAC_LIBS) $(MODULE_LIBS)
libgetdataflac_la_CPPFLAGS = $(AM_CPPFLAGS) $(FLAC_CPPFLAGS)

libgetdatagzip_la_SOURCES = gzip.c
libgetdatagzip_la_LDFLAGS = $(GZIP_LDFLAGS) -release $(GETDATA_LIB_VERSION) \
														-module
libgetdatagzip_la_LIBADD = $(GZIP_LIBS) $(MODULE_LIBS)
libgetdatagzip_la_CPPFLAGS = $(AM_CPPFLAGS) $(GZIP_CPPFLAGS)

libgetdatabzip2_la_SOURCES = bzip.c
libgetdatabzip2_la_LDFLAGS = $(BZIP2_LDFLAGS) -release $(GETDATA_LIB_VERSION) \
														 -module
libgetdatabzip2_la_LIBADD = $(BZIP2_LIBS) $(MODULE_LIBS)
libgetdatabzip2_la_CPPFLAGS = $(AM_CPPFLAGS) $(BZIP2_CPPFLAGS)

libgetdatalzma_la_SOURCES = lzma.c
libgetdatalzma_la_LDFLAGS = $(LZMA_LDFLAGS) -release $(GETDATA_LIB_VERSION) \
														 -module
libgetdatalzma_la_LIBADD = $(LZMA_LIBS) $(MODULE_LIBS)
libgetdatalzma_la_CPPFLAGS = $(AM_CPPFLAGS) $(LZMA_CPPFLAGS)

libgetdatazzip_la_SOURCES = zzip.c
libgetdatazzip_la_LDFLAGS = $(ZZIP_LDFLAGS) -release $(GETDATA_LIB_VERSION) \
														 -module
libgetdatazzip_la_LIBADD = $(ZZIP_LIBS) $(MODULE_LIBS)
libgetdatazzip_la_CPPFLAGS = $(AM_CPPFLAGS) $(ZZIP_CPPFLAGS)

libgetdatazzslim_la_SOURCES = zzslim.c
libgetdatazzslim_la_LDFLAGS = $(SLIM_LDFLAGS) $(ZZIP_LDFLAGS) \
														-release $(GETDATA_LIB_VERSION) -module
libgetdatazzslim_la_LIBADD = $(SLIM_LIBS) $(ZZIP_LIBS) $(MODULE_LIBS)
libgetdatazzslim_la_CPPFLAGS = $(AM_CPPFLAGS) $(SLIM_CPPFLAGS) $(ZZIP_CPPFLAGS)

# Handle MacOS >=10.11 embedding install paths in .so files
if USE_INSTALL_NAME_TOOL
if USE_MODULES
nodist_noinst_DATA = libpath_mangled

# Change, say /usr/local/lib/libgetdata.#.dylib, into .libs/libgetdata.#.dylib
libpath_mangled: ${nodist_python_SCRIPTS}
	for m in ${module_LTLIBRARIES}; do \
		${INSTALL_NAME_TOOL} -change \
		${libdir}/libgetdata.${GETDATA_IFACE_VERSION}.dylib \
		${abs_top_builddir}/src/.libs/libgetdata.${GETDATA_IFACE_VERSION}.dylib \
		.libs/`basename $$m .la`-${GETDATA_LIB_VERSION}.so; \
		done
	touch $@

# Change mangled path into install path again
install-data-hook:
	for m in ${module_LTLIBRARIES}; do \
		${INSTALL_NAME_TOOL} -change \
		${abs_top_builddir}/src/.libs/libgetdata.${GETDATA_IFACE_VERSION}.dylib \
		${libdir}/libgetdata.${GETDATA_IFACE_VERSION}.dylib \
		${DESTDIR}${moduledir}/`basename $$m .la`-${GETDATA_LIB_VERSION}.so; \
		done
endif
endif

clean-local:
	rm -rf *~ getdata.pc *.gcno ${nodist_noinst_DATA}
