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
# GetData is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License along
# with GetData; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
#
AUTOMAKE_OPTIONS = foreign
getdata_includedir = ${includedir}/getdata

if GNU_CXX_COMPILER
WEXTRA=-Wextra
endif

INCLUDES = -Wall $(WEXTRA) -I$(top_srcdir)/src

SUBDIRS=test

getdata_include_HEADERS = dirfile.h entry.h

lib_LTLIBRARIES=libgetdata++.la
libgetdata___la_SOURCES = dirfile.cpp entry.cpp \
												${getdata_include_HEADERS}
libgetdata___la_LIBADD=../../src/libgetdata.la
libgetdata___la_LDFLAGS = -version-info @GETDATAXX_VERSION@

clean-local:
	rm -rvf *~
