# Copyright (C) 2008-2015 D. V. Wiebe
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
getdata_includedir = ${includedir}/getdata

AM_CPPFLAGS = ${GD_CXX_WALL} $(GD_CXX_WEXTRA) -I$(top_srcdir)/src
LIBS=

SUBDIRS=test

getdata_include_HEADERS = getdata/dirfile.h getdata/entry.h getdata/rawentry.h \
													getdata/lincomentry.h getdata/linterpentry.h \
													getdata/bitentry.h getdata/multiplyentry.h \
													getdata/phaseentry.h getdata/constentry.h \
													getdata/stringentry.h getdata/indexentry.h \
													getdata/sbitentry.h getdata/polynomentry.h \
													getdata/fragment.h getdata/divideentry.h \
													getdata/recipentry.h getdata/carrayentry.h \
													getdata/windowentry.h getdata/mplexentry.h \
													getdata/types.h getdata/sarrayentry.h \
													getdata/indirentry.h getdata/sindirentry.h

lib_LTLIBRARIES=libgetdata++.la
libgetdata___la_SOURCES = dirfile.cpp bitentry.cpp carrayentry.cpp \
													constentry.cpp divideentry.cpp entry.cpp \
													fragment.cpp indirentry.cpp lincomentry.cpp \
													linterpentry.cpp mplexentry.cpp multiplyentry.cpp \
													phaseentry.cpp polynomentry.cpp rawentry.cpp \
													recipentry.cpp sarrayentry.cpp sbitentry.cpp \
													sindirentry.cpp stringentry.cpp windowentry.cpp \
													${getdata_include_HEADERS} internal.h
libgetdata___la_LIBADD=../../src/libgetdata.la
libgetdata___la_LDFLAGS = -version-info @GETDATAXX_VERSION@

clean-local:
	rm -rf *~ getdata/*~
