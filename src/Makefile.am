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
# You should have received a copy of the GNU General Public
# License along with GetData; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
# 02111-1307 USA.
#
AUTOMAKE_OPTIONS = foreign
INCLUDES = -Wall

if INCLUDE_LEGACY_API
LEGACY_C = legacy.c
GETDATA_LEGACY_H = getdata_legacy.h
endif

BUILT_SOURCES = getdata.h

nodist_include_HEADERS = getdata.h
include_HEADERS = ${GETDATA_LEGACY_H}

lib_LTLIBRARIES = libgetdata.la
libgetdata_la_SOURCES = getdata.c putdata.c dirfile_open.c errors.c nframes.c \
												spf.c ${LEGACY_C} ${GETDATA_LEGACY_H} getdata_internal.h
libgetdata_la_LDFLAGS = -version-info @GETDATA_IFACE_VERSION@:@GETDATA_IMPL_REVISION@:@GETDATA_IFACE_AGE@

