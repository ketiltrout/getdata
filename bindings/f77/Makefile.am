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
# You should have received a copy of the GNU General Public License
# along with the GetData; if not, write to the Free Software Foundation,
# Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
#
AUTOMAKE_OPTIONS = foreign
INCLUDES = -Wall -I$(top_srcdir)/src

SUBDIRS = test

include_HEADERS=getdata.f

lib_LTLIBRARIES=libfgetdata.la
libfgetdata_la_SOURCES = fgetdata.c fgetdata.h
libfgetdata_la_LIBADD=../../src/libgetdata.la
libfgetdata_la_LDFLAGS = -version-info @FGETDATA_IFACE_VERSION@:@FGETDATA_IMPL_REVISION@:@FGETDATA_IFACE_AGE@
