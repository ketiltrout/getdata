# Copyright (C) 2008-2011, 2015 D. V. Wiebe
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
AUTOMAKE_OPTIONS = foreign dist-xz check-news 1.13

ACLOCAL_AMFLAGS = -I m4

BUILDCC=@BUILDCC@

noinst_PROGRAMS=make_parameters

make_parameters_SOURCES = make_parameters.c

SUBDIRS = . src

EXTRA_DIST = ChangeLog README.matlab

make_parameters$(EXEEXT): $(make_parameters_SOURCES)
	${BUILDCC} $(DEFS) $(GETDATA_CFLAGS) -Isrc -o make_parameters$(EXEEXT) $(srcdir)/$(make_parameters_SOURCES)

clean-local:
	rm -rf *~ 
