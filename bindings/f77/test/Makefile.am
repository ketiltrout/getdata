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
FFLAGS += -I$(srcdir)/..
FCFLAGS += -I$(srcdir)/..
F95LDADD=../libf95getdata.la $(F77LDADD)
F77LDADD=../libfgetdata.la

if MAKE_F95BINDINGS
F95TESTS=fgetdata fget_field_list
endif

TESTS=gdfget $(F95TESTS)

gdfget_SOURCES=gdfget.f
gdfget_LDADD=$(F77LDADD)

fgetdata_SOURCES=fgetdata.f95
fgetdata_LDADD=$(F95LDADD)

fget_field_list_SOURCES=fget_field_list.f95
fget_field_list_LDADD=$(F95LDADD)

check_PROGRAMS=$(TESTS)

# libtool 1.5.x hack: treat FC stuff like F77
LTFCCOMPILE = $(LIBTOOL) --tag=F77 $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) \
							--mode=compile $(FC) $(AM_FCFLAGS) $(FCFLAGS)
FCLINK = $(LIBTOOL) --tag=F77 --mode=link $(FCLD) $(AM_FCFLAGS) $(FCFLAGS) \
				 $(AM_LDFLAGS) $(LDFLAGS) -o $@

clean-local:
	rm -rf test_dirfile
