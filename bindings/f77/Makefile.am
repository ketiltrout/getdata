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

SED = @SED@
DIFF = @DIFF@

if MAKE_F95BINDINGS
F95_INC=getdata.mod
F95_LIB=libf95getdata.la
endif

SUBDIRS = test

BUILT_SOURCES=getdata.f $(F95_INC)

noinst_PROGRAMS=make_parameters

nodist_include_HEADERS=getdata.f $(F95_INC)

lib_LTLIBRARIES=libfgetdata.la $(F95_LIB)
libfgetdata_la_SOURCES = fgetdata.c fgetdata.h
libfgetdata_la_LIBADD=../../src/libgetdata.la
libfgetdata_la_LDFLAGS = -version-info @FGETDATA_VERSION@

nodist_libf95getdata_la_SOURCES=getdata.f95
libf95getdata_la_LIBADD=libfgetdata.la ../../src/libgetdata.la
libf95getdata_la_LDFLAGS = -version-info @F95GETDATA_VERSION@

# libtool 1.5.x hack: treat FC stuff like F77
LTFCCOMPILE = $(LIBTOOL) --tag=F77 $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) \
							--mode=compile $(FC) $(AM_FCFLAGS) $(FCFLAGS)
FCLINK = $(LIBTOOL) --tag=F77 --mode=link $(FCLD) $(AM_FCFLAGS) $(FCFLAGS) \
				 $(AM_LDFLAGS) $(LDFLAGS) -o $@

getdata.mod: getdata.o

clean-local:
	rm -rf getdata.mod make_parameters.sed.in make_parameters.stamp \
		make_parameters.sed getdata.f getdata.f95

make_parameters.sed.in: make_parameters ../../src/getdata.h
	./make_parameters

make_parameters.sed: make_parameters.stamp
	@if test ! -f $@; then \
		rm -f $<; \
		$(MAKE) $<; \
		fi

make_parameters.stamp: make_parameters.sed.in
	@if $(DIFF) make_parameters.sed make_parameters.sed.in >/dev/null 2>&1; then \
		echo "make_parameters.sed is unchanged"; \
		else \
		rm -f make_parameters.sed; \
		cp make_parameters.sed.in make_parameters.sed; \
		fi; \
		touch make_parameters.stamp

getdata.f: $(srcdir)/getdata.f.in make_parameters.sed
	rm -f $@
	$(SED) $< -f make_parameters.sed > $@
	chmod a-w $@

getdata.f95: $(srcdir)/getdata.f95.in make_parameters.sed
	rm -f $@
	$(SED) $< -f make_parameters.sed > $@
	chmod a-w $@
