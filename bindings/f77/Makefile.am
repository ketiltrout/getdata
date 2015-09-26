# Copyright (C) 2008-2014 D. V. Wiebe
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

SED = @SED@
DIFF = @DIFF@

if MAKE_F95BINDINGS
F95_INC=getdata.mod
F95_LIB=libf95getdata.la
endif

FCFLAGS += $(GD_FC_WALL) $(GD_FC_WEXTRA)
EXTRA_DIST=getdata.f.in getdata.f90.in

SUBDIRS = test

BUILT_SOURCES=getdata.f $(F95_INC)
LIBS=

nodist_include_HEADERS=getdata.f $(F95_INC)

lib_LTLIBRARIES=libfgetdata.la $(F95_LIB)
libfgetdata_la_CPPFLAGS = ${GD_CC_WALL} $(GD_CC_WEXTRA) -I$(top_srcdir)/src
libfgetdata_la_SOURCES = fgetdata.c fgetdata.h
libfgetdata_la_LIBADD=../../src/libgetdata.la
libfgetdata_la_LDFLAGS = -version-info @FGETDATA_VERSION@

nodist_libf95getdata_la_SOURCES=getdata.f90
libf95getdata_la_LIBADD=libfgetdata.la
libf95getdata_la_LDFLAGS = -version-info @F95GETDATA_VERSION@

getdata.mod: getdata.o

clean-local:
	rm -rf getdata.mod make_parameters.sed.in make_parameters.stamp \
		make_parameters.sed getdata.f getdata.f90 getdata.mod
	rm -rf *~

make_parameters.sed.in: ../make_parameters ../../src/getdata.h
	../make_parameters f > $@

../make_parameters: ../make_parameters.c
	cd .. && ${MAKE} make_parameters

make_parameters.sed: make_parameters.stamp
	@if test ! -f $@; then \
		rm -f make_parameters.stamp; \
		$(MAKE) make_parameters.stamp; \
		fi

if HAVE_DIFF

make_parameters.stamp: make_parameters.sed.in
	@if $(DIFF) make_parameters.sed make_parameters.sed.in >/dev/null 2>&1; then \
		echo "make_parameters.sed is unchanged"; \
		else \
		rm -f make_parameters.sed; \
		cp make_parameters.sed.in make_parameters.sed; \
		fi; \
		touch make_parameters.stamp

else

make_parameters.stamp: make_parameters.sed.in
		@rm -f make_parameters.sed; \
		cp make_parameters.sed.in make_parameters.sed; \
		touch make_parameters.stamp

endif

getdata.f: $(srcdir)/getdata.f.in make_parameters.sed
	rm -f $@
	$(SED) -f make_parameters.sed $(srcdir)/getdata.f.in > $@
	chmod a-w $@

getdata.f90: $(srcdir)/getdata.f90.in make_parameters.sed
	rm -f $@
	$(SED) -f make_parameters.sed $(srcdir)/getdata.f90.in > $@
	chmod a-w $@
