# Copyright (C) 2011 D. V. Wiebe
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

if CC_WALL
WALL=-Wall
endif

if GETDATA_DEBUG
DEBUG_C = ../../src/debug.c
endif

SUBDIRS=test

perl_LTLIBRARIES=GetData.la
perl_SCRIPTS=GetData.pm

BUILT_SOURCES=simple_funcs.xs GetData.pm GetData.c
AM_CFLAGS=${WALL} ${PERL_CFLAGS}
AM_CPPFLAGS=${PERL_CPPFLAGS}

GetData_la_LDFLAGS = -module -avoid-version
GetData_la_LIBADD = ${PERL_LDLFLAGS} ../../src/libgetdata.la
nodist_GetData_la_SOURCES = GetData.c ${DEBUG_C}

EXTRA_DIST=GetData.xs

simple_funcs.xs: $(srcdir)/simple_funcs.xsin $(srcdir)/simple_funcs.pl
	$(PERL) simple_funcs.pl $< > $@

funclist.sed: GetData.c
	$(PERL) -ne 'BEGIN { print "s/\@FUNCLIST@/" }' \
	-e 'print "$$1 " if /^XS\(XS_GetData_([a-z].*)\);/;' \
	-e 'END { print "/\n" }' $< > $@

make_parameters.sed: ../make_parameters ../../src/getdata.h
	  ../make_parameters P > $@

../make_parameters: $(srcdir)/../make_parameters.c
	  cd .. && ${MAKE} make_parameters

GetData.pm: $(srcdir)/GetData.pm.in make_parameters.sed funclist.sed
	rm -f $@
	$(SED) -f make_parameters.sed $< | $(SED) -f funclist.sed > $@
	chmod a-w $@

GetData.c: GetData.xs simple_funcs.xs typemap
	$(XSUBPP) -typemap $(srcdir)/typemap $< > $@

clean-local:
	rm -f $(BUILT_SOURCES) make_parameters.sed funclist.sed
