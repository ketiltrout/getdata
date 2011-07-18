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

simple_funcs.xs: $(srcdir)/simple_funcs.xsin $(srcdir)/simple_funcs.pl
	$(PERL) simple_funcs.pl $< > $@

make_parameters.sed: ../make_parameters ../../src/getdata.h
	  ../make_parameters P > $@

../make_parameters: $(srcdir)/../make_parameters.c
	  cd .. && ${MAKE} make_parameters

GetData.pm: $(srcdir)/GetData.pm.in make_parameters.sed funclist.pl \
						GetData.xs simple_funcs.xs
	rm -f $@
	$(SED) -f make_parameters.sed $< | \
		$(PERL) funclist.pl GetData.xs simple_funcs.xs > $@
	chmod a-w $@

all-local: perl.mk
	$(MAKE) -f perl.mk

perl.mk: Makefile.PL GetData.pm GetData.xs simple_funcs.xs typemap
	perl $<

clean-local: 
	$(MAKE) -f perl.mk clean
	rm -f $(BUILT_SOURCES) make_parameters.sed perl.mk.old

test testdb:
	$(MAKE) -f perl.mk $@

.PHONY: test
