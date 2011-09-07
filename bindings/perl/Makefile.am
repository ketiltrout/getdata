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

EXTRA_DIST=funclist.pl GetData.xs simple_funcs.pl simple_funcs.xsin typemap \
					 GetData.pm.in MANIFEST

if GETDATA_DEBUG
DEBUG_C = debug.c
TEST_VERBOSE="TEST_VERBOSE=1"
debug.c: $(top_srcdir)/src/debug.c
	echo "#undef VERSION" > $@
	cat $< >> $@
endif

lib:
	mkdir lib

lib/typemap: $(srcdir)/typemap lib
	cat $< > $@

lib/simple_funcs.xs: $(srcdir)/simple_funcs.xsin $(srcdir)/simple_funcs.pl lib
	$(PERL) $(srcdir)/simple_funcs.pl $< > $@

make_parameters.sed: ../make_parameters ../../src/getdata.h
	  ../make_parameters P > $@

../make_parameters: $(srcdir)/../make_parameters.c
	  cd .. && ${MAKE} make_parameters

GetData.pm: $(srcdir)/GetData.pm.in make_parameters.sed $(srcdir)/funclist.pl \
						$(srcdir)/GetData.xs lib/simple_funcs.xs
	rm -f $@
	$(SED) -f make_parameters.sed $< | \
		$(PERL) $(srcdir)/funclist.pl $(srcdir)/GetData.xs lib/simple_funcs.xs > $@
	chmod a-w $@

all-local: blib/arch/auto/GetData/GetData.so

blib/arch/auto/GetData/GetData.so: Build
	$(PERL) $< build

.PRECIOUS: Build
Build: Build.PL GetData.pm GetData.xs lib/simple_funcs.xs lib/typemap $(DEBUG_C)
	perl $<

clean-local: 
	rm -rf lib
	if test -e Build; then \
		$(PERL) Build clean; \
	fi
	rm -f $(BUILT_SOURCES) make_parameters.sed debug.c MYMETA.yml MYMETA.json GetData.pm
	rm -rf Build _build

if TEST_PERL
check:
	$(MAKE) test

test testdb testcover:
	${DL_LIBRARY_PATH}=${${DL_LIBRARY_PATH}}:../../src/.libs $(PERL) ./Build $@

.PHONY: test testdb testcover
endif
