# Copyright (C) 2011, 2012 D. V. Wiebe
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
					 GetData.pm.in MANIFEST.in test/big_test.t test/callback.t

if GETDATA_DEBUG
TEST_VERBOSE=verbose=1
endif

PERL_TESTS=t/big_test.t t/callback.t

man3dir=${perlmandir}
nodist_man3_MANS=blib/libdoc/GetData.$(PERL_MAN3EXT)

nodist_perl_SCRIPTS=blib/lib/GetData.pm

perlautogetdatadir=${perldir}/auto/GetData
nodist_perlautogetdata_SCRIPTS=blib/arch/auto/GetData/GetData.bs blib/arch/auto/GetData/GetData.so

t lib:
	mkdir -p $@

t/%.t: test/%.t
	$(MAKE) t
	cat $< > $@

lib/GetData.xs: GetData.xs
	$(MAKE) lib
	cat $(srcdir)/GetData.xs > $@

lib/typemap: typemap
	$(MAKE) lib
	cat $(srcdir)/typemap > $@

lib/simple_funcs.xs: simple_funcs.xsin simple_funcs.pl
	$(MAKE) lib
	$(PERL) $(srcdir)/simple_funcs.pl $(srcdir)/simple_funcs.xsin > $@

MANIFEST: MANIFEST.in
	cat $(srcdir)/MANIFEST.in > $@

make_parameters.sed: ../make_parameters ../../src/getdata.h
	  ../make_parameters P > $@

../make_parameters: ../make_parameters.c ../../src/gd_config.h
	  cd .. && ${MAKE} make_parameters

GetData.pm: GetData.pm.in make_parameters.sed funclist.pl lib/GetData.xs \
	lib/simple_funcs.xs
	rm -f $@
	$(SED) -f make_parameters.sed $(srcdir)/GetData.pm.in | \
		$(PERL) $(srcdir)/funclist.pl lib/GetData.xs lib/simple_funcs.xs > $@
	chmod a-w $@

blib/arch/auto/GetData/GetData.bs blib/arch/auto/GetData/GetData.so \
	blib/lib/GetData.pm blib/libdoc/GetData.$(PERL_MAN3EXT): build.stamp
	
build.stamp: Build lib/GetData.xs lib/simple_funcs.xs lib/typemap
	$(PERL) Build build
	touch $@

.PRECIOUS: Build
Build: Build.PL GetData.pm lib/GetData.xs lib/simple_funcs.xs lib/typemap \
	MANIFEST
	$(PERL) Build.PL

clean-local: 
	rm -rf lib blib
	if test -e Build; then \
		$(PERL) Build clean; \
	fi
	rm -f $(BUILT_SOURCES) make_parameters.sed MYMETA.yml MYMETA.json GetData.pm
	rm -rf Build _build MANIFEST t build.stamp

uninstall-hook:
	rmdir $(DESTDIR)${perlautogetdatadir}

if TEST_PERL
check: $(PERL_TESTS)
	${DL_LIBRARY_PATH}=${${DL_LIBRARY_PATH}}:../../src/.libs $(PERL) ./Build test ${TEST_VERBOSE}

testdb testcover: $(PERL_TESTS)
	${DL_LIBRARY_PATH}=${${DL_LIBRARY_PATH}}:../../src/.libs $(PERL) ./Build $@ ${TEST_VERBOSE}

.PHONY: check testdb testcover
endif
