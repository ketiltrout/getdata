# Copyright (C) 2011, 2012, 2013, 2015 D. V. Wiebe
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

if GETDATA_DEBUG
TEST_VERBOSE=verbose=1
endif

blibdir=build/blib

plTESTS=t/big_test.t t/callback.t

EXTRA_DIST=funclist.pl simple_funcs.pl simple_funcs.xsin GetData.pm.in \
					 src/typemap src/GetData.xs ${plTESTS}

man3dir=${perlmandir}
nodist_man3_MANS=${blibdir}/man3/GetData.$(PERL_MAN3EXT)

nodist_perl_SCRIPTS=${blibdir}/lib/GetData.pm

perlautogetdatadir=${perldir}/auto/GetData
nodist_perlautogetdata_SCRIPTS=${blibdir}/arch/auto/GetData/GetData.so

build/build.stamp: src/Makefile.PL src/GetData.xs src/typemap
	rm -rf build
	mkdir build
	cp -r $(srcdir)/src/* build
	[ -e build/Makefile.PL ] || cp src/Makefile.PL build
	mkdir build/t
	cp -r $(srcdir)/t/* build/t
	mkdir build/lib
	touch $@

src/Makefile.PL: src/Makefile.PL.in $(top_builddir)/config.status
	cd $(top_builddir) && $(SHELL) ./config.status $(subdir)/$@ $(am__depfiles_maybe)

build/simple_funcs.xs: simple_funcs.xsin simple_funcs.pl build/build.stamp
	$(PERL) $(srcdir)/simple_funcs.pl $(srcdir)/simple_funcs.xsin > $@

build/Makefile: build/build.stamp build/lib/GetData.pm
	cd build && ${PERL} Makefile.PL
	$(SED) 's|^LD_RUN_PATH|#LD_RUN_PATH|g;s|LD_RUN_PATH="$$(LD_RUN_PATH)" ||g' \
		-i $@

make_parameters.sed: ../make_parameters ../../src/getdata.h
	  ../make_parameters P > $@

../make_parameters: ../make_parameters.c ../../src/gd_config.h
	  cd .. && ${MAKE} make_parameters

build/lib/GetData.pm: GetData.pm.in make_parameters.sed funclist.pl \
	src/GetData.xs build/simple_funcs.xs build/build.stamp
	rm -f $@
	$(SED) -f make_parameters.sed $(srcdir)/GetData.pm.in | \
		$(PERL) $(srcdir)/funclist.pl $(srcdir)/src/GetData.xs \
		build/simple_funcs.xs > $@
	chmod a-w $@

${blibdir}/arch/auto/GetData/GetData.so ${blibdir}/lib/GetData.pm \
	${blibdir}/man3/GetData.$(PERL_MAN3EXT): build/make.stamp
	
build/make.stamp: build/Makefile
	cd build && ${MAKE}
	touch $@

clean-local: 
	rm -rf build
	rm -f $(BUILT_SOURCES) make_parameters.sed src/Makefile.PL
	rm -rf src/*~ t/*~ *~

uninstall-hook:
	rmdir $(DESTDIR)${perlautogetdatadir}

if TEST_PERL
check: build/Makefile
	export ${DL_LIBRARY_PATH}=../../../src/.libs:${${DL_LIBRARY_PATH}} && \
		( cd build && ${MAKE} test ${TEST_VERBOSE} )

testdb testcover: build/Makefile
	export ${DL_LIBRARY_PATH}=../../../src/.libs:${${DL_LIBRARY_PATH}} && \
		( cd build && ${MAKE} $@ ${TEST_VERBOSE} )

.PHONY: check testdb testcover
endif
