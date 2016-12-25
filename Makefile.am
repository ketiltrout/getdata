# Copyright (C) 2008-2015 D. V. Wiebe
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

EXTRA_DIST = ChangeLog

ACLOCAL_AMFLAGS = -I m4

if MAKE_UTIL
UTIL = util
endif
SUBDIRS = src . test bindings ${UTIL} man doc

dist_doc_DATA = COPYING.DOC

clean-local:
	if [ -e $(distdir) ]; then chmod -R u+w $(distdir); fi
	if [ -e idl_$(distdir) ]; then chmod -R u+w idl_$(distdir); fi
	if [ -e matlab_$(distdir) ]; then chmod -R u+w matlab_$(distdir); fi
	rm -rf *~  m4/*~ README.txt $(distdir) idl_$(distdir) matlab_$(distdir)

%.sig: %
	gpg -abo $@ $<

DISTCHECK_CONFIGURE_FLAGS = --enable-legacy-api

# a locally-installed library for building and testing the IDL and matlab
# packages
$(distdir)/_inst/lib/pkgconfig: $(distdir).tar.gz
	tar -zxvf $(distdir).tar.gz
	cd $(distdir) && \
	mkdir _inst && \
	./configure --enable-legacy-api --disable-bindings --prefix=`pwd`/_inst && \
		${MAKE} && \
		${MAKE} install

# matlab-only package
matlab_$(distdir).tar.gz: matlabdist
matlabdist: $(DISTFILES) $(distdir)/_inst/lib/pkgconfig
	rm -rf svn_export
	svn export $(top_srcdir) svn_export
	PKG_CONFIG_PATH=`pwd`/$(distdir)/_inst/lib/pkgconfig && \
	( cd svn_export && \
		mkdir matlab_getdata && \
		mkdir matlab_getdata/m4 && \
		mkdir matlab_getdata/src && \
		cp bindings/matlab/package/* matlab_getdata && \
		cp -r bindings/matlab/Makefile.am bindings/matlab/gd_*.[cm] \
		bindings/matlab/matlab.c bindings/matlab/gd_matlab.h bindings/matlab/test \
		bindings/matlab/doc.tail bindings/matlab/Contents.m.head \
		bindings/matlab/make_contents.sh.in \
		matlab_getdata/src && \
		cp -r bindings/make_parameters.c matlab_getdata && \
		cp -r m4/matlab.m4 m4/compiler.m4 m4/version.m4 matlab_getdata/m4 && \
		cp -r doc/README.matlab COPYING AUTHORS ChangeLog INSTALL NEWS \
		matlab_getdata && \
		( cd matlab_getdata && \
		aclocal -I m4 && \
		libtoolize && \
		autoconf && \
		autoheader && \
		automake --add-missing --force-missing && \
		./configure && \
		${MAKE} dist && \
		cp matlab_${distdir}* ../.. \
		) )
	rm -rf svn_export

matlabdistcheck: matlab_$(distdir).tar.gz $(distdir)/_inst/lib/pkgconfig
	if [ -e matlab_$(distdir) ]; then \
		chmod -R u+w matlab_$(distdir); rm -rf matlab_$(distdir); \
	fi
	tar -zxvf $<
	PKG_CONFIG_PATH=`pwd`/$(distdir)/_inst/lib/pkgconfig && \
	${DL_LIBRARY_PATH}=${${DL_LIBRARY_PATH}}:`pwd`/$(distdir)/_inst/lib && \
	cd matlab_$(distdir) && ./configure && make dist && make distcheck
	rm -rf matlab_$(distdir)
	rm -rf $(distdir)

# idl-only package
idl_$(distdir).tar.gz: idldist
idldist: $(DISTFILES) $(distdir)/_inst/lib/pkgconfig
	rm -rf svn_export
	svn export $(top_srcdir) svn_export
	PKG_CONFIG_PATH=`pwd`/$(distdir)/_inst/lib/pkgconfig && \
	( cd svn_export && \
		mkdir idl_getdata && \
		mkdir idl_getdata/m4 && \
		mkdir idl_getdata/src && \
		cp bindings/idl/package/* idl_getdata && \
		cp -r bindings/idl/Makefile.am bindings/idl/makedlm.sh.in \
		bindings/idl/getdata.c bindings/idl/test idl_getdata/src && \
		cp -r bindings/make_parameters.c idl_getdata && \
		cp -r m4/idl.m4 m4/compiler.m4 m4/ax_compare_version.m4 m4/version.m4 \
		idl_getdata/m4 && \
		cp -r doc/README.idl COPYING AUTHORS ChangeLog INSTALL NEWS idl_getdata && \
		( cd idl_getdata && \
		aclocal -I m4 && \
		libtoolize && \
		autoconf && \
		autoheader && \
		automake --add-missing --force-missing && \
		./configure && \
		${MAKE} dist && \
		cp idl_${distdir}* ../.. \
		) )
	rm -rf svn_export

idldistcheck: idl_$(distdir).tar.gz $(distdir)/_inst/lib/pkgconfig
	if [ -e idl_$(distdir) ]; then \
		chmod -R u+w idl_$(distdir); rm -rf idl_$(distdir); \
	fi
	tar -zxvf $<
	PKG_CONFIG_PATH=`pwd`/$(distdir)/_inst/lib/pkgconfig && \
	${DL_LIBRARY_PATH}=${${DL_LIBRARY_PATH}}:`pwd`/$(distdir)/_inst/lib && \
	cd idl_$(distdir) && ./configure && make dist && make distcheck
	rm -rf idl_$(distdir)
	rm -rf $(distdir)

# windows source package
windist: $(DISTFILES)
	rm -rf crlf_export
	cd man && $(MAKE) htmlman
	svn --native-eol CRLF export $(top_srcdir) crlf_export
	( cd crlf_export && \
		mkdir $(distdir) && \
		mkdir $(distdir)/bindings && \
		mkdir $(distdir)/doc && \
		mkdir $(distdir)/man && \
		cp ../man/*[35].html $(distdir)/man && \
		cp -r bindings/cxx $(distdir)/bindings && \
		cp -r cmake util src test $(distdir) && \
		$(SED) cmake/CMakeLists.txt \
		-e 's/-DPACKAGE_VERSION=\\".*\\"/-DPACKAGE_VERSION=\\"$(PACKAGE_VERSION)\\"/' \
		> $(distdir)/cmake/CMakeLists.txt && \
		cp -r doc/README.cxx doc/unclean_database_recovery.txt $(distdir)/doc && \
		cp AUTHORS COPYING COPYING.DOC ChangeLog NEWS TODO $(distdir) && \
		cp README.win $(distdir)/README && \
		rm -f $(distdir)/*/Makefile.am $(distdir)/*/*/Makefile.am && \
		zip -rq getdata_win-$(PACKAGE_VERSION).zip $(distdir) && \
		cp getdata_win*.zip .. \
	)
	rm -rf crlf_export

# coverity scan target
cov-scan:
	rm -rf cov-int
	make clean
	./config.status --recheck
	cov-build --dir cov-int make
	tar -zcvf getdata-cov-int-${PACKAGE_VERSION}.tgz cov-int/

# package lists
PACKAGES=$(distdir).tar.gz $(distdir).tar.xz idl_$(distdir).tar.gz \
				 idl_$(distdir).tar.xz getdata_win-$(PACKAGE_VERSION).zip \
				 matlab_$(distdir).tar.gz matlab_$(distdir).tar.xz
SIGS=$(addsuffix .sig,$(PACKAGES))

# release stuff
README.txt: NEWS RELEASE_NOTES.in
	cp -f RELEASE_NOTES.in $@ 
	head -n `grep -nm 1 '^|' NEWS | awk 'BEGIN {FS=":"} {print $$1-1}'` NEWS >> $@

dist-post: $(SIGS)

dist-push: $(PACKAGES) $(SIGS) README.txt
	@if [ -z "${SFUSER}" ]; then echo "ERROR: SFUSER unset"; false; fi
	rsync -avP -e ssh $^ $(SFUSER),getdata@frs.sourceforge.net:/home/frs/project/g/ge/getdata/getdata/$(PACKAGE_VERSION)/
