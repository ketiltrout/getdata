# Copyright (C) 2008, 2011 D. V. Wiebe
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
AUTOMAKE_OPTIONS = foreign dist-bzip2 check-news 1.10

EXTRA_DIST = ChangeLog

ACLOCAL_AMFLAGS = -I m4

SUBDIRS = src . test bindings util man doc

dist_doc_DATA = COPYING.DOC

clean-local:
	rm -rf *~  m4/*~ README.txt

%.sig: %
	gpg -abo $@ $<

DISTCHECK_CONFIGURE_FLAGS = --with-idl-dlm-dir="$$dc_install_base/idl" \
														--with-perl-dir="$$dc_install_base/perl" \
														--with-python-module-dir="$$dc_install_base/python"

# idl-only package
idldist: $(DISTFILES)
	rm -rf svn_export
	svn export . svn_export
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
		make dist && \
		cp idl_${distdir}* ../.. \
		) )
	rm -rf svn_export

idldistcheck: idl_$(distdir).tar.gz
	tar -zxvf $<
	cd idl_$(distdir) && ./configure && make dist && make distcheck
	rm -rf idl_$(distdir)

# windows source package
windist: $(DISTFILES)
	rm -rf crlf_export
	cd man && $(MAKE) htmlman
	svn --native-eol CRLF export . crlf_export
	( cd crlf_export && \
		mkdir $(distdir) && \
		mkdir $(distdir)/bindings && \
		mkdir $(distdir)/doc && \
		mkdir $(distdir)/man && \
		cp ../man/*[35].html $(distdir)/man && \
		cp -r bindings/cxx $(distdir)/bindings && \
		cp -r cmake src test $(distdir) && \
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

# package lists
PACKAGES=$(distdir).tar.gz $(distdir).tar.bz2 idl_$(distdir).tar.gz \
				 idl_$(distdir).tar.bz2 getdata_win-$(PACKAGE_VERSION).zip
SIGS=$(addsuffix .sig,$(PACKAGES))

# release stuff
README.txt: NEWS RELEASE_NOTES.in
	cp -f RELEASE_NOTES.in $@ 
	head -n `grep -nm 1 '^|' NEWS | awk 'BEGIN {FS=":"} {print $$1-1}'` NEWS >> $@

dist-post: $(SIGS)

dist-push: $(PACKAGES) $(SIGS) README.txt
	rsync -avP -e ssh $^ $(SFUSER),getdata@frs.sourceforge.net:/home/frs/project/g/ge/getdata/getdata/$(PACKAGE_VERSION)/
