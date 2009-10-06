# (C) 2008 D. V. Wiebe
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

SUBDIRS = src util man . doc test bindings

dist_doc_DATA = COPYING.DOC

clean-local:
	rm -rf *~  m4/*~

%.sig: %
	gpg -abo $@ $<

DISTCHECK_CONFIGURE_FLAGS = --with-idl-dlm-dir="$$dc_install_base/idl"

dist-post: $(distdir).tar.gz.sig $(distdir).tar.bz2.sig

dist-push:  $(distdir).tar.gz.sig $(distdir).tar.bz2.sig $(distdir).tar.gz $(distdir).tar.bz2
	rsync -avP -e ssh $^ $(SFUSER)@frs.sourceforge.net:uploads/
