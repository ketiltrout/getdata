# Copyright (C) 2008-2012, 2014 D. V. Wiebe
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
AUTOMAKE_OPTIONS = foreign serial-tests

AM_CPPFLAGS = ${GD_CXX_WALL} $(GD_CXX_WEXTRA) -I$(srcdir)/.. -I$(top_srcdir)/src
LDADD=../libgetdata++.la
TESTS=big_test

big_test_SOURCES=big_test.cpp

check_PROGRAMS=${TESTS}

# clean up after the tests
check-local: check-TESTS
	$(MAKE) getdata-clean-dirs

getdata-clean-dirs:
	rm -rf *odirfile

clean-local:
	$(MAKE) getdata-clean-dirs
	rm -rf *~
