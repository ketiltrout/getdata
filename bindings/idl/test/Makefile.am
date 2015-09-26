# Copyright (C) 2009, 2010, 2014, 2015 D. V. Wiebe
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

if TEST_IDL
TESTS_ENVIRONMENT=${DL_LIBRARY_PATH}=../../../src/.libs:${${DL_LIBRARY_PATH}} IDL_PATH="${srcdir}" IDL_DLM_PATH=../.libs ${IDL}
idlTESTS=big_test.pro
TESTS=$(addprefix ${srcdir}/,$(idlTESTS))
endif

EXTRA_DIST=getdata_idl_test_routines.pro ${idlTESTS}

# clean up after the tests
check-local: check-TESTS
	$(MAKE) getdata-clean-dirs

getdata-clean-dirs:
	rm -rf dirfile

clean-local:
	$(MAKE) getdata-clean-dirs
	rm -rf *~ 
