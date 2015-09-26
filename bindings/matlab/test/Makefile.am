# Copyright (C) 2013, 2014, 2015 D. V. Wiebe
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

if TEST_MATLAB
TESTS_ENVIRONMENT=${DL_LIBRARY_PATH}=../../../src/.libs:../.libs:${${DL_LIBRARY_PATH}} MATLABPATH=${srcdir}:..:${srcdir}/.. ${srcdir}/test.sh ${MATLAB}

matlabTESTS=big_test.m

TESTS=$(addprefix ${srcdir}/,$(matlabTESTS))
endif

EXTRA_DIST=${matlabTESTS} test.sh

check-local: check-TESTS
	echo $(MAKE) getdata-clean-dirs

getdata-clean-dirs:
	rm -rf dirfile

clean-local:
	$(MAKE) getdata-clean-dirs
	rm -rf *~
