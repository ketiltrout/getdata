# Copyright (C) 2013, 2014, 2015, 2016 D. V. Wiebe
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

if TEST_PHP
TESTS_ENVIRONMENT=${DL_LIBRARY_PATH}=../../../src/.libs:${${DL_LIBRARY_PATH}} \
	${PHP} -n -d "output_handler=" -d "open_basedir=" -d "safe_mode=0" \
	-d "disable_functions=" -d "output_buffering=Off" -d "error_reporting=32767" \
	-d "display_errors=1" -d "display_startup_errors=1" -d "log_errors=0" \
	-d "html_errors=0" -d "track_errors=1" -d "report_memleaks=1" \
	-d "report_zend_debug=0" -d "docref_root=" -d "docref_ext=.html" \
	-d "error_prepend_string=" -d "error_append_string=" -d "auto_prepend_file=" \
	-d "auto_append_file=" -d "magic_quotes_runtime=0" \
	-d "ignore_repeated_errors=0" -d "precision=14" -d "memory_limit=128M" \
	-d "extension_dir=@top_builddir@/bindings/php/.libs" \
	-d "extension=getdata.so" -d "session.auto_start=0" -f
phpTESTS=big_test.php callback.php persistence.php
TESTS=$(addprefix ${srcdir}/,$(phpTESTS))
endif

EXTRA_DIST=${phpTESTS}

# clean up after the tests
check-local: check-TESTS
	$(MAKE) getdata-clean-dirs

getdata-clean-dirs:
	echo rm -rf dirfile

clean-local:
	$(MAKE) getdata-clean-dirs
	rm -rf *~ 
