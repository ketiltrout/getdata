# Copyright (C) 2013, 2014 D. V. Wiebe
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
php_LTLIBRARIES = getdata.la
AM_CFLAGS = ${WALL} ${WEXTRA}
AM_CPPFLAGS = -I$(top_srcdir)/src ${CPPFLAGS} ${PHP_CPPFLAGS}
getdata_la_LDFLAGS = -module -avoid-version -shared ${PHP_LDFLAGS}
getdata_la_LIBADD = ${PHP_LIBS} ../../src/libgetdata.la
getdata_la_SOURCES = getdata.c php_getdata.h
nodist_getdata_la_SOURCES = constants.c

BUILT_SOURCES=constants.c

constants.c: ../make_parameters ../../src/getdata.h
	  ../make_parameters h > $@

../make_parameters: ../make_parameters.c ../../src/gd_config.h
	  cd .. && ${MAKE} make_parameters

clean-local:
	rm -rf *~ ${BUILT_SOURCES}

SUBDIRS=. test
