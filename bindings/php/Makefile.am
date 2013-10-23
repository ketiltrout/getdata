# Copyright (C) 2013 D. V. Wiebe
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
all-local: getdata/Makefile
	cd getdata && ${MAKE}

BUILT_SOURCES=getdata/constants.c

getdata/constants.c: ../make_parameters ../../src/getdata.h
	  ../make_parameters h > $@

../make_parameters: ../make_parameters.c ../../src/gd_config.h
	  cd .. && ${MAKE} make_parameters

getdata/modules: getdata/Makefile

getdata/Makefile: getdata/configure
	cd getdata && \
		./configure --enable-getdata && \
		${SED} -e "s/make test/make check/" < Makefile > Makefile.temp && \
		mv -f Makefile.temp Makefile

getdata/configure: getdata/config.m4
	cd getdata && ${PHPIZE}

clean-local:
if MAKE_PHPBINDINGS
	cd getdata && ${PHPIZE} --clean
endif
	rm -rf *~ getdata/*~ ${BUILT_SOURCES} getdata/Makefile.temp

SUBDIRS=. test
