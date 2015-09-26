# Copyright (C) 2008, 2009, 2012, 2013 D. V. Wiebe
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

if MAKE_CXXBINDINGS
READMECXX=README.cxx
endif

if MAKE_F77BINDINGS
READMEF77=README.f77
endif

if MAKE_F95BINDINGS
READMEF95=README.f95
endif

if MAKE_IDLBINDINGS
READMEIDL=README.idl
endif

if MAKE_MATLABBINDINGS
READMEMATLAB=README.matlab
endif

if MAKE_PERLBINDINGS
READMEPERL=README.perl
endif

if MAKE_PYBINDINGS
READMEPYTHON=README.python
endif

dist_doc_DATA = ${READMECXX} ${READMEF77} ${READMEF95} ${READMEIDL} \
								${READMEMATLAB} ${READMEPERL} ${READMEPYTHON} \
								unclean_database_recovery.txt

clean-local:
	rm -rf *~
