# Copyright (C) 2008-2014, 2016 D. V. Wiebe
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

BUILDCC=@BUILDCC@

if MAKE_CXXBINDINGS
CXX_SUBDIR=cxx
endif

if MAKE_F77BINDINGS
F77_SUBDIR=f77
endif

if MAKE_IDLBINDINGS
IDL_SUBDIR=idl
endif

if MAKE_MATLABBINDINGS
MATLAB_SUBDIR=matlab
endif

if MAKE_PYBINDINGS
PY_SUBDIR=python
endif

if MAKE_PERLBINDINGS
PERL_SUBDIR=perl
endif

if MAKE_PHPBINDINGS
PHP_SUBDIR=php
endif

noinst_PROGRAMS=make_parameters

make_parameters_SOURCES = make_parameters.c
nodist_make_parameters_SOURCES = ../src/getdata.h ../src/gd_config.h

SUBDIRS = . ${CXX_SUBDIR} ${F77_SUBDIR} ${IDL_SUBDIR} ${MATLAB_SUBDIR} \
					${PERL_SUBDIR} ${PHP_SUBDIR} ${PY_SUBDIR} 

make_parameters$(EXEEXT): $(make_parameters_SOURCES) \
				    $(nodist_make_parameters_SOURCES)
	${BUILDCC} $(DEFS) -I$(top_srcdir)/src -I../src -o make_parameters$(EXEEXT) \
		$(srcdir)/$(make_parameters_SOURCES)

clean-local:
	rm -rf *~ 
