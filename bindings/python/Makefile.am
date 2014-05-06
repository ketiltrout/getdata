# Copyright (C) 2009, 2013 D. V. Wiebe
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

SUBDIRS=test

nodist_python_SCRIPTS = pygetdata.so

BUILT_SOURCES = pyconstants.c

EXTRA_DIST = pydirfile.c pygetdata.c pyentry.c pygetdata.h pyfragment.c

distutils_path=build/lib.${PYTHON_PLATFORM}-${PYTHON_VERSION}
pygetdata.so: ${distutils_path}/pygetdata.so
	cp $< $@

${distutils_path}/pygetdata.so: setup.py ${BUILT_SOURCES}
	${PYTHON} setup.py build

pyconstants.c: ../make_parameters
	../make_parameters p > $@

../make_parameters: ../make_parameters.c
	cd .. && ${MAKE} make_parameters

clean-local:
if MAKE_PYBINDINGS
	if [ -e setup.py ]; then ${PYTHON} setup.py clean; fi
endif
	rm -rf build pygetdata.so ${BUILT_SOURCES} debug.c *~ 

