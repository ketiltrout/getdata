# Copyright (C) 2009, 2013, 2014, 2016, 2018 D. V. Wiebe
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

if MAKE_PYGETDATA_CAPI
PYGETDATA_H=pygetdata.h
endif

nodist_python_SCRIPTS = pygetdata${PYTHON_OBJECT_SUFFIX}
python_HEADERS = ${PYGETDATA_H}

BUILT_SOURCES = pyconstants.c

EXTRA_DIST = pydirfile.c pygetdata.c pyentry.c gdpy_intern.h pyfragment.c

pygetdata${PYTHON_OBJECT_SUFFIX}: setup.py ${BUILT_SOURCES} ${EXTRA_DIST}
	${PYTHON} setup.py build_ext --inplace

pyconstants.c: ../make_parameters
	../make_parameters p > $@

../make_parameters: ../make_parameters.c
	cd .. && ${MAKE} make_parameters

clean-local:
if MAKE_PYBINDINGS
	test ! -f setup.py || ${PYTHON} setup.py clean
endif
	rm -rf build pygetdata${PYTHON_OBJECT_SUFFIX} ${BUILT_SOURCES} debug.c *~ libpath_mangled

# Handle MacOS >=10.11 embedding install paths in .so files
if USE_INSTALL_NAME_TOOL
nodist_noinst_DATA = libpath_mangled

# Change, say /usr/local/lib/libgetdata.#.dylib, into .libs/libgetdata.#.dylib
libpath_mangled: ${nodist_python_SCRIPTS}
	${INSTALL_NAME_TOOL} -change \
		${libdir}/libgetdata.${GETDATA_IFACE_VERSION}.dylib \
		${abs_top_builddir}/src/.libs/libgetdata.${GETDATA_IFACE_VERSION}.dylib $<
	touch $@

# Change mangled path into install path again
install-exec-hook:
	${INSTALL_NAME_TOOL} -change \
		${abs_top_builddir}/src/.libs/libgetdata.${GETDATA_IFACE_VERSION}.dylib \
		${libdir}/libgetdata.${GETDATA_IFACE_VERSION}.dylib \
		${DESTDIR}${pythondir}/pygetdata${PYTHON_OBJECT_SUFFIX}
endif

# Avoid a race condition from distclean deleting setup.py between the test and
# the ${PYTHON} in the clean-local rule.  The variable shennanigans here are to
# fool automake into letting us add a second distclean-generic rule.  Had we
# defined this rule with a bare "disclean-generic:", automake would assume we've
# redefined the distclean-generic recipe, with an empty one, which we don't want
# to do.
gd_distclean_generic = distclean-generic
$(gd_distclean_generic): clean-local
