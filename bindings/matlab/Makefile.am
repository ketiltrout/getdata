# Copyright (C) 2013-2016 D. V. Wiebe
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

matlabdir=$(matlabbasedir)/getdata

matlabfiles=gd_add_bit.m gd_add_divide.m gd_add_indir.m gd_add_lincom.m \
						gd_add_linterp.m gd_add_mplex.m gd_add_multiply.m gd_add_phase.m \
						gd_add_polynom.m gd_add_raw.m gd_add_recip.m gd_add_sbit.m \
						gd_add_sindir.m gd_add_window.m \
						gd_alter_bit.m gd_alter_carray.m gd_alter_const.m \
						gd_alter_divide.m gd_alter_indir.m \
						gd_alter_lincom.m gd_alter_linterp.m \
						gd_alter_mplex.m gd_alter_multiply.m gd_alter_phase.m \
						gd_alter_polynom.m gd_alter_raw.m gd_alter_recip.m \
						gd_alter_sarray.m gd_alter_sbit.m gd_alter_sindir.m \
						gd_alter_window.m gd_field_list_by_type.m gd_field_list.m \
						gd_get_constant.m gd_madd_bit.m gd_madd_divide.m gd_madd_indir.m \
						gd_madd_lincom.m gd_madd_linterp.m gd_madd_mplex.m \
						gd_madd_multiply.m gd_madd_phase.m gd_madd_polynom.m \
						gd_madd_recip.m gd_madd_sbit.m gd_madd_sindir.m gd_madd_window.m \
						gd_mfield_list_by_type.m gd_mfield_list.m gd_mvector_list.m \
						gd_nfields_by_type.m gd_nfields.m gd_nmfields_by_type.m \
						gd_nmfields.m gd_nmvectors.m gd_nvectors.m gd_put_carray.m \
						gd_put_constant.m gd_put_sarray.m gd_vector_list.m \
						getdata_constants.m
mexfiles=gd_add gd_add_alias \
				 gd_add_carray gd_add_const gd_add_sarray \
				 gd_add_spec gd_add_string gd_aliases \
				 gd_alias_target gd_alter_affixes \
				 gd_alter_encoding gd_alter_endianness gd_alter_entry \
				 gd_alter_frameoffset \
				 gd_alter_protection gd_alter_spec gd_bof gd_array_len gd_carrays \
				 gd_close gd_constants gd_delete gd_desync \
				 gd_dirfilename gd_dirfile_standards \
				 gd_discard gd_encoding gd_encoding_support gd_endianness \
				 gd_entry gd_entry_list gd_entry_type gd_eof \
				 gd_error gd_error_string gd_flags gd_flush gd_fragment_affixes \
				 gd_fragment_index gd_fragmentname gd_fragment_namespace \
				 gd_framenum gd_frameoffset gd_get_carray gd_get_carray_slice \
				 gd_getdata gd_get_sarray gd_get_sarray_slice \
				 gd_get_string gd_hidden gd_hide gd_include gd_include_affix \
				 gd_invalid_dirfile gd_linterp_tablename gd_madd gd_madd_alias \
				 gd_madd_carray gd_madd_const gd_madd_sarray \
				 gd_madd_spec gd_madd_string \
				 gd_malter_spec gd_match_entries gd_mcarrays \
				 gd_mconstants gd_metaflush gd_move gd_mplex_lookback gd_msarrays \
				 gd_mstrings gd_naliases gd_native_type gd_nentries gd_nfragments \
				 gd_nframes gd_open gd_parent_fragment gd_protection \
				 gd_put_carray_slice \
				 gd_putdata gd_put_sarray_slice \
				 gd_put_string gd_raw_close gd_raw_filename \
				 gd_reference gd_rename gd_rewrite_fragment \
				 gd_sarrays gd_seek gd_spf gd_strings gd_strtok \
				 gd_sync gd_tell gd_unhide gd_uninclude gd_validate gd_verbose_prefix

matlabcfiles=$(addsuffix .c,${mexfiles})
matlabmexfiles=$(addsuffix .@mexext@,${mexfiles})
matlabdocfiles=$(addsuffix .m,${mexfiles})

EXTRA_DIST=$(matlabcfiles) doc.tail Contents.m.head

dist_matlab_SCRIPTS=$(matlabfiles)
nodist_matlab_SCRIPTS=Contents.m getdata_constants.m $(matlabmexfiles) \
											$(matlabdocfiles)

lib_LTLIBRARIES=libgetdata-matlab.la

if GD_EXTERNAL
libgetdata_matlab_la_LIBADD=$(GETDATA_LIBS)
AM_CPPFLAGS=$(GETDATA_CFLAGS)
GDMX_LIBS=$(GETDATA_LIBS)
else
libgetdata_matlab_la_LIBADD=../../src/libgetdata.la
AM_CPPFLAGS=-I${top_srcdir}/src -I../../src
GDMX_LIBS=-L../../src/.libs -lgetdata
endif

libgetdata_matlab_la_SOURCES=matlab.c gd_matlab.h
libgetdata_matlab_la_CPPFLAGS=${MATLAB_CPPFLAGS} ${AM_CPPFLAGS}
libgetdata_matlab_la_LDFLAGS=-version-info @MATLABGETDATA_VERSION@

%.m: %.c ${srcdir}/doc.tail
	grep '^ %' $< | sed -e 's/^ //' > $@
	cat ${srcdir}/doc.tail >> $@

%.@mexext@: %.c libgetdata-matlab.la
	${MEX} ${AM_CPPFLAGS} -L.libs -lgetdata-matlab $(GDMX_LIBS) $<

Contents.m: ${srcdir}/Contents.m.head make_contents.sh $(matlabfiles) \
						$(matlabdocfiles) ${srcdir}/doc.tail
	${SHELL} ./make_contents.sh ${srcdir} > $@
	cat ${srcdir}/doc.tail >> $@

getdata_constants.m: ../make_parameters
	../make_parameters m > $@

../make_parameters: ../make_parameters.c
	cd .. && ${MAKE} make_parameters

clean-local:
	rm -rf $(nodist_matlab_SCRIPTS) *~ *.gcno
