# (C) 2008-2010 D. V. Wiebe
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

if LFS_TRANSITIONAL_API
LFS_transitional_mans = gd_getdata64.3 gd_get_framenum_subset64.3 \
												gd_get_frameoffset64.3 gd_get_nframes64.3 gd_putdata64.3
INSTALL_DATA_HOOK64 = install-data-hook64
endif

nodist_man_MANS = gd_alter_encoding.3 gd_alter_endianness.3 \
											 gd_alter_frameoffset.3
dist_man_MANS = gd_add.3 gd_add_bit.3 gd_add_spec.3 gd_alter_bit.3 \
								gd_alter_entry.3 gd_alter_spec.3 gd_cbopen.3 gd_close.3 \
								gd_get_constant.3 gd_put_constant.3 gd_get_constants.3 \
								gd_getdata.3 gd_putdata.3 gd_delete.3 gd_get_encoding.3 \
								gd_get_endianness.3 gd_get_entry.3 gd_get_entry_type.3 \
								gd_error.3 gd_error_string.3 gd_get_field_list.3 \
								gd_get_field_list_by_type.3 gd_flush.3 gd_get_fragment_index.3 \
								gd_get_fragmentname.3 gd_get_framenum_subset.3 \
								gd_get_frameoffset.3 gd_free_entry_strings.3 gd_include.3 \
								gd_madd_bit.3 gd_get_mconstants.3 gd_metaflush.3 \
								gd_get_mfield_list.3 gd_get_mfield_list_by_type.3 gd_move.3 \
								gd_get_mstrings.3 gd_get_mvector_list.3 gd_dirfilename.3 \
								gd_get_native_type.3 gd_get_nfields.3 gd_get_nfields_by_type.3 \
								gd_get_nfragments.3 gd_get_nframes.3 gd_get_nmfields.3 \
								gd_get_nmfields_by_type.3 gd_get_nmvectors.3 gd_get_nvectors.3 \
								gd_get_parent_fragment.3 gd_parser_callback.3 gd_protect.3 \
								gd_get_protection.3 gd_get_raw_filename.3 gd_reference.3 \
								gd_rename.3 gd_get_spf.3 gd_get_string.3 gd_put_string.3 \
								gd_get_strings.3 gd_uninclude.3 gd_validate.3 \
								gd_get_vector_list.3 dirfile.5 dirfile-encoding.5 \
								dirfile-format.5 $(LFS_transitional_mans)

install-data-hook: ${INSTALL_DATA_HOOK64}
	cd $(DESTDIR)$(mandir)/man3 && \
		$(LN_S) -f gd_add_bit.3 gd_add_clincom.3 && \
		$(LN_S) -f gd_add_bit.3 gd_add_const.3 && \
		$(LN_S) -f gd_add_bit.3 gd_add_cpolynom.3 && \
		$(LN_S) -f gd_add_bit.3 gd_add_lincom.3 && \
		$(LN_S) -f gd_add_bit.3 gd_add_linterp.3 && \
		$(LN_S) -f gd_add_bit.3 gd_add_multiply.3 && \
		$(LN_S) -f gd_add_bit.3 gd_add_polynom.3 && \
		$(LN_S) -f gd_add_bit.3 gd_add_phase.3 && \
		$(LN_S) -f gd_add_bit.3 gd_add_raw.3 && \
		$(LN_S) -f gd_add_bit.3 gd_add_sbit.3 && \
		$(LN_S) -f gd_add_bit.3 gd_add_string.3
	cd $(DESTDIR)$(mandir)/man3 && \
		$(LN_S) -f gd_madd_bit.3 gd_madd_clincom.3 && \
		$(LN_S) -f gd_madd_bit.3 gd_madd_const.3 && \
		$(LN_S) -f gd_madd_bit.3 gd_madd_cpolynom.3 && \
		$(LN_S) -f gd_madd_bit.3 gd_madd_lincom.3 && \
		$(LN_S) -f gd_madd_bit.3 gd_madd_linterp.3 && \
		$(LN_S) -f gd_madd_bit.3 gd_madd_multiply.3 && \
		$(LN_S) -f gd_madd_bit.3 gd_madd_phase.3 && \
		$(LN_S) -f gd_madd_bit.3 gd_madd_polynom.3 && \
		$(LN_S) -f gd_madd_bit.3 gd_madd_sbit.3 && \
		$(LN_S) -f gd_madd_bit.3 gd_madd_string.3
	cd $(DESTDIR)$(mandir)/man3 && \
		$(LN_S) -f gd_add.3 gd_madd.3
	cd $(DESTDIR)$(mandir)/man3 && \
		$(LN_S) -f gd_add_spec.3 gd_madd_spec.3
	cd $(DESTDIR)$(mandir)/man3 && \
		$(LN_S) -f gd_cbopen.3 gd_open.3
	cd $(DESTDIR)$(mandir)/man3 && \
		$(LN_S) -f gd_close.3 gd_discard.3
	cd $(DESTDIR)$(mandir)/man3 && \
		$(LN_S) -f gd_alter_bit.3 gd_alter_clincom.3 && \
		$(LN_S) -f gd_alter_bit.3 gd_alter_const.3 && \
		$(LN_S) -f gd_alter_bit.3 gd_alter_cpolynom.3 && \
		$(LN_S) -f gd_alter_bit.3 gd_alter_lincom.3 && \
		$(LN_S) -f gd_alter_bit.3 gd_alter_linterp.3 && \
		$(LN_S) -f gd_alter_bit.3 gd_alter_multiply.3 && \
		$(LN_S) -f gd_alter_bit.3 gd_alter_phase.3 && \
		$(LN_S) -f gd_alter_bit.3 gd_alter_polynom.3 && \
		$(LN_S) -f gd_alter_bit.3 gd_alter_raw.3
	cd $(DESTDIR)$(mandir)/man3 && \
		$(LN_S) -f gd_alter_spec.3 gd_malter_spec.3
	cd $(DESTDIR)$(mandir)/man3 && \
		$(LN_S) -f gd_framenum_subset.3 gd_framenum.3

install-data-hook64:
	cd $(DESTDIR)$(mandir)/man3 && \
		$(LN_S) -f gd_get_frameoffset64.3 gd_alter_frameoffset64.3

uninstall-hook:
	cd $(DESTDIR)$(mandir)/man3 && \
		rm -rf gd_add_const.3 gd_add_lincom.3 gd_add_linterp.3 gd_add_multiply.3 \
		gd_add_phase.3 gd_add_raw.3 gd_add_string.3 gd_add_const.3 gd_add_lincom.3 \
		gd_add_linterp.3 gd_add_multiply.3 gd_add_phase.3 gd_add_string.3 \
		gd_madd.3 gd_madd_spec.3 gd_open.3 gd_discard.3 gd_alter_const.3 \
		gd_alter_lincom.3 gd_alter_linterp.3 gd_alter_multiply.3 gd_alter_phase.3 \
		gd_alter_raw.3 gd_malter_spec.3 gd_alter_frameoffset64.3 gd_add_clincom.3 \
		gd_add_cpolynom.3 gd_add_polynom.3 gd_add_sbit.3 gd_madd_clincom.3 \
		gd_madd_cpolynom.3 gd_madd_polynom.3 gd_madd_sbit.3 gd_alter_clincom.3 \
		gd_alter_cpolynom.3 gd_alter_polynom.3 gd_alter_sbit.3 gd_framenum.3 \
		gd_alter_frameoffset64.3

clean-local:
	rm -rf *~ 
	rm -rf gd_alter_encoding.3 gd_alter_endianness.3 gd_alter_frameoffset.3
