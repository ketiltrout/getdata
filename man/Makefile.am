# Copyright (C) 2008-2010 D. V. Wiebe
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
LFS_transitional_mans = gd_getdata64.3 gd_framenum_subset64.3 \
												gd_frameoffset64.3 gd_nframes64.3 gd_putdata64.3
MAN3ALIASES64=gd_frameoffset64.3:gd_alter_frameoffset64.3
endif

nodist_man_MANS = gd_alter_encoding.3 gd_alter_endianness.3 \
											 gd_alter_frameoffset.3
dist_man_MANS = checkdirfile.1 dirfile2ascii.1 gd_add.3 gd_add_alias.3 \
								gd_add_bit.3 gd_add_spec.3 gd_alias_target.3 gd_aliases.3 \
								gd_alter_affixes.3 gd_alter_bit.3 gd_alter_entry.3 \
								gd_alter_protection.3 gd_alter_spec.3 gd_bof.3 gd_carray_len.3 \
								gd_carrays.3 gd_cbopen.3 gd_close.3 gd_constants.3 \
								gd_dirfile_standards.3 gd_delete.3 gd_dirfilename.3 \
								gd_encoding.3 gd_endianness.3 gd_entry.3 gd_entry_type.3 \
								gd_eof.3 gd_error.3 gd_error_count.3 gd_error_string.3 \
								gd_field_list.3 gd_field_list_by_type.3 gd_flush.3 \
								gd_fragment_affixes.3 gd_fragment_index.3 gd_fragmentname.3 \
								gd_framenum_subset.3 gd_frameoffset.3 gd_free_entry_strings.3 \
								gd_get_carray_slice.3 gd_get_constant.3 gd_getdata.3 \
								gd_get_string.3 gd_hidden.3 gd_hide.3 gd_include.3 \
								gd_invalid_dirfile.3 \
								gd_madd_bit.3 gd_mcarrays.3 gd_mconstants.3 gd_metaflush.3 \
								gd_mfield_list.3 gd_mfield_list_by_type.3 gd_move.3 \
								gd_mstrings.3 gd_mvector_list.3 gd_dirfilename.3 gd_naliases.3 \
								gd_native_type.3 gd_nfields.3 gd_nfields_by_type.3 \
								gd_nfragments.3 gd_nframes.3 gd_nmfields.3 \
								gd_nmfields_by_type.3 gd_nmvectors.3 gd_nvectors.3 \
								gd_parent_fragment.3 gd_parser_callback.3 gd_protection.3 \
								gd_put_carray_slice.3 gd_put_constant.3 gd_putdata.3 \
								gd_put_string.3 gd_raw_filename.3 gd_reference.3 gd_rename.3 \
								gd_rewrite_fragment.3 gd_seek.3 gd_spf.3 gd_strings.3 \
								gd_tell.3 gd_uninclude.3 gd_validate.3 gd_vector_list.3 \
								dirfile.5 dirfile-encoding.5 dirfile-format.5 \
								$(LFS_transitional_mans)

# these are real_file:alias
MAN3ALIASES=gd_add_bit.3:gd_add_carray.3 gd_add_bit.3:gd_add_clincom.3 \
	gd_add_bit.3:gd_add_const.3 gd_add_bit.3:gd_add_cpolynom.3 \
	gd_add_bit.3:gd_add_crecip.3 gd_add_bit.3:gd_add_divide.3 \
	gd_add_bit.3:gd_add_lincom.3 gd_add_bit.3:gd_add_linterp.3 \
	gd_add_bit.3:gd_add_multiply.3 gd_add_bit.3:gd_add_polynom.3 \
	gd_add_bit.3:gd_add_phase.3 gd_add_bit.3:gd_add_raw.3 \
	gd_add_bit.3:gd_add_recip.3 gd_add_bit.3:gd_add_sbit.3 \
	gd_add_bit.3:gd_add_string.3 gd_add_bit.3:gd_add_window.3 \
	gd_madd_bit.3:gd_madd_carray.3 gd_madd_bit.3:gd_madd_clincom.3 \
	gd_madd_bit.3:gd_madd_const.3 gd_madd_bit.3:gd_madd_cpolynom.3 \
	gd_madd_bit.3:gd_madd_crecip.3 gd_madd_bit.3:gd_madd_divide.3 \
	gd_madd_bit.3:gd_madd_lincom.3 gd_madd_bit.3:gd_madd_linterp.3 \
	gd_madd_bit.3:gd_madd_multiply.3 gd_madd_bit.3:gd_madd_phase.3 \
	gd_madd_bit.3:gd_madd_polynom.3 gd_madd_bit.3:gd_madd_recip.3 \
	gd_madd_bit.3:gd_madd_sbit.3 gd_madd_bit.3:gd_madd_string.3 \
	gd_add.3:gd_madd.3 gd_add_spec.3:gd_madd_spec.3 gd_cbopen.3:gd_open.3 \
	gd_close.3:gd_discard.3 gd_alter_bit.3:gd_alter_carray.3 \
	gd_alter_bit.3:gd_alter_clincom.3 gd_alter_bit.3:gd_alter_const.3 \
	gd_alter_bit.3:gd_alter_cpolynom.3 gd_alter_bit.3:gd_alter_crecip.3 \
	gd_alter_bit.3:gd_alter_divide.3 gd_alter_bit.3:gd_alter_lincom.3 \
	gd_alter_bit.3:gd_alter_linterp.3 gd_alter_bit.3:gd_alter_multiply.3 \
	gd_alter_bit.3:gd_alter_phase.3 gd_alter_bit.3:gd_alter_polynom.3 \
	gd_alter_bit.3:gd_alter_recip.3 gd_alter_bit.3:gd_alter_raw.3 \
	gd_alter_bit.3:gd_alter_sbit.3 gd_alter_bit.3:gd_alter_window.3 \
	gd_alter_spec.3:gd_malter_spec.3 gd_framenum_subset.3:gd_framenum.3 \
	gd_get_carray_slice.3:gd_get_carray.3 gd_put_carray_slice.3:gd_put_carray.3 \
	gd_delete.3:gd_delete_alias.3 gd_hide.3:gd_unhide.3 \
	gd_add_alias.3:gd_madd_alias.3 gd_flush.3:gd_sync.3 \
	gd_flush.3:gd_raw_close.3 $(MAN3ALIASES64)

#man conversion
HTMLMANS=$(addsuffix .html,$(nodist_man_MANS)) \
				 $(addsuffix .html,$(dist_man_MANS))

%.html: %
	./make_html.pl $< > $@

htmlman: $(HTMLMANS)
	for m in $(MAN3ALIASES); do \
		./make_html.pl `echo $$m | $(SED) -e 's/:\\(.*\\).3/ \\1/'` \
			> `echo $$m | $(SED) -e 's/3/3.html/g' | $(SED) -e 's/.*://'`; \
	 done

install-data-hook:
	cd $(DESTDIR)$(mandir)/man3 && \
		for m in $(MAN3ALIASES); do \
		rm -f `echo $$m | $(SED) -e 's/.*://'`; \
		$(LN_S) `echo $$m | $(SED) -e 's/:/ /'`; \
		done

uninstall-hook:
	cd $(DESTDIR)$(mandir)/man3 && \
		for m in $(MAN3ALIASES); do \
		rm -f `echo $$m | $(SED) -e 's/.*://'`; \
		done

clean-local:
	rm -f *~ *.html
	rm -f $(nodist_man_MANS)
