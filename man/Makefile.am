# (C) 2008 D. V. Wiebe
#
##########################################################################
#
# This file is part of the GetData project.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# GetData is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with GetData; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
#
AUTOMAKE_OPTIONS = foreign

if HAVE_OFF64_T
LFS_transitional_mans = getdata64.3 get_frameoffset64.3 get_nframes64.3 \
												putdata64.3
INSTALL_DATA_HOOK64 = install-data-hook64
endif

dist_man_MANS = dirfile_add.3 dirfile_add_bit.3 dirfile_add_spec.3 \
								dirfile_alter_bit.3 dirfile_alter_encoding.3 \
								dirfile_alter_endianness.3 dirfile_alter_entry.3 \
								dirfile_alter_frameoffset.3 dirfile_alter_spec.3 \
								dirfile_cbopen.3 dirfile_close.3 get_constant.3 put_constant.3 \
								get_constants.3 getdata.3 putdata.3 dirfile_delete.3 \
								get_encoding.3 get_endianness.3 get_entry.3 get_error.3 \
								get_error_string.3 get_field_list.3 get_field_list_by_type.3 \
								dirfile_flush.3 get_fragmentname.3 get_frameoffset.3 \
								dirfile_free_entry_strings.3 dirfile_include.3 \
								dirfile_madd_bit.3 get_mconstants.3 dirfile_metaflush.3 \
								get_mfield_list.3 get_mfield_list_by_type.3 dirfile_move.3 \
								get_mstrings.3 get_mvector_list.3 dirfilename.3 get_nfields.3 \
								get_nfields_by_type.3 get_nfragments.3 get_nframes.3 \
								get_nmfields.3 get_nmfields_by_type.3 get_nmvectors.3 \
								get_nvectors.3 dirfile_reference.3 get_spf.3 get_string.3 \
								put_string.3 get_strings.3 get_vector_list.3 dirfile.5 \
								dirfile-encoding.5 dirfile-format.5 $(LFS_transitional_mans)

install-data-hook: ${INSTALL_DATA_HOOK64}
	cd $(DESTDIR)$(mandir)/man3 && \
		$(LN_S) -f dirfile_add_bit.3 dirfile_add_const.3 && \
		$(LN_S) -f dirfile_add_bit.3 dirfile_add_lincom.3 && \
		$(LN_S) -f dirfile_add_bit.3 dirfile_add_linterp.3 && \
		$(LN_S) -f dirfile_add_bit.3 dirfile_add_multiply.3 && \
		$(LN_S) -f dirfile_add_bit.3 dirfile_add_phase.3 && \
		$(LN_S) -f dirfile_add_bit.3 dirfile_add_raw.3 && \
		$(LN_S) -f dirfile_add_bit.3 dirfile_add_string.3
	cd $(DESTDIR)$(mandir)/man3 && \
		$(LN_S) -f dirfile_madd_bit.3 dirfile_add_const.3 && \
		$(LN_S) -f dirfile_madd_bit.3 dirfile_add_lincom.3 && \
		$(LN_S) -f dirfile_madd_bit.3 dirfile_add_linterp.3 && \
		$(LN_S) -f dirfile_madd_bit.3 dirfile_add_multiply.3 && \
		$(LN_S) -f dirfile_madd_bit.3 dirfile_add_phase.3 && \
		$(LN_S) -f dirfile_madd_bit.3 dirfile_add_string.3
	cd $(DESTDIR)$(mandir)/man3 && \
		$(LN_S) -f dirfile_add.3 dirfile_madd.3
	cd $(DESTDIR)$(mandir)/man3 && \
		$(LN_S) -f dirfile_add_spec.3 dirfile_madd_spec.3
	cd $(DESTDIR)$(mandir)/man3 && \
		$(LN_S) -f dirfile_cbopen.3 dirfile_open.3
	cd $(DESTDIR)$(mandir)/man3 && \
		$(LN_S) -f dirfile_close.3 dirfile_discard.3
	cd $(DESTDIR)$(mandir)/man3 && \
		$(LN_S) -f dirfile_alter_bit.3 dirfile_alter_const.3 && \
		$(LN_S) -f dirfile_alter_bit.3 dirfile_alter_lincom.3 && \
		$(LN_S) -f dirfile_alter_bit.3 dirfile_alter_linterp.3 && \
		$(LN_S) -f dirfile_alter_bit.3 dirfile_alter_multiply.3 && \
		$(LN_S) -f dirfile_alter_bit.3 dirfile_alter_phase.3 && \
		$(LN_S) -f dirfile_alter_bit.3 dirfile_alter_raw.3
	cd $(DESTDIR)$(mandir)/man3 && \
		$(LN_S) -f dirfile_alter_spec.3 dirfile_malter_spec.3

install-data-hook64:
	cd $(DESTDIR)$(mandir)/man3 && \
		$(LN_S) -f get_frameoffset64.3 dirfile_alter_frameoffset64.3

clean-local:
	rm -rf *~ 
