# Copyright (C) 2008-2016 D. V. Wiebe
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
AUTOMAKE_OPTIONS = foreign serial-tests

LDADD=../src/libgetdata.la
AM_CPPFLAGS=${GD_CC_WALL} $(GD_CC_WEXTRA) -I$(top_srcdir)/src

# The enc_*.c files aren't directly used as tests.  They're included by
# corresponding encoding tests.
EXTRA_DIST=test.h enc_add.c enc_complex64.c enc_complex128.c enc_del.c \
					 enc_enoent.c enc_float32.c enc_float64.c enc_get_cont.c enc_int8.c \
					 enc_int16.c enc_int32.c enc_int64.c enc_move_from.c enc_nframes.c \
					 enc_put_offs.c enc_seek.c enc_uint8.c enc_uint16.c enc_uint32.c \
					 enc_uint64.c

ADD_TESTS=add_add add_affix add_alias add_alias_affix add_alias_index \
					add_alias_meta add_alias_name add_alias_ns add_alias_prot \
					add_alias_rdonly add_amb_code7 add_bit add_bit_bitnum \
					add_bit_bitsize add_bit_inaff add_bit_invalid add_bit_numbits \
					add_bit_scalars add_carray add_carray_entry add_carray_type \
					add_clincom add_clincom_nfields add_code add_const add_const_type \
					add_cpolynom add_cpolynom_order add_crecip add_crecip89 \
					add_dangle_dup add_divide add_divide_inaff1 add_divide_inaff2 \
					add_divide_invalid add_dot5 add_dot6 add_dot10 add_duplicate \
					add_format add_indir add_invalid add_lincom add_lincom_affix \
					add_lincom_invalid add_lincom_nfields add_lincom_nfields2 \
					add_linterp add_linterp_inaff add_linterp_invalid add_meta \
					add_meta_alias add_meta_parent add_mplex add_mplex_inaff \
					add_mplex_period add_mplex_scalars add_multiply add_multiply_invalid \
					add_name_len add_ns add_ns_frag add_ns_frag2 add_phase \
					add_phase_inaff add_phase_invalid add_polynom add_polynom_inaff \
					add_polynom_order add_polynom_order2 add_polynom_scalar add_protect \
					add_raw add_raw_include add_raw_invalid add_raw_prot add_raw_spf \
					add_raw_spf_scalar add_raw_sub add_raw_type add_rdonly add_recip \
					add_recip_inaff add_resolv add_sarray add_sarray_dup add_sarray_nil \
					add_sbit add_scalar add_scalar_carray add_scalar_carray_bad \
					add_sindir add_sort add_spec add_spec_affix add_spec_directive \
					add_spec_index \
					add_spec_invalid add_spec_meta add_spec_nil add_spec_prot \
					add_spec_raw_autoenc add_spec_raw_prot add_spec_raw_unkenc \
					add_spec_resolv add_string add_string_affix add_string_dup \
					add_string_entry add_type add_window add_window_inaff add_window_op

ALIAS_TESTS=alias_list alias_list_alias alias_list_missing alias_num \
						alias_num_alias alias_num_missing alias_target alias_target_alias \
						alias_target_missing

ALLOC_TESTS=alloc_affix alloc_callback alloc_clear alloc_entry alloc_tok

ALTER_TESTS=alter_bit_bitnum alter_bit_in alter_bit_numbits alter_bit_scalar \
						alter_carray_len alter_carray_type alter_clincom alter_clincom_32 \
						alter_clincom_nfields alter_clincom_nfields_code \
						alter_clincom_null alter_code alter_const alter_const_c2r \
						alter_const_r2c alter_const_r2r alter_const_type alter_cpolynom \
						alter_cpolynom_null alter_cpolynom_ord alter_cpolynom_ord_code \
						alter_crecip alter_crecip89 alter_crecip89_null alter_crecip_zero \
						alter_divide alter_divide_in alter_entry alter_entry_affix \
						alter_entry_entype alter_entry_hidden alter_entry_lincom \
						alter_entry_lincom_nfields alter_entry_recode \
						alter_entry_recode_recalc alter_entry_sarray alter_entry_scalar1 \
						alter_entry_scalar2a alter_entry_scalar2n alter_entry_scalar3 \
						alter_entry_scalar3c alter_entry_scalar3i alter_entry_scalar3r \
						alter_entry_scalar4 alter_entry_scalar_amb alter_index alter_indir \
						alter_lincom_23 alter_lincom_32 alter_lincom_affix \
						alter_lincom_input alter_lincom_nfields alter_lincom_nfields_code \
						alter_lincom_offset alter_lincom_scalar alter_lincom_slope \
						alter_linterp alter_linterp_in alter_linterp_move alter_mplex \
						alter_mplex_in alter_mplex_scalar alter_mspec alter_mspec_affix \
						alter_multiply alter_phase alter_phase_in alter_phase_scalar \
						alter_polynom_coeff alter_polynom_input alter_polynom_ord \
						alter_polynom_ord2 alter_polynom_ord_code alter_polynom_scalar \
						alter_prot alter_raw_prot alter_raw_spf alter_raw_spf_type \
						alter_raw_type alter_rdonly alter_recip alter_recip_scalar \
						alter_recip_zero alter_sarray alter_sarray_nop alter_sbit \
						alter_scalar_affix alter_sindir alter_spec alter_spec_affix \
						alter_spec_code alter_spec_format alter_spec_meta alter_spec_nil \
						alter_spec_polynom alter_spec_prot alter_spec_rdonly alter_window \
						alter_window_in alter_window_op alter_window_scalar

ASCII_TESTS=ascii_add ascii_complex64 ascii_complex128 ascii_get \
						ascii_get_complex ascii_float32 ascii_float64 ascii_get_get \
						ascii_get_here ascii_get_sub ascii_int8 ascii_int16 ascii_int32 \
						ascii_int64 ascii_nframes ascii_put ascii_put_here ascii_seek \
						ascii_seek_far ascii_sync ascii_uint8 ascii_uint16 ascii_uint32 \
						ascii_uint64

BOF_TESTS=bof bof_bit bof_bit_code bof_code bof_const bof_index bof_lincom \
					bof_lincom_code bof_phase bof_phase_code bof_phase_neg bof_recurse

BZIP_TESTS=bzip_add bzip_complex64 bzip_complex128 bzip_del bzip_enoent \
					 bzip_float32 bzip_float64 bzip_get bzip_get_cont bzip_get_far \
					 bzip_get_get bzip_get_get2 bzip_get_put bzip_int8 bzip_int16 \
					 bzip_int32 bzip_int64 bzip_move_from bzip_move_to bzip_nframes \
					 bzip_put bzip_put_back bzip_put_endian bzip_put_get bzip_put_offs \
					 bzip_put_pad bzip_put_sub bzip_seek bzip_seek_far bzip_sync \
					 bzip_uint8 bzip_uint16 bzip_uint32 bzip_uint64

CALIST_TESTS=calist calist0 calist_free calist_hidden calist_long calist_meta \
						 calist_meta0 calist_meta_free calist_meta_hidden calist_meta_meta \
						 calist_meta_parent calist_null

CLOSE_TESTS=close_bad close_close close_discard close_null

CONVERT_TESTS=convert_complex128_complex64 convert_complex128_float64 \
							convert_complex128_int64 convert_complex128_uint64 \
							convert_complex64_complex128 convert_complex64_float64 \
							convert_complex64_int64 convert_complex64_uint64 \
							convert_float32_complex128 convert_float32_complex64 \
							convert_float32_float64 convert_float32_int16 \
							convert_float32_int32 convert_float32_int64 convert_float32_int8 \
							convert_float32_uint16 convert_float32_uint32 \
							convert_float32_uint64 convert_float32_uint8 \
							convert_float64_complex128 convert_float64_complex64 \
							convert_float64_float32 convert_float64_int16 \
							convert_float64_int32 convert_float64_int64 convert_float64_int8 \
							convert_float64_uint16 convert_float64_uint32 \
							convert_float64_uint64 convert_float64_uint8 \
							convert_int16_complex128 convert_int16_complex64 \
							convert_int16_float32 convert_int16_float64 convert_int16_int32 \
							convert_int16_int64 convert_int16_int8 convert_int16_uint16 \
							convert_int16_uint32 convert_int16_uint64 convert_int16_uint8 \
							convert_int32_complex128 convert_int32_complex64 \
							convert_int32_float32 convert_int32_float64 convert_int32_int16 \
							convert_int32_int64 convert_int32_int8 convert_int32_uint16 \
							convert_int32_uint32 convert_int32_uint64 convert_int32_uint8 \
							convert_int64_complex128 convert_int64_complex64 \
							convert_int64_float32 convert_int64_float64 convert_int64_int16 \
							convert_int64_int32 convert_int64_int8 convert_int64_uint16 \
							convert_int64_uint32 convert_int64_uint64 convert_int64_uint8 \
							convert_int8_complex128 convert_int8_complex64 \
							convert_int8_float32 convert_int8_float64 convert_int8_int16 \
							convert_int8_int32 convert_int8_int64 convert_int8_uint16 \
							convert_int8_uint32 convert_int8_uint64 convert_int8_uint8 \
							convert_uint16_complex128 convert_uint16_complex64 \
							convert_uint16_float32 convert_uint16_float64 \
							convert_uint16_int16 convert_uint16_int32 convert_uint16_int64 \
							convert_uint16_int8 convert_uint16_uint32 convert_uint16_uint64 \
							convert_uint16_uint8 convert_uint32_complex128 \
							convert_uint32_complex64 convert_uint32_float32 \
							convert_uint32_float64 convert_uint32_int16 convert_uint32_int32 \
							convert_uint32_int64 convert_uint32_int8 convert_uint32_uint16 \
							convert_uint32_uint64 convert_uint32_uint8 \
							convert_uint64_complex128 convert_uint64_complex64 \
							convert_uint64_float32 convert_uint64_float64 \
							convert_uint64_int16 convert_uint64_int32 convert_uint64_int64 \
							convert_uint64_int8 convert_uint64_uint16 convert_uint64_uint32 \
							convert_uint64_uint8 convert_uint8_complex128 \
							convert_uint8_complex64 convert_uint8_float32 \
							convert_uint8_float64 convert_uint8_int16 convert_uint8_int32 \
							convert_uint8_int64 convert_uint8_int8 convert_uint8_uint16 \
							convert_uint8_uint32 convert_uint8_uint64

CREAT_TESTS=creat creat_excl creat_rdonly creat_rdonly_exists

CVLIST_TESTS=cvlist cvlist_hidden cvlist_invalid cvlist_long cvlist_meta \
						 cvlist_meta0 cvlist_meta_code cvlist_meta_hidden \
						 cvlist_meta_invalid cvlist_meta cvlist_null cvlist_meta_parent

DEL_TESTS=del_alias del_alias_dangle del_alias_target del_bad_code del_carray \
					del_carray_deref del_const del_const_deref del_const_force del_data \
					del_data_enoent del_data_open del_data_prot del_del del_derived \
					del_derived_after del_derived_force del_divide del_meta \
					del_meta_force del_prot del_rdonly del_ref

DESYNC_TESTS=desync desync_flush desync_path desync_reopen desync_reopen_inv

DFES_TESTS=dfes_bit dfes_divide dfes_lincom dfes_linterp dfes_multiply \
					 dfes_null dfes_phase dfes_raw dfes_recip dfes_zero

ELIST_TESTS=elist_alias elist_hidden elist_noalias elist_parent elist_parent2 \
						elist_scalar elist_type

ENCODE_TESTS=encode_alter encode_alter_all encode_alter_open encode_get \
						 encode_recode encode_recode_open encode_support

ENDIAN_TESTS=endian_alter endian_alter_all endian_alter_arg endian_alter_dprot \
						 endian_alter_fprot endian_alter_index endian_alter_rdonly \
						 endian_alter_sie endian_get endian_index endian_move

ENTRY_TESTS=entry_bad_code entry_bit entry_bit_scalar entry_divide \
						entry_invalid entry_lincom entry_lincom_scalar entry_linterp \
						entry_mplex entry_mplex_scalar entry_multiply entry_phase \
						entry_phase_scalar entry_polynom entry_polynom_scalar entry_raw \
						entry_raw_scalar entry_raw_scalar_code entry_raw_scalar_type \
						entry_scalar_repr entry_recip entry_type entry_type_alias \
						entry_window entry_window_scalar

EOF_TESTS=eof eof_bit eof_bit_code eof_const eof_index eof_lincom \
					eof_lincom_code eof_phase eof_phase_code eof_phase_neg eof_recurse

ERROR_TESTS=error error_error error_num error_short error_verbose \
						error_verbose_prefix

FILE_TESTS=file file_code file_type

FLAC_TESTS=flac_add flac_complex64 flac_complex128 flac_del flac_enoent \
					 flac_float32 flac_float64 flac_get_big flac_get_cont \
					 flac_get_far flac_get_get flac_get_get2 flac_get_int8 \
					 flac_get_int64 flac_get_little flac_get_long flac_int8 \
					 flac_int16 flac_int32 flac_int64 flac_move_from flac_nframes \
					 flac_put_big flac_put_complex128 flac_put_float64 \
					 flac_put_int32 flac_put_little flac_put_offs flac_seek \
					 flac_seek_far flac_sync flac_uint8 flac_uint16 flac_uint32 \
					 flac_uint64

FLIST_TESTS=flist flist0 flist2 flist_hidden flist_invalid flist_meta \
						flist_meta2 flist_meta_hidden flist_meta_invalid flist_type \
						flist_type_hidden flist_type_invalid flist_type_meta \
						flist_type_meta_hidden flist_type_meta_invalid

FLUSH_TESTS=flush_all flush_bad_code flush_flush flush_invalid flush_lincom \
						flush_lincom1 flush_mult flush_raw_close flush_recurse flush_sync

FOFFS_TESTS=foffs_alter foffs_alter_all foffs_alter_dprot foffs_alter_fprot \
						foffs_alter_index foffs_alter_range foffs_alter_rdonly foffs_get \
						foffs_index foffs_move

FRAGMENT_TESTS=fragment_affix fragment_affix_alter fragment_affix_alter2 \
							 fragment_affix_alter_code fragment_affix_alter_dotpx \
							 fragment_affix_alter_index fragment_affix_alter_nop \
							 fragment_affix_alter_nons fragment_affix_alter_ns \
							 fragment_affix_alter_pns fragment_affix_dup \
							 fragment_affix_index fragment_affix_ns fragment_index \
							 fragment_index_alias fragment_name fragment_name_oor \
							 fragment_ndotdots fragment_ndots fragment_ns fragment_ns0 \
							 fragment_ns_add fragment_ns_add0 fragment_ns_alter \
							 fragment_ns_alter_nop fragment_ns_alter_pdel \
							 fragment_ns_del fragment_ns_dot fragment_ns_dotdot \
							 fragment_ns_dotns fragment_ns_nsdot fragment_num \
							 fragment_parent fragment_parent_index fragment_parent_root

GET_TESTS=get64 get_affix get_bad_code get_bit get_carray get_carray_bad \
					get_carray_c2r get_carray_slice get_carray_slice_bounds \
					get_carray_slice_type get_carray_type get_char get_clincom \
					get_complex128 get_complex64 get_const get_const_bad \
					get_const_carray get_const_complex get_const_repr get_const_reprz \
					get_cpolynom get_cpolynom1 get_cpolynom_int get_dim get_dimin \
					get_divide get_divide_ccin get_divide_code get_divide_crin \
					get_divide_crinr get_divide_rcin get_divide_s get_dot get_endian8 \
					get_endian16 get_endian32 get_endian64 get_endian_complex128_arm \
					get_endian_complex128_big get_endian_complex128_little \
					get_endian_complex64_arm get_endian_complex64_big \
					get_endian_complex64_little get_endian_float32_arm \
					get_endian_float32_big get_endian_float32_little \
					get_endian_float64_arm get_endian_float64_big \
					get_endian_float64_little get_ff get_float32 get_float64 get_foffs \
					get_foffs2 get_fs get_here get_here_foffs get_heres \
					get_index_complex get_index_type get_indir get_indir_typein get_int8 \
					get_int16 get_int32 get_int64 get_invalid get_lincom1 get_lincom2 \
					get_lincom2s get_lincom3 get_lincom3s get_lincom_mdt get_lincom_noin \
					get_lincom_non get_lincom_null get_lincom_spf get_lincom_code \
					get_linterp get_linterp1 get_linterp_abs get_linterp_complex \
					get_linterp_empty get_linterp_nodir get_linterp_noin \
					get_linterp_notab get_linterp_sort get_mplex get_mplex_bof \
					get_mplex_complex get_mplex_lb get_mplex_lball get_mplex_nolb \
					get_mplex_s get_mplex_saved get_multiply get_multiply_ccin \
					get_multiply_code get_multiply_crin get_multiply_crinr \
					get_multiply_noin get_multiply_rcin get_multiply_s get_neg get_none \
					get_nonexistent get_null get_off64 get_phase get_phase_affix \
					get_polynom get_polynom_cmpin get_polynom_noin get_range get_recip \
					get_recip_cmpin get_recip_const get_recurse get_reprz get_rofs \
					get_sarray get_sarray_bad get_sarray_slice get_sarray_slice_bounds \
					get_sarray_slice_type get_sarray_type get_sbit get_sf get_sindir \
					get_sindir_code get_sindir_neg get_sindir_none get_sindir_null \
					get_sindir_reprz get_sindir_type get_sindir_typein get_ss get_string \
					get_type get_uint16 get_uint32 get_uint64 get_window get_window_clr \
					get_window_complex get_window_ge get_window_gt get_window_le \
					get_window_lt get_window_ne get_window_s get_window_set get_zero \
					get_zero_complex get_zero_float

GLOBAL_TESTS=global_flags global_name

GZIP_TESTS=gzip_add gzip_complex64 gzip_complex128 gzip_del gzip_enoent \
					 gzip_float32 gzip_float64 gzip_get gzip_get_cont gzip_get_far \
					 gzip_get_get gzip_get_get2 gzip_get_put gzip_int8 gzip_int16 \
					 gzip_int32 gzip_int64 gzip_move_from gzip_move_to gzip_nframes \
					 gzip_put gzip_put_back gzip_put_endian gzip_put_get \
					 gzip_put_nframes gzip_put_off gzip_put_offs gzip_put_pad \
					 gzip_put_sub gzip_seek gzip_seek_far gzip_seek_put gzip_sync \
					 gzip_uint8 gzip_uint16 gzip_uint32 gzip_uint64

HEADER_TESTS=header_complex header_off64t

HIDE_TESTS=hide hide_bad hide_hidden hide_hidden_bad hide_unhide \
					 hide_unhide_bad

INCLUDE_TESTS=include_accmode include_affix include_affix_ns include_auto \
							include_cb include_creat include_format include_ignore \
							include_include include_index include_invalid \
							include_nonexistent include_ndotdots include_ndots include_ns \
							include_ns_dot include_ns_dotdot include_ns_dotns include_ns_nil \
							include_ns_null include_ns_prefix include_pc include_prot \
							include_ref include_ref_code include_ref_type include_sub \
							include_syntax

INDEX_TESTS=index_domain index_empty index_index index_one index_range \
						index_reprz index_s index_scalar index_subset

if INCLUDE_LEGACY_API
LEGACY_TESTS=legacy_error legacy_estring legacy_format legacy_get \
						 legacy_get_put legacy_get_rofs legacy_nframes legacy_nonexistent \
						 legacy_put legacy_spf
endif

LEN_TESTS=len_bad len_carray len_const len_sarray len_string len_type

LZMA_TESTS=lzma_enoent lzma_get lzma_nframes lzma_put lzma_xz_add \
					 lzma_xz_complex64 lzma_xz_complex128 lzma_xz_del lzma_xz_float32 \
					 lzma_xz_float64 lzma_xz_get lzma_xz_get_cont lzma_xz_get_far \
					 lzma_xz_get_get lzma_xz_get_get2 lzma_xz_get_put lzma_xz_int8 \
					 lzma_xz_int16 lzma_xz_int32 lzma_xz_int64 lzma_xz_move_from \
					 lzma_xz_move_to lzma_xz_nframes lzma_xz_offs_clear lzma_xz_put \
					 lzma_xz_put_back lzma_xz_put_endian lzma_xz_put_get \
					 lzma_xz_put_offs lzma_xz_put_pad lzma_xz_seek lzma_xz_seek_far \
					 lzma_xz_sync lzma_xz_uint8 lzma_xz_uint16 lzma_xz_uint32 \
					 lzma_xz_uint64

MADD_TESTS=madd madd_affix madd_alias madd_aliasmeta madd_alias_affix \
					 madd_alias_alias madd_alias_subsub madd_bit madd_bit_invalid \
					 madd_carray madd_clincom madd_clincom_nfields madd_const \
					 madd_cpolynom madd_cpolynom_order madd_crecip madd_crecip89 \
					 madd_divide madd_index madd_indir madd_lincom madd_lincom_invalid \
					 madd_lincom_nfields madd_linterp madd_linterp_invalid madd_mplex \
					 madd_multiply madd_multiply_invalid madd_parent_bad madd_phase \
					 madd_phase_invalid madd_polynom madd_polynom_order madd_raw \
					 madd_recip madd_sarray madd_sarray_dup madd_sbit madd_sindir \
					 madd_spec madd_spec_directive madd_spec_invalid madd_spec_parent \
					 madd_spec_resolv madd_string madd_string_dup madd_subsub madd_window

MATCH_TESTS=match_fragment match_pcre match_pcre_bad match_pcre_caseless \
						match_pcre_ext match_pcre_js match_pcre_utf8 match_regex \
						match_regex_bad match_regex_ext match_regex_icase

MFLUSH_TESTS=mflush mflush_affix mflush_amb_code mflush_carray mflush_hex \
						 mflush_indir mflush_mplex mflush_ns mflush_pretty mflush_prot \
						 mflush_ref mflush_repr mflush_repr9 mflush_reprz mflush_sindir \
						 mflush_spec mflush_string mflush_type mflush_type4 mflush_windop

MOVE_TESTS=move_affix move_affix_dup move_affix_meta move_affix_updb \
					 move_alias move_code move_data_enc_ar move_data_enc_ra \
					 move_data_enc_un move_data_endian move_data_foffs \
					 move_data_foffs_neg move_data_nop move_data_prot move_entype \
					 move_index move_meta move_move move_nop move_ns move_nsns \
					 move_protect move_rdonly move_subdir move_unkenc

NAME_TESTS=name_affix name_affix_bad name_alias name_code name_dangle \
					 name_dot5 name_dot5r name_dot9 name_dot10 name_dup name_index \
					 name_meta name_meta2 name_move name_move_alias name_name name_ns \
					 name_ns2 name_nsdot name_prot name_rdonly name_updb name_updb_affix \
					 name_updb_alias name_updb_carray name_updb_const \
					 name_updb_const_alias name_updb_sarray

NATIVE_TESTS=native_bit native_code native_const native_index native_lincom \
						 native_lincom_cmpin native_lincom_cmpscal native_linterp \
						 native_linterp_cmp native_mult native_mult1 native_mult2 \
						 native_polynom native_polynom_cmpin native_polynom_cmpscal \
						 native_raw native_phase native_recip native_recip_cmpin \
						 native_recip_cmpscal native_recurse native_reprz native_sbit \
						 native_string

NENTRIES_TESTS=nentries_alias nentries_entype nentries_hidden nentries_noalias \
							 nentries_scalar

NFIELDS_TESTS=nfields_hidden nfields_invalid nfields_nfields nfields_type \
							nfields_type_hidden nfields_type_invalid nfields_vector \
							nfields_vector_hidden nfields_vector_invalid

NFRAMES_TESTS=nframes64 nframes_empty nframes_invalid nframes_nframes \
							nframes_off64 nframes_spf

NMETA_TESTS=nmeta nmeta_hidden nmeta_invalid nmeta_parent nmeta_type \
						nmeta_type_hidden nmeta_type_invalid nmeta_type_parent \
						nmeta_parent_meta nmeta_vectors nmeta_vectors_del \
						nmeta_vectors_hidden nmeta_vectors_invalid nmeta_vectors_parent

OPEN_TESTS=open_abs open_cb_abort open_cb_cont open_cb_ignore open_cb_invalid \
					 open_cb_rescan open_cb_rescan_alloc open_eaccess open_invalid \
					 open_nonexistent open_notdirfile open_open open_rofs open_sym_al \
					 open_sym_at open_sym_c open_sym_cl open_sym_ct open_sym_d \
					 open_sym_l open_sym_p open_sym_pl open_sym_pt

PARSE_TESTS=parse_alias parse_alias_code parse_alias_dup parse_alias_loop \
						parse_alias_meta parse_alias_missing parse_alias_ncols \
						parse_badline parse_bit parse_bit4 parse_bit_bitnum \
						parse_bit_bitsize parse_bit_ncols parse_bit_numbits \
						parse_bit_scalar parse_carray parse_carray_long parse_carray_ncols \
						parse_carray_type parse_const parse_const_complex \
						parse_const_ncols parse_const_type parse_divide parse_double \
						parse_duplicate parse_duplicate_ignore parse_endian_bad \
						parse_endian_bad2 parse_endian_big parse_endian_force \
						parse_endian_little parse_endian_slash parse_eol parse_esc \
						parse_foffs parse_foffs_include parse_foffs_slash parse_hex \
						parse_hidden parse_hidden_field parse_hidden_frag \
						parse_hidden_meta parse_huge parse_include parse_include_absolute \
						parse_include_absrel parse_include_affix_bad \
						parse_include_affix_ref parse_include_dir parse_include_loop \
						parse_include_nodir parse_include_nonexistent parse_include_ns \
						parse_include_ns2 parse_include_nsabs parse_include_nsinh \
						parse_include_nspop parse_include_nsrabs parse_include_nsrainh \
						parse_include_nsrinh parse_include_nsroot parse_include_prefix \
						parse_include_prefix_dup parse_include_preprefix parse_include_ref \
						parse_include_relabs parse_include_relrel parse_include_slash \
						parse_include_suffix parse_include_sufsuffix parse_index \
						parse_indir parse_lincom parse_lincom_ncols1 parse_lincom_ncols2 \
						parse_lincom_nfields parse_lincom_nofields parse_lincom_non \
						parse_lincom_non_ncols parse_lincom_scalar parse_linterp \
						parse_linterp_ncols parse_literal_cmpbad parse_literal_fltcmp \
						parse_literal_fltcmp0 parse_literal_intcmp parse_literal_intcmp0 \
						parse_literal_uintcmp parse_literal_uintcmp0 parse_malias \
						parse_malias_dup parse_malias_meta parse_meta parse_meta_affix \
						parse_meta_alias parse_meta_frag parse_meta_implicit \
						parse_meta_implicit2 parse_meta_implicit_affix parse_meta_index \
						parse_meta_index2 parse_meta_jump parse_meta_malias \
						parse_meta_meta parse_meta_ncols parse_meta_parent parse_meta_raw \
						parse_mplex parse_mplex_ncols parse_mplex_neg parse_mplex_nomax \
						parse_mplex_scalar parse_multiply parse_multiply_ncols parse_name \
						parse_name_dot parse_name_ext parse_ncols parse_ns parse_ns_dot \
						parse_ns_dotdot parse_ns_dotdotns parse_ns_dotns parse_ns_include \
						parse_ns_include_ns parse_ns_ndotdots parse_ns_ndots \
						parse_ns_nsdot parse_ns_nsdotdot parse_nsf parse_nsf_abs \
						parse_nsf_alias parse_nsf_dot parse_nsf_hide parse_nsf_index \
						parse_nsf_meta parse_nsf_meta2 parse_nsf_ref parse_nsf_refabs \
						parse_nsf_root parse_nsf_sub parse_octal_zero parse_phase \
						parse_phase_ncols parse_phase_scalar parse_polynom \
						parse_polynom_ncols1 parse_polynom_ncols2 parse_polynom_scalar \
						parse_protect_all parse_protect_bad parse_protect_data \
						parse_protect_format parse_protect_none parse_quote \
						parse_quote_mismatch parse_raw parse_raw_char parse_raw_char_bad \
						parse_raw_ncols parse_raw_scalar parse_raw_spf parse_raw_type \
						parse_recip parse_recip_ncols parse_ref parse_ref_nonexistent \
						parse_ref_type parse_sarray parse_sarray_long parse_sarray_ncols \
						parse_sbit parse_scalar1 parse_scalar2 parse_scalar_repr \
						parse_sindir parse_sort parse_string parse_string_ncols \
						parse_string_null parse_utf8 parse_utf8_invalid parse_utf8_zero \
						parse_version parse_version_89 parse_version_98 \
						parse_version_include parse_version_permissive parse_version_p8 \
						parse_version_p9 parse_version_slash parse_whitespace parse_window \
						parse_window_ncols parse_window_op parse_window_scalar

PROTECT_TESTS=protect_alter protect_alter_all protect_alter_arg \
							protect_alter_index protect_alter_rdonly protect_get \
							protect_index

PUT_TESTS=put64 put_bad_code put_bit put_bit_repr put_bof put_carray \
					put_carray_client put_carray_bad put_carray_bounds put_carray_rdonly \
					put_carray_slice put_carray_type put_char put_clincom1 \
					put_complex128 put_complex64 put_const put_const_protect \
					put_cpolynom put_crecip put_divide put_endian8 put_endian16 \
					put_endian32 put_endian64 put_endian_complex128_arm \
					put_endian_complex128_big put_endian_complex128_little \
					put_endian_complex64_arm put_endian_complex64_big \
					put_endian_complex64_little put_endian_float32_arm \
					put_endian_float32_big put_endian_float32_little \
					put_endian_float64_arm put_endian_float64_big \
					put_endian_float64_little put_ff put_float32 put_float64 put_foffs \
					put_fs put_here put_heres put_indir put_int8 put_int16 put_int32 \
					put_int64 put_invalid put_lincom1 put_lincom2 put_lincom_noin \
					put_lincom_repr put_linterp put_linterp_cmp put_linterp_noin \
					put_linterp_nomono put_linterp_notab put_linterp_repr \
					put_linterp_reverse put_mplex put_mplex_complex put_mplex_repr \
					put_multiply put_neg put_nofile put_null put_off64 put_phase \
					put_phase_noin put_phase_repr put_polynom1 put_polynom2 \
					put_polynom_noin put_polynom_repr put_protect put_range put_rdonly \
					put_recip put_recip_repr put_recurse put_repr put_rofs put_sarray \
					put_sarray_bad put_sarray_bounds put_sarray_rdonly put_sarray_slice \
					put_sarray_type put_sbit put_scalar put_sf put_sindir put_ss \
					put_string put_string_protect put_string_type put_sub put_type \
					put_uint16 put_uint32 put_uint64 put_window put_zero

REF_TESTS=ref ref_empty ref_get ref_none ref_set ref_set_code ref_set_prot \
					ref_set_rdonly ref_set_type ref_two

REPR_TESTS=repr_a repr_bad repr_float32 repr_float64 repr_i repr_int16 \
					 repr_int32 repr_int64 repr_int8 repr_m repr_r repr_real_a \
					 repr_real_i repr_real_m repr_real_r repr_uint16 repr_uint32 \
					 repr_uint64 repr_uint8

SALIST_TESTS=salist salist0 salist_hidden salist_long salist_meta salist_meta0 \
						 salist_meta_free salist_meta_hidden salist_meta_meta \
						 salist_meta_parent

SEEK_TESTS=seek64 seek_arg seek_code seek_cur seek_dim seek_end seek_far \
					 seek_foffs seek_foffs2 seek_index seek_index_end seek_lincom \
					 seek_mplex seek_mult seek_neg seek_phase seek_range seek_range2 \
					 seek_range_end seek_recurse seek_set seek_sub

SIE_TESTS=sie_err_open sie_get_big sie_get_header sie_get_little sie_move_from \
					sie_move_to sie_nframes_big sie_nframes_little sie_put_append \
					sie_put_append2 sie_put_back sie_put_big sie_put_header \
					sie_put_little sie_put_many sie_put_newo sie_put_newo0 sie_put_pad \
					sie_put_pad0 sie_put_trunc sie_put_trunc2 sie_put_trunc_nf sie_seek \
					sie_seek_far sie_sync

SLIM_TESTS=slim_get slim_nframes slim_seek slim_seek_far

SVLIST_TESTS=svlist svlist0 svlist2 svlist_hidden svlist_invalid svlist_long \
						 svlist_meta svlist_meta0 svlist_meta_hidden svlist_meta_invalid \
						 svlist_meta_meta svlist_meta_parent

SPF_TESTS=spf_alias spf_alias_missing spf_alias_meta spf_const spf_divide \
					spf_lincom spf_multiply spf_polynom spf_raw spf_recip spf_recurse

TABLE_TESTS=table table_code table_type

TELL_TESTS=tell tell64 tell_code tell_dim tell_multi_div tell_multi_lincom \
					 tell_recurse tell_sub

TOK_TESTS=tok_arg tok_escape tok_quote

TRUNC_TESTS=trunc trunc_dir trunc_rdonly trunc_rofs trunc_truncsub

VALID_TESTS=valid_reprz

UNCLUDE_TESTS=unclude unclude_del unclude_index unclude_keep unclude_move \
							unclude_open unclude_prot unclude_rdonly

VERSION_TESTS=version_0 version_0_write version_1 version_1_write version_2 \
							version_2_write version_3 version_3_write version_4 \
							version_4_write version_5 version_5_strict version_5_write \
							version_6 version_6_strict version_6_write version_7 \
							version_7_strict version_7_write version_8 version_8_strict \
							version_8_write version_9 version_9_strict version_9_write

VLIST_TESTS=vlist vlist_alias vlist_hidden vlist_invalid vlist_meta \
						vlist_meta_hidden vlist_meta_invalid

ZZIP_TESTS=zzip_data zzip_get zzip_get_get zzip_nframes zzip_seek zzip_seek_far

ZZSLIM_TESTS=zzslim_get zzslim_nframes zzslim_seek zzslim_seek_far

TESTS=$(ADD_TESTS)      $(ALIAS_TESTS)    $(ALLOC_TESTS)    $(ALTER_TESTS)    \
			$(ASCII_TESTS)    $(BOF_TESTS)      $(BZIP_TESTS)     $(CALIST_TESTS)   \
			$(CLOSE_TESTS)    $(CONVERT_TESTS)  $(CREAT_TESTS)    $(CVLIST_TESTS)   \
			$(DEL_TESTS)      $(DFES_TESTS)     $(DESYNC_TESTS)   $(ELIST_TESTS)    \
			$(ENCODE_TESTS)   $(ENDIAN_TESTS)   $(ENTRY_TESTS)    $(EOF_TESTS)      \
			$(ERROR_TESTS)    $(FILE_TESTS)     $(FLAC_TESTS)     $(FLIST_TESTS)    \
			$(FLUSH_TESTS)    $(FOFFS_TESTS)    $(FRAGMENT_TESTS) $(GET_TESTS)      \
			$(GLOBAL_TESTS)   $(GZIP_TESTS)     $(HEADER_TESTS)   $(HIDE_TESTS)     \
			$(INCLUDE_TESTS)  $(INDEX_TESTS)    $(LEGACY_TESTS)   $(LEN_TESTS)      \
			$(LZMA_TESTS)     $(MADD_TESTS)     $(MATCH_TESTS)    $(MFLUSH_TESTS)   \
			$(MOVE_TESTS)     $(NAME_TESTS)     $(NATIVE_TESTS)   $(NENTRIES_TESTS) \
			$(NFIELDS_TESTS)  $(NFRAMES_TESTS)  $(NMETA_TESTS)    $(OPEN_TESTS)     \
			$(PARSE_TESTS)    $(PROTECT_TESTS)  $(PUT_TESTS)      $(REF_TESTS)      \
			$(REPR_TESTS)     $(SALIST_TESTS)   $(SEEK_TESTS)     $(SIE_TESTS)      \
			$(SLIM_TESTS)     $(SPF_TESTS)      $(SVLIST_TESTS)   $(TABLE_TESTS)    \
			$(TELL_TESTS)     $(TOK_TESTS)      $(TRUNC_TESTS)    $(UNCLUDE_TESTS)  \
			$(VALID_TESTS)    $(VERSION_TESTS)  $(VLIST_TESTS)    $(ZZIP_TESTS)     \
			$(ZZSLIM_TESTS)

check_PROGRAMS=$(TESTS)

# fast tests
.PHONY: fastcheck
fastcheck:
	touch $(TESTS)
	${MAKE} check

# valgrind
valgrind.log: $(TESTS)
	rm -f $@; \
		for x in $^; do ( \
		echo $$x:; \
		sed -e 's/exec "/exec valgrind --leak-check=full --track-origins=yes --track-fds=yes --suppressions=valgrind.suppressions "/' $$x | sh; \
		echo $$x = $$? ); done &> $@;

# clean up after the tests
check-local: check-TESTS
	$(MAKE) getdata-clean-dir

getdata-clean-dir:
	rm -rf dirfile

clean-local:
	$(MAKE) getdata-clean-dir
	rm -rf *~ *.gcno *.gcda
