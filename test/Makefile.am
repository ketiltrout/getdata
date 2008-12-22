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

if GNU_C_COMPILER
WEXTRA=-Wextra
endif

if HAVE_OFF64_T
GET_OFF64=get_off64
NF_OFF64=nframes_off64
PUT_OFF64=put_off64
endif

LDADD=../src/libgetdata.la
INCLUDES=-Wall $(WEXTRA) -I$(top_srcdir)/src -D__TEST__=\"$@\"

ADD_TESTS=add add_bit add_bit_bitnum add_bit_bitsize add_bit_invalid \
					add_bit_numbits add_code add_const add_duplicate add_format \
					add_invalid add_lincom add_lincom_invalid add_lincom_nfields \
					add_linterp add_linterp_invalid add_many add_multiply \
					add_multiply_invalid add_phase add_phase_invalid add_protect add_raw \
					add_raw_invalid add_raw_spf add_raw_type add_rdonly add_sort \
					add_spec add_spec_directive add_spec_invalid add_string add_type

ALTER_TESTS=alter_bit_bitnum alter_bit_numbits alter_const alter_entry \
						alter_entry_recode alter_lincom_input alter_lincom_offset \
						alter_lincom_slope alter_linterp alter_linterp_move alter_mspec \
						alter_multiply alter_phase alter_raw_spf alter_raw_type alter_spec

ASCII_TESTS=ascii_get ascii_nframes ascii_put

if TEST_BZIP2
BZIP_TESTS=bzip_get bzip_get_get bzip_move_from bzip_nframes bzip_put
endif

CVLIST_TESTS=cvlist cvlist_invalid cvlist_meta cvlist_meta_invalid

CLOSE_TESTS=close close_bad close_discard close_null

CONVERT_TESTS=convert_uint8_int8 convert_uint8_uint16 convert_uint8_int16 \
							convert_uint8_uint32 convert_uint8_int32 convert_uint8_float32 \
							convert_uint8_uint64 convert_uint8_int64 convert_uint8_float64 \
							convert_int8_uint8 convert_int8_uint16 convert_int8_int16 \
							convert_int8_uint32 convert_int8_int32 convert_int8_float32 \
							convert_int8_uint64 convert_int8_int64 convert_int8_float64 \
							convert_uint16_uint8 convert_uint16_int8 convert_uint16_int16 \
							convert_uint16_uint32 convert_uint16_int32 \
							convert_uint16_float32 convert_uint16_uint64 \
							convert_uint16_int64 convert_uint16_float64 convert_int16_uint8 \
							convert_int16_int8 convert_int16_uint16 convert_int16_uint32 \
							convert_int16_int32 convert_int16_float32 convert_int16_uint64 \
							convert_int16_int64 convert_int16_float64 convert_uint32_uint8 \
							convert_uint32_int8 convert_uint32_uint16 convert_uint32_int16 \
							convert_uint32_int32 convert_uint32_float32 \
							convert_uint32_uint64 convert_uint32_int64 \
							convert_uint32_float64 convert_int32_uint8 convert_int32_int8 \
							convert_int32_uint16 convert_int32_int16 convert_int32_uint32 \
							convert_int32_float32 convert_int32_uint64 convert_int32_int64 \
							convert_int32_float64 convert_uint64_uint8 convert_uint64_int8 \
							convert_uint64_uint16 convert_uint64_int16 convert_uint64_uint32 \
							convert_uint64_int32 convert_uint64_float32 convert_uint64_int64 \
							convert_uint64_float64 convert_int64_uint8 convert_int64_int8 \
							convert_int64_uint16 convert_int64_int16 convert_int64_uint32 \
							convert_int64_int32 convert_int64_float32 convert_int64_uint64 \
							convert_int64_float64 convert_float32_uint8 convert_float32_int8 \
							convert_float32_uint16 convert_float32_int16 \
							convert_float32_uint32 convert_float32_int32 \
							convert_float32_uint64 convert_float32_int64 \
							convert_float32_float64 convert_float64_uint8 \
							convert_float64_int8 convert_float64_uint16 \
							convert_float64_int16 convert_float64_uint32 \
							convert_float64_int32 convert_float64_uint64 \
							convert_float64_int64 convert_float64_float32

CREAT_TESTS=creat creat_excl creat_rdonly

DEL_TESTS=del del_const del_const_deref del_const_force del_data del_derived \
					del_derived_force del_meta del_meta_force

DFES_TESTS=dfes_bit dfes_lincom dfes_linterp dfes_multiply dfes_null \
					 dfes_phase dfes_raw dfes_zero

ENCODE_TESTS=encode_alter encode_get encode_move

ENDIAN_TESTS=endian_alter endian_get endian_move

ENTRY_TESTS=entry_bad_code entry_bit entry_bit_scalar entry_invalid \
						entry_lincom entry_lincom_scalar entry_linterp entry_multiply \
						entry_phase entry_phase_scalar entry_raw entry_raw_scalar \
						entry_raw_scalar_code entry_raw_scalar_type entry_type

ERROR_TESTS=error error_error error_short

FILE_TESTS=file file_code file_type

FLIST_TESTS=flist flist_invalid flist_meta flist_meta_invalid flist_type \
						flist_type_invalid flist_type_meta flist_type_meta_invalid

FLUSH_TESTS=flush flush_all flush_bad_code flush_invalid flush_meta

FOFFS_TESTS=foffs_alter foffs_get foffs_move

FRAGMENT_TESTS=fragment_index fragment_name fragment_name_oor

GET_TESTS=get64 get_bad_code get_bit get_char get_endian8 get_endian16 \
					get_endian32 get_endian64 get_endian_float32 get_endian_float64 \
					get_ff get_float32 get_float64 get_foffs get_fs get_int8 get_int16 \
					get_int32 get_int64 get_invalid get_lincom1 get_lincom2 get_lincom3 \
					get_lincom_noin get_linterp get_linterp_noin get_linterp_notab \
					get_multiply get_multiply_noin get_nonexistent get_null $(GET_OFF64) \
					get_phase get_recurse get_sf get_ss get_type get_uint16 get_uint32 \
					get_uint64

GLOBAL_TESTS=global_name global_ref global_ref_empty global_ref_get \
						 global_ref_set

if TEST_GZIP
GZIP_TESTS=gzip_get gzip_get_get gzip_move_from gzip_nframes gzip_put
endif

INCLUDE_TESTS=include include_accmode include_auto include_cb include_creat \
							include_ignore include_index include_invalid include_nonexistent \
							include_pc include_ref include_syntax

if INCLUDE_LEGACY_API
LEGACY_TESTS=legacy_get legacy_get_put legacy_get_rofs legacy_nframes \
			legacy_nonexistent legacy_put legacy_spf
endif

MADD_TESTS=madd madd_bit madd_bit_invalid madd_lincom madd_lincom_invalid \
					 madd_linterp madd_linterp_invalid madd_multiply \
					 madd_multiply_invalid madd_phase madd_phase_invalid madd_spec \
					 madd_spec_directive madd_spec_invalid

MOVE_TESTS=move move_data_enc_ar move_data_enc_ra move_data_endian \
					 move_data_foffs move_data_nop move_index move_protect move_subdir

NAME_TESTS=name name_move

NFIELDS_TESTS=nfields nfields_invalid nfields_type nfields_type_invalid

NFRAGMENTS_TESTS=nfragments

NFRAMES_TESTS=nframes nframes64 nframes_empty nframes_invalid $(NF_OFF64) \
							nframes_spf

NMETA_TESTS=nmeta nmeta_invalid nmeta_parent nmeta_type nmeta_type_invalid \
						nmeta_type_parent nmeta_vectors nmeta_vectors_invalid \
						nmeta_vectors_parent

NVECTORS_TESTS=nvectors nvectors_invalid

OPEN_TESTS=open open_cb_abort open_cb_cont open_cb_ignore open_cb_invalid \
					 open_cb_rescan open_eaccess open_nonexistent open_notdirfile

PARENT_TESTS=parent

PARSE_TESTS=parse_badline parse_bit parse_bit4 parse_bit_bitnum \
						parse_bit_bitsize parse_bit_ncols parse_bit_numbits \
						parse_bit_scalar parse_const parse_const_ncols parse_duplicate \
						parse_duplicate_ignore parse_endian_bad parse_endian_big \
						parse_endian_force parse_endian_little parse_endian_slash \
						parse_eol parse_foffs parse_foffs_include parse_foffs_slash \
						parse_include parse_include_nonexistent parse_include_slash \
						parse_index parse_lincom parse_lincom_ncols1 parse_lincom_ncols2 \
						parse_lincom_nfields parse_lincom_nofields parse_lincom_scalar \
						parse_linterp parse_linterp_ncols parse_meta parse_meta_index \
						parse_meta_parent parse_meta_raw parse_multiply \
						parse_multiply_ncols parse_name parse_name_dot parse_name_ext \
						parse_name_pedantic parse_ncols parse_phase parse_phase_ncols \
						parse_phase_scalar parse_protect_all parse_protect_bad \
						parse_protect_data parse_protect_format parse_protect_none \
						parse_quote parse_quote_mismatch parse_raw parse_raw_char \
						parse_raw_ncols parse_raw_scalar parse_raw_spf parse_raw_type \
						parse_ref parse_ref_nonexistent parse_sort parse_string \
						parse_string_ncols parse_string_null parse_version \
						parse_version_include parse_version_pedantic parse_version_slash \
						parse_whitespace

PROTECT_TESTS=protect_alter protect_get

PUT_TESTS=put64 put_bad_code put_bit put_bof put_char put_const \
					put_const_protect put_endian8 put_endian16 put_endian32 put_endian64 \
					put_endian_float32 put_endian_float64 put_ff put_float32 put_float64 \
					put_foffs put_fs put_int8 put_int16 put_int32 put_int64 put_invalid \
					put_lincom1 put_lincom2 put_lincom_noin put_linterp put_linterp_noin \
					put_linterp_notab put_multiply put_null $(PUT_OFF64) put_phase \
					put_phase_noin put_protect put_rdonly put_recurse put_rofs put_sf \
					put_ss put_string put_string_protect put_type put_uint16 put_uint32 \
					put_uint64

REF_TESTS=ref ref_none ref_two

if TEST_SLIM
SLIM_TESTS=slim_get slim_nframes slim_put
endif

SVLIST_TESTS=svlist svlist_invalid svlist_meta svlist_meta_invalid

SPF_TESTS=spf spf_lincom spf_multiply spf_recurse

TRUNC_TESTS=trunc trunc_rdonly trunc_rofs

UNCLUDE_TESTS=unclude unclude_del unclude_move

VLIST_TESTS=vlist vlist_invalid vlist_meta vlist_meta_invalid

TESTS=$(ADD_TESTS)        $(ALTER_TESTS)      $(ASCII_TESTS)      \
			$(BZIP_TESTS)       $(CVLIST_TESTS)     $(CLOSE_TESTS)      \
			$(CONVERT_TESTS)    $(CREAT_TESTS)      $(DEL_TESTS)        \
			$(DFES_TESTS)       $(ENCODE_TESTS)     $(ENDIAN_TESTS)     \
			$(ENTRY_TESTS)      $(ERROR_TESTS)      $(FILE_TESTS)       \
			$(FLIST_TESTS)      $(FLUSH_TESTS)      $(FOFFS_TESTS)      \
			$(FRAGMENT_TESTS)   $(GET_TESTS)        $(GLOBAL_TESTS)     \
			$(GZIP_TESTS)       $(INCLUDE_TESTS)    $(LEGACY_TESTS)     \
			$(MADD_TESTS)       $(MOVE_TESTS)       $(NAME_TESTS)       \
			$(NFIELDS_TESTS)    $(NFRAGMENTS_TESTS) $(NFRAMES_TESTS)    \
			$(NMETA_TESTS)      $(NVECTORS_TESTS)   $(OPEN_TESTS)       \
			$(PARENT_TESTS)     $(PARSE_TESTS)      $(PROTECT_TESTS)    \
			$(PUT_TESTS)        $(REF_TESTS)        $(SLIM_TESTS)       \
			$(SPF_TESTS)        $(SVLIST_TESTS)     $(TRUNC_TESTS)      \
			$(UNCLUDE_TESTS)    $(VLIST_TESTS)

check_PROGRAMS=$(TESTS)

# clean up after the tests
check-local: check-TESTS
	$(MAKE) getdata-clean-dirs

getdata-clean-dirs:
	rm -rvf *odirfile

clean-local:
	$(MAKE) getdata-clean-dirs
	rm -rf *~ 
