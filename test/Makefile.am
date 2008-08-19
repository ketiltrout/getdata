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
# You should have received a copy of the GNU General Public
# License along with GetData; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
# 02111-1307 USA.
#
AUTOMAKE_OPTIONS = foreign

if HAVE_OFF64_T
GET_OFF64=get_off64
NF_OFF64=nframes_off64
PUT_OFF64=put_off64
endif

LDADD=../src/libgetdata.la
INCLUDES=-Wall -I$(top_srcdir)/src -D__TEST__=\"$@\"
TESTS=convert_uint8_int8      convert_uint8_uint16   convert_uint8_int16     \
			convert_uint8_uint32    convert_uint8_int32    convert_uint8_float32   \
			convert_uint8_uint64    convert_uint8_int64    convert_uint8_float64   \
			convert_int8_uint8      convert_int8_uint16    convert_int8_int16      \
			convert_int8_uint32     convert_int8_int32     convert_int8_float32    \
			convert_int8_uint64     convert_int8_int64     convert_int8_float64    \
      convert_uint16_uint8    convert_uint16_int8    convert_uint16_int16    \
			convert_uint16_uint32   convert_uint16_int32   convert_uint16_float32  \
			convert_uint16_uint64   convert_uint16_int64   convert_uint16_float64  \
			convert_int16_uint8     convert_int16_int8     convert_int16_uint16    \
			convert_int16_uint32    convert_int16_int32    convert_int16_float32   \
			convert_int16_uint64    convert_int16_int64    convert_int16_float64   \
      convert_uint32_uint8    convert_uint32_int8    convert_uint32_uint16   \
			convert_uint32_int16    convert_uint32_int32   convert_uint32_float32  \
			convert_uint32_uint64   convert_uint32_int64   convert_uint32_float64  \
			convert_int32_uint8     convert_int32_int8     convert_int32_uint16    \
			convert_int32_int16     convert_int32_uint32   convert_int32_float32   \
			convert_int32_uint64    convert_int32_int64    convert_int32_float64   \
      convert_uint64_uint8    convert_uint64_int8    convert_uint64_uint16   \
			convert_uint64_int16    convert_uint64_uint32  convert_uint64_int32    \
			convert_uint64_float32  convert_uint64_int64   convert_uint64_float64  \
			convert_int64_uint8     convert_int64_int8     convert_int64_uint16    \
			convert_int64_int16     convert_int64_uint32   convert_int64_int32     \
			convert_int64_float32   convert_int64_uint64   convert_int64_float64   \
			convert_float32_uint8   convert_float32_int8   convert_float32_uint16  \
			convert_float32_int16   convert_float32_uint32 convert_float32_int32   \
			convert_float32_uint64  convert_float32_int64  convert_float32_float64 \
			convert_float64_uint8   convert_float64_int8   convert_float64_uint16  \
			convert_float64_int16   convert_float64_uint32 convert_float64_int32   \
			convert_float64_float32 convert_float64_uint64 convert_float64_int64   \
			creat creat_excl creat_rdonly get64 get_bad_code get_bit get_char \
			get_endian8 get_endian16 get_endian32 get_endian64 get_endian_float32 \
			get_endian_float64 get_ff get_float32 get_float64 get_foffs get_fs \
			get_int8 get_int16 get_int32 get_int64 get_invalid get_lincom1 \
			get_lincom2 get_lincom3 get_lincom_noin get_linterp get_linterp_noin \
			get_linterp_notab get_multiply get_multiply_noin get_nonexistent \
			get_null $(GET_OFF64) get_phase get_recurse get_sf get_ss get_type \
			get_uint16 get_uint32 get_uint64 legacy_get legacy_nframes \
			legacy_nonexistent legacy_put legacy_spf nframes nframes64 nframes_empty \
			nframes_invalid $(NF_OFF64) nframes_spf open open_eaccess \
			open_nonexistent open_notdirfile parse_badline parse_bit parse_bit4 \
			parse_bit_bitnum parse_bit_bitsize parse_bit_ncols parse_bit_numbits \
			parse_endian_bad parse_endian_big parse_endian_force parse_endian_little \
			parse_foffs parse_include parse_include_nonexistent parse_index \
			parse_lincom parse_lincom_ncols1 parse_lincom_ncols2 \
			parse_lincom_nfields parse_lincom_nofields parse_linterp \
			parse_linterp_ncols parse_multiply parse_multiply_ncols parse_ncols \
			parse_phase parse_phase_ncols parse_raw parse_raw_char parse_raw_ncols \
			parse_raw_spf parse_raw_type parse_version parse_version_include \
			parse_version_pedantic put64 put_bad_code put_bit put_bof put_char \
			put_endian8 put_endian16 put_endian32 put_endian64 put_endian_float32 \
			put_endian_float64 put_ff put_float32 put_float64 put_foffs put_fs \
			put_int8 put_int16 put_int32 put_int64 put_invalid put_lincom1 \
			put_lincom2 put_lincom_noin put_linterp put_linterp_notab put_multiply \
			put_null $(PUT_OFF64) put_phase put_phase_noin put_rdonly put_recurse \
			put_rofs put_sf put_ss put_type put_uint16 put_uint32 put_uint64 spf \
			spf_lincom spf_multiply spf_recurse trunc trunc_rdonly trunc_rofs

check_PROGRAMS=$(TESTS)

clean-local:
	rm -rf *odirfile
