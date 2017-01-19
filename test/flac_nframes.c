/* Copyright (C) 2016, 2017 D. V. Wiebe
 *
 ***************************************************************************
 *
 * This file is part of the GetData project.
 *
 * GetData is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation; either version 2.1 of the License, or (at your
 * option) any later version.
 *
 * GetData is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with GetData; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
#include "test.h"

#ifndef TEST_FLAC
#define ENC_SKIP_TEST 1
#endif

#ifdef USE_FLAC
#define USE_ENC 1
#endif

#ifdef WORDS_BIGENDIAN
#define ENDIANNESS "--endian=big"
#else
#define ENDIANNESS "--endian=little"
#endif

#define ENC_SUFFIX ".flac"
#define ENC_COMPRESS snprintf(command, 4096, \
    "%s " ENDIANNESS " --silent --sample-rate=1 --channels=1 --bps=16 " \
    "--sign=signed --delete-input-file %s >/dev/null 2>/dev/null", FLAC, \
    data)

#include "enc_nframes.c"
