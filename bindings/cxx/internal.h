// Copyright (C) 2011, 2015 D. V. Wiebe
//
///////////////////////////////////////////////////////////////////////////
//
// This file is part of the GetData project.
//
// GetData is free software; you can redistribute it and/or modify it under
// the terms of the GNU Lesser General Public License as published by the
// Free Software Foundation; either version 2.1 of the License, or (at your
// option) any later version.
//
// GetData is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
// License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with GetData; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
//
#ifdef HAVE_CONFIG_H
#include "gd_config.h"
#endif

/* To avoid including stuff out of tree, we include everything here,
 * even though getdata/dirfile.h will try to include it again.
 */
#undef GETDATA_LEGACY_API
#define GD_NO_LEGACY_API
#define GD_C89_API
#define GD_64BIT_API

#include "getdata/types.h"
#include "getdata/fragment.h"
#include "getdata/entry.h"
#include "getdata/rawentry.h"
#include "getdata/lincomentry.h"
#include "getdata/linterpentry.h"
#include "getdata/bitentry.h"
#include "getdata/sbitentry.h"
#include "getdata/phaseentry.h"
#include "getdata/indexentry.h"
#include "getdata/polynomentry.h"
#include "getdata/constentry.h"
#include "getdata/carrayentry.h"
#include "getdata/stringentry.h"
#include "getdata/mplexentry.h"
#include "getdata/multiplyentry.h"
#include "getdata/divideentry.h"
#include "getdata/recipentry.h"
#include "getdata/windowentry.h"
#include "getdata/dirfile.h"

#include <cstring>
#include <cstdlib>
#include <cstdio>
#include <stdint.h>
#ifdef HAVE_INTTYPES_H
#ifndef __STDC_FORMAT_MACROS
#define __STDC_FORMAT_MACROS
#endif
#include <inttypes.h>
#endif

/* debugging macros */
#ifdef GETDATA_DEBUG
extern "C" const char* gd_colnil(void);
extern "C" const char* gd_coladd(void);
extern "C" const char* gd_colsub(void);
#define dtracevoid() printf("%s %s()\n", gd_coladd(), __func__)
#define dtrace(fmt, ...) printf("%s %s(" fmt ")\n", gd_coladd(), \
    __func__, ##__VA_ARGS__)
#define dprintf(fmt, ...) printf("%s %s:%i " fmt "\n", gd_colnil(), \
    __func__, __LINE__, ##__VA_ARGS__)
#define dreturnvoid() printf("%s %s = (nil)\n", gd_colsub(), __func__)
#define dreturn(fmt, ...) printf("%s %s = " fmt "\n", gd_colsub(), \
    __func__, ##__VA_ARGS__)
#define dwatch(fmt, v) printf("%s %s = " fmt "\n", gd_colnil(), #v, v)
#else
#define dtracevoid()
#define dtrace(...)
#define dprintf(...)
#define dreturnvoid()
#define dreturn(...)
#define dwatch(...)
#endif

#ifdef _MSC_VER
#define __gd_unused 
#else
#define __gd_unused __attribute__ (( unused ))
#endif

using namespace GetData;
