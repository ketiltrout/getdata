// Copyright (C) 2011 D. V. Wiebe
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
#undef GETDATA_LEGACY_API
#include "getdata/dirfile.h"

#include <cstring>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

/* debugging macros */
#ifdef GETDATA_DEBUG
extern "C" const char* gd_colnil(void);
extern "C" const char* gd_coladd(void);
extern "C" const char* gd_colsub(void);
#define dtracevoid() printf("%s %s()\n", gd_coladd(), __FUNCTION__)
#define dtrace(fmt, ...) printf("%s %s(" fmt ")\n", gd_coladd(), \
    __FUNCTION__, ##__VA_ARGS__)
#define dprintf(fmt, ...) printf("%s %s:%i " fmt "\n", gd_colnil(), \
    __FUNCTION__, __LINE__, ##__VA_ARGS__)
#define dreturnvoid() printf("%s %s = (nil)\n", gd_colsub(), __FUNCTION__)
#define dreturn(fmt, ...) printf("%s %s = " fmt "\n", gd_colsub(), \
    __FUNCTION__, ##__VA_ARGS__)
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
