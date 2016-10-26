/* Copyright (C) 2014, 2016 D. V. Wiebe
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

/* figure out expected support */
#ifdef USE_SLIM
#define GD_SLIM_MODE GD_RDONLY
#else
#define GD_SLIM_MODE GD_E_UNSUPPORTED
#endif

#ifdef USE_GZIP
#define GD_GZIP_MODE GD_RDWR
#else
#define GD_GZIP_MODE GD_E_UNSUPPORTED
#endif

#ifdef USE_BZIP2
#define GD_BZIP2_MODE GD_RDWR
#else
#define GD_BZIP2_MODE GD_E_UNSUPPORTED
#endif

#ifdef USE_LZMA
#define GD_LZMA_MODE GD_RDWR
#else
#define GD_LZMA_MODE GD_E_UNSUPPORTED
#endif

#ifdef USE_ZZIP
#define GD_ZZIP_MODE GD_RDONLY
#else
#define GD_ZZIP_MODE GD_E_UNSUPPORTED
#endif

#ifdef USE_ZZSLIM
#define GD_ZZSLIM_MODE GD_RDONLY
#else
#define GD_ZZSLIM_MODE GD_E_UNSUPPORTED
#endif

#define N 13
int main(void)
{
  int i, r = 0;
  struct {
    unsigned long e;
    int v;
  } d[N] = {
    { GD_AUTO_ENCODED,    GD_E_UNKNOWN_ENCODING }, /* 0 */
    { GD_UNENCODED,       GD_RDWR }, /* 1 */
    { GD_TEXT_ENCODED,    GD_RDWR }, /* 2 */
    { GD_SLIM_ENCODED,    GD_SLIM_MODE }, /* 3 */
    { GD_GZIP_ENCODED,    GD_GZIP_MODE }, /* 4 */
    { GD_BZIP2_ENCODED,   GD_BZIP2_MODE }, /* 5 */
    { GD_LZMA_ENCODED,    GD_LZMA_MODE }, /* 6 */
    { GD_SIE_ENCODED,     GD_RDWR }, /* 7 */
    { GD_ZZIP_ENCODED,    GD_ZZIP_MODE }, /* 8 */
    { GD_ZZSLIM_ENCODED,  GD_ZZSLIM_MODE }, /* 9 */
    { GD_ENC_UNSUPPORTED, GD_E_UNKNOWN_ENCODING }, /* 10 */
    { GD_ENCODING,        GD_E_UNKNOWN_ENCODING }, /* 11 */
    { 765,                GD_E_UNKNOWN_ENCODING }  /* 12 */
  };
  
  for (i = 0; i < N; ++i) {
    int q = gd_encoding_support(d[i].e);
    CHECKIi(i, q, d[i].v);
  }

  return r;
}
