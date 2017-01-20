/* Copyright (C) 2016, 2017 D.V. Wiebe
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

#define CHECK_INDEX_TYPE(o,t,v,C) \
  do { \
    n = gd_getdata(D, "INDEX", 0, o, 0, 8, t, v); \
    CHECKUi(o, n, 8); \
    for (i = 0; i < 8; ++i) \
      C(o + i, v[i], o + i); \
  } while(0)

#define CHECK_INDEX_CTYPE(o,t,v,C) \
  do { \
    n = gd_getdata(D, "INDEX", 0, o, 0, 8, t, v); \
    CHECKUi(o, n, 8); \
    for (i = 0; i < 8; ++i) { \
      CHECKFi(o + i, v[i * 2], o + i); \
      CHECKFi(o + i, v[i * 2 + 1], 0); \
    } \
  } while(0)

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  int r = 0;
  int8_t     i8[8];
  int16_t   i16[8];
  int32_t   i32[8];
  int64_t   i64[8];
  uint8_t    u8[8];
  uint16_t  u16[8];
  uint32_t  u32[8];
  uint64_t  u64[8];
  float     f32[8];
  double    f64[8];
  float    c64[16];
  double  c128[16];
  size_t i, n;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEEMPTYFILE(format, 0600);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);

  CHECK_INDEX_TYPE(    0,       GD_INT8,   i8, CHECKIi);
  CHECK_INDEX_TYPE(  100,      GD_UINT8,   u8, CHECKUi);
  CHECK_INDEX_TYPE(  200,      GD_INT16,  i16, CHECKIi);
  CHECK_INDEX_TYPE(  300,     GD_UINT16,  u16, CHECKUi);
  CHECK_INDEX_TYPE(  400,      GD_INT32,  i32, CHECKIi);
  CHECK_INDEX_TYPE(  500,     GD_UINT32,  u32, CHECKUi);
  CHECK_INDEX_TYPE(  600,      GD_INT64,  i64, CHECKIi);
  CHECK_INDEX_TYPE(  700,     GD_UINT64,  u64, CHECKUi);
  CHECK_INDEX_TYPE(  800,    GD_FLOAT32,  f32, CHECKFi);
  CHECK_INDEX_TYPE(  900,    GD_FLOAT64,  f64, CHECKFi);
  CHECK_INDEX_CTYPE(1000,  GD_COMPLEX64,  c64, CHECKRi);
  CHECK_INDEX_CTYPE(1100, GD_COMPLEX128, c128, CHECKRi);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
