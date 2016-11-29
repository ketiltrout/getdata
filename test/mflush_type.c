/* Copyright (C) 2016 D. V. Wiebe
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

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  int e1, e2, r = 0;
  gd_type_t t;
  DIRFILE *D;

  rmdirfile();
  D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_EXCL);

  gd_add_raw(D, "i08", GD_INT8, 1, 0);
  gd_add_raw(D, "i16", GD_INT16, 1, 0);
  gd_add_raw(D, "i32", GD_INT32, 1, 0);
  gd_add_raw(D, "i64", GD_INT64, 1, 0);
  gd_add_raw(D, "u08", GD_UINT8, 1, 0);
  gd_add_raw(D, "u16", GD_UINT16, 1, 0);
  gd_add_raw(D, "u32", GD_UINT32, 1, 0);
  gd_add_raw(D, "u64", GD_UINT64, 1, 0);
  gd_add_raw(D, "f32", GD_FLOAT32, 1, 0);
  gd_add_raw(D, "f64", GD_FLOAT64, 1, 0);
  gd_add_raw(D, "c64", GD_COMPLEX64, 1, 0);
  gd_add_raw(D, "c28", GD_COMPLEX128, 1, 0);

  e1 = gd_metaflush(D);
  CHECKI(e1, GD_E_OK);

  e2 = gd_close(D);
  CHECKI(e2, 0);

  D = gd_open(filedir, GD_RDONLY);

  t = gd_native_type(D, "i08"); CHECKIi(0, t, GD_INT8);
  t = gd_native_type(D, "i16"); CHECKIi(1, t, GD_INT16);
  t = gd_native_type(D, "i32"); CHECKIi(2, t, GD_INT32);
  t = gd_native_type(D, "i64"); CHECKIi(3, t, GD_INT64);
  t = gd_native_type(D, "u08"); CHECKIi(4, t, GD_UINT8);
  t = gd_native_type(D, "u16"); CHECKIi(5, t, GD_UINT16);
  t = gd_native_type(D, "u32"); CHECKIi(6, t, GD_UINT32);
  t = gd_native_type(D, "u64"); CHECKIi(7, t, GD_UINT64);
  t = gd_native_type(D, "f32"); CHECKIi(8, t, GD_FLOAT32);
  t = gd_native_type(D, "f64"); CHECKIi(9, t, GD_FLOAT64);
  t = gd_native_type(D, "c64"); CHECKIi(10, t, GD_COMPLEX64);
  t = gd_native_type(D, "c28"); CHECKIi(11, t, GD_COMPLEX128);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
