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
  int e1, e2, e3, r = 0;
  gd_type_t t;
  DIRFILE *D;

  rmdirfile();
  D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_EXCL);

  gd_add_raw(D, "i16", GD_INT16, 1, 0);
  gd_add_raw(D, "i32", GD_INT32, 1, 0);
  gd_add_raw(D, "u08", GD_UINT8, 1, 0);
  gd_add_raw(D, "u16", GD_UINT16, 1, 0);
  gd_add_raw(D, "u32", GD_UINT32, 1, 0);
  gd_add_raw(D, "f32", GD_FLOAT32, 1, 0);
  gd_add_raw(D, "f64", GD_FLOAT64, 1, 0);

  e1 = gd_dirfile_standards(D, 4);
  CHECKI(e1, 4);

  e2 = gd_metaflush(D);
  CHECKI(e2, GD_E_OK);

  e3 = gd_close(D);
  CHECKI(e3, 0);

  D = gd_open(filedir, GD_RDONLY);

  t = gd_native_type(D, "i16"); CHECKIi(1, t, GD_INT16);
  t = gd_native_type(D, "i32"); CHECKIi(2, t, GD_INT32);
  t = gd_native_type(D, "u08"); CHECKIi(4, t, GD_UINT8);
  t = gd_native_type(D, "u16"); CHECKIi(5, t, GD_UINT16);
  t = gd_native_type(D, "u32"); CHECKIi(6, t, GD_UINT32);
  t = gd_native_type(D, "f32"); CHECKIi(8, t, GD_FLOAT32);
  t = gd_native_type(D, "f64"); CHECKIi(9, t, GD_FLOAT64);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
