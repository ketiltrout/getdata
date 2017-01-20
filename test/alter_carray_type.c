/* Copyright (C) 2010-2011, 2013, 2016, 2017 D.V. Wiebe
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
  int e1, e2, e3, n, r = 0;
  size_t i, z;
  DIRFILE *D;
  double d[7];

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "carray CARRAY FLOAT32 8.3 7.2 6.1 5.0 3.9 2.8 1.7\n");

  D = gd_open(filedir, GD_RDWR);

  e1 = gd_alter_carray(D, "carray", GD_STRING, 0);
  CHECKI(e1, GD_E_BAD_TYPE);

  e2 = gd_alter_carray(D, "carray", -1, 0);
  CHECKI(e2, GD_E_BAD_TYPE);

  e3 = gd_alter_carray(D, "carray", GD_UINT8, 0);
  CHECKI(e3, 0);

  z = gd_array_len(D, "carray");
  CHECKU(z, 7);

  n = gd_get_carray(D, "carray", GD_FLOAT64, &d);
  CHECKI(n, 0);

  for (i = 0; i < z; ++i)
    CHECKFi(i, d[i], ((i > 3) ? 7. : 8.) - i);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
