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

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  double c[16];
  int e1, e2, r = 0;
  size_t n1, n2;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
      "div1 DIVIDE data missing\n"
      "div2 DIVIDE missing data\n"
      "data RAW COMPLEX128 1\n"
      );
  MAKEDATAFILE(data, double, i, 256);

  D = gd_open(filedir, GD_RDONLY);
  n1 = gd_getdata(D, "div1", 5, 0, 1, 0, GD_COMPLEX128, c);
  CHECKU(n1, 0);
  e1 = gd_error(D);
  CHECKI(e1, GD_E_BAD_CODE);

  n2 = gd_getdata(D, "div2", 5, 0, 1, 0, GD_COMPLEX128, c);
  CHECKU(n2, 0);
  e2 = gd_error(D);
  CHECKI(e2, GD_E_BAD_CODE);

  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  return r;
}
