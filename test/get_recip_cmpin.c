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
  double d1[2], d2[2];
  int r = 0;
  size_t n1, n2;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "div RECIP data 2.\n"
    "cdiv RECIP data 3;4\n"
    "data RAW COMPLEX128 1\n");

  MAKEDATAFILE(data, double, i, 256);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n1 = gd_getdata(D, "div", 5, 0, 1, 0, GD_COMPLEX128, d1);
  CHECKU(n1, 1);

  /* a / (c + id) = ac / (cc + dd) - i ad / (cc + dd) */
  CHECKF(d1[0], 2 * 10. / (10. * 10 + 11. * 11));
  CHECKF(d1[1], -2 * 11. / (10. * 10 + 11. * 11));

  n2 = gd_getdata(D, "cdiv", 10, 0, 1, 0, GD_COMPLEX128, d2);
  CHECKU(n2, 1);

  /* (a + ib) / (c + id) = (ac - bd) / (cc + dd) + i (bc - ad) / (cc + dd) */
  CHECKF(d2[0], (3 * 20. + 4 * 21.) / (20. * 20 + 21. * 21));
  CHECKF(d2[1], (4 * 20. - 3 * 21.) / (20. * 20 + 21. * 21));

  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  return r;
}
