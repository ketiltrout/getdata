/* Copyright (C) 2013, 2017 D.V. Wiebe
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
  const char *count = "dirfile/count";
  unsigned char c[8], d[8];
  int n1, n2, i, e1, e2, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "mplex MPLEX data count 0 3\n"
    "count RAW UINT8 8\n"
    "data RAW UINT8 8\n"
  );
  MAKEDATAFILE(data, unsigned char, i, 256);
  MAKEDATAFILE(count, unsigned char, i % 3, 256);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n1 = gd_getdata(D, "mplex", 5, 0, 1, 0, GD_UINT8, &c);
  e1 = gd_error(D);
  n2 = gd_getdata(D, "mplex", 6, 0, 1, 0, GD_UINT8, &d);
  e2 = gd_error(D);

  gd_discard(D);

  unlink(count);
  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(e1, 0);
  CHECKI(e2, 0);
  CHECKI(n1, 8);
  CHECKI(n2, 8);
  for (i = 0; i < 8; ++i) {
    CHECKIi(i, c[i], 39 + 3 * ((i + 1) / 3));
    CHECKIi(i, d[i], 48 + 3 * (i / 3));
  }

  return r;
}
