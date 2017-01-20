/* Copyright (C) 2011, 2013, 2017 D.V. Wiebe
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
/* Check GD_HERE and FRAMEOFFSET */
#include "test.h"

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  unsigned char c[8], d[8];
  int i, j, k, l, m, n, e1, e2, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "FRAMEOFFSET 2\ndata RAW UINT8 8\n");
  MAKEDATAFILE(data, unsigned char, i, 256);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n = gd_seek(D, "data", 0, 4, GD_SEEK_SET);
  m = gd_getdata(D, "data", GD_HERE, 0, 1, 0, GD_UINT8, c);
  e1 = gd_error(D);
  l = gd_tell(D, "data");
  k = gd_getdata(D, "data", GD_HERE, 0, 1, 0, GD_UINT8, d);
  e2 = gd_error(D);
  j = gd_tell(D, "data");

  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(e1, 0);
  CHECKI(e2, 0);
  CHECKI(n, 4);
  CHECKI(m, 8);
  CHECKI(l, 12);
  CHECKI(k, 8);
  CHECKI(j, 20);

  for (i = 0; i < 8; ++i) {
    CHECKUi(i, c[i], 0);
    CHECKUi(i, d[i], (i < 4) ? 0 : i - 4);
  }

  return r;
}
