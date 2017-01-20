/* Copyright (C) 2015, 2017 D.V. Wiebe
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
  unsigned char c1[5], c2[5];
  int i, n1, n2, e1, e2, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "FRAMEOFFSET 2\ndata RAW UINT8 1\n");
  MAKEDATAFILE(data, unsigned char, i, 256);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n1 = gd_getdata(D, "data", 0, 0, 5, 0, GD_UINT8, c1);
  e1 = gd_error(D);
  CHECKI(n1, 5);
  CHECKI(e1, 0);

  for (i = 0; i < 5; ++i)
    CHECKUi(i, c1[i], (i < 2) ? 0 : i - 2);

  n2 = gd_getdata(D, "data", 0, 0, 5, 0, GD_UINT8, c2);
  e2 = gd_error(D);
  CHECKI(n2, 5);
  CHECKI(e2, 0);

  for (i = 0; i < 5; ++i)
    CHECKUi(i, c2[i], (i < 2) ? 0 : i - 2);

  gd_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  return r;
}
