/* Copyright (C) 2014, 2017 D.V. Wiebe
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
#if !defined USE_LZMA || !defined TEST_LZMA
  return 77; /* skip test */
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  const char *xzdata = "dirfile/data.xz";
  uint16_t c1[8], c2[8];
  char command[4096];
  int i, n1, e1, e2, n2, e3, r = 0;
  DIRFILE *D;

  memset(c1, 0, 16);
  memset(c2, 0, 16);
  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "data RAW UINT16 8\n");
  MAKEDATAFILE(data, uint16_t, i, 256);

  /* compress */
  snprintf(command, 4096, "%s -f %s > /dev/null", XZ, data);
  if (gd_system(command))
    return 1;

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n1 = gd_getdata(D, "data", 5, 0, 1, 0, GD_UINT16, c1);
  CHECKI(n1, 8);
  e1 = gd_error(D);
  CHECKI(e1, 0);

  e2 = gd_close(D);
  CHECKI(e2, 0);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n2 = gd_getdata(D, "data", 5, 0, 1, 0, GD_UINT16, c2);
  e3 = gd_error(D);
  CHECKI(e3, 0);
  CHECKI(n2, 8);
  for (i = 0; i < 8; ++i) {
    CHECKIi(i,c1[i], 40 + i);
    CHECKIi(i,c2[i], 40 + i);
  }

  gd_discard(D);

  unlink(xzdata);
  unlink(format);
  rmdir(filedir);

  return r;
#endif
}
