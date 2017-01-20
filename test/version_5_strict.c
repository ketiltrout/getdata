/* Copyright (C) 2010-2011, 2013, 2017 D.V. Wiebe
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
/* CheckStandards Version 5 strictness */
#include "test.h"

int cb(gd_parser_data_t *pdata, void *ll)
{
  ((int*)ll)[pdata->linenum - 1] = 1;
  return GD_SYNTAX_IGNORE;
}

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/ar";
  uint16_t c[8];
  int ll[7] = {0, 0, 0, 0, 0, 0, 0};
  uint16_t d[8];
  int n, error, m, error2, i, r = 0;
  DIRFILE *D;

  memset(c, 0, 8);
  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "/VERSION 5\n"
    "ENDIAN little\n"
    "X<r RAW UINT8 8\n"
    "/ENCODING raw\n"
    "/REFERENCE ar\n"
    "/PROTECT none\n"
    "ar RAW UINT8 8\n"
  );
  MAKEDATAFILE(data, unsigned char, i, 256);

  D = gd_cbopen(filedir, GD_RDONLY | GD_PEDANTIC, cb, ll);
  n = gd_getdata(D, "ar", 5, 0, 1, 0, GD_UINT16, c);
  error = gd_error(D);

  m = gd_getdata(D, "FILEFRAM", 5, 0, 8, 0, GD_UINT16, d);
  error2 = gd_error(D);

  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error,0);
  CHECKI(error2,0);
  CHECKI(n,8);

  for (i = 0; i < 7; ++i)
    if ((i == 2 || i == 3 || i == 4 || i == 5)) {
      CHECKIi(i,!ll[i],0);
    } else
      CHECKIi(i,ll[i],0);

  for (i = 0; i < 8; ++i)
    CHECKIi(i,c[i], 40 + i);

  CHECKI(m,8);

  for (i = 0; i < 8; ++i)
    CHECKIi(i,d[i], 5 + i);

  return r;
}
