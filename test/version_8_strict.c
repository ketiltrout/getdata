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
#include "test.h"

int cb(gd_parser_data_t *pdata, void *ll)
{
  ((int*)ll)[pdata->linenum - 1] = 1;
  return GD_SYNTAX_IGNORE;
}

#define NLINES 13
int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/ar";
  uint16_t c[8];
  int ll[NLINES];
  int i, n, error, r = 0;
  DIRFILE *D;

  memset(c, 0, 16);
  memset(ll, 0, NLINES * sizeof(int));
  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "/VERSION 8\n"
    "X<r RAW UINT8 8\n"
    "X.r RAW UINT8 8\n"
    "X\\#r RAW COMPLEX128 8\n"
    "Xr POLYNOM INDEX 8 3 1 2\n"
    "ar RAW UINT8 8\n"
    "e RECIP 3. Xr\n"
    "FRAMEOFFSET 3\n"
    "FRAMEOFFSET RAW UINT8 3\n"
    "/FRAMEOFFSET 3\n"
    "e RECIP ar/c Xr\n"
    "e DIVIDE ar Xr\n"
    "ar/c CONST COMPLEX128 3;3\n"
  );

  MAKEDATAFILE(data, unsigned char, i, 256);

  D = gd_cbopen(filedir, GD_RDONLY | GD_PEDANTIC, cb, ll);
  n = gd_getdata(D, "ar", 5, 0, 1, 0, GD_UINT16, c);
  error = gd_error(D);

  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error,0);

  for (i = 0; i < NLINES; ++i) {
    if (i == 1 || i == 2 || i == 7 || i == 10 || i == 11) {
      CHECKIi(i,ll[i], 1);
    } else {
      CHECKIi(i,ll[i],0);
    }
  }

  CHECKI(n,8);

  for (i = 0; i < n; ++i)
    CHECKUi(i,c[i],16 +i);

  return r;
}
