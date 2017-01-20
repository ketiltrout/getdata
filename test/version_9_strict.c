/* Copyright (C) 2010-2011, 2013, 2014, 2017 D.V. Wiebe
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

#define NLINES 18
int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *format1 = "dirfile/format1";
  const char *format2 = "dirfile/format2";
  const char *data = "dirfile/ar";
  uint16_t c[8];
  int ll[NLINES];
  int i, n, v, error, r = 0;
  DIRFILE *D;

  memset(c, 0, 16);
  memset(ll, 0, NLINES * sizeof(int));
  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "/VERSION 8\n"  /* 0 */
    "/INCLUDE format1\n" /* 1 */
    "w WINDOW INDEX INDEX SET 0x1\n"
  );
  MAKEFORMATFILE(format1,
    "\n\n\n"
    "/VERSION 9\n" /* 3 */
    "/INCLUDE format2 A Z\n" /* 4 */
    "/INCLUDE format2 A.Z\n" /* 5 */
    "Xr RAW COMPLEX128 0xA\n" /* 6 */
    "Xy POLYNOM INDEX 8 055 0xAE 2\n" /* 7 */
    "ar WINDOW AdZ INDEX SET 0x1\n" /* 8 */
    "AINDEXZ PHASE INDEX 0\n" /* 9 */
    "/HIDDEN Xy\n"
  );
  MAKEFORMATFILE(format2,
    "\n\n\n"
    "\n\n\n\n\n\n\n\n"
    "c RAW UINT8 1\n" /* 11 */
    "/HIDDEN c\n" /* 12 */
    "/REFERENCE c\n" /* 13 */
    "/VERSION 8\n" /* 14 */
    "d PHASE INDEX 0\n" /* 15 */
    "d/c CONST FLOAT64 1\n" /* 16 */
    "/META d d CONST FLOAT64 1\n"
  );
  MAKEDATAFILE(data, unsigned char, i, 256);

  D = gd_cbopen(filedir, GD_RDONLY | GD_PEDANTIC, cb, ll);
  error = gd_error(D);
  v = gd_dirfile_standards(D, GD_VERSION_CURRENT);
  CHECKI(v,GD_DIRFILE_STANDARDS_VERSION);
  n = gd_getdata(D, "ar", 4, 0, 8, 0, GD_UINT16, c);

  gd_discard(D);

  unlink(data);
  unlink(format);
  unlink(format1);
  unlink(format2);
  rmdir(filedir);

  CHECKI(error,0);

  for (i = 0; i < NLINES; ++i) {
    if (i == 2 || i == 5) {
      CHECKIi(i,ll[i], 1);
    } else {
      CHECKIi(i,ll[i],0);
    }
  }

  CHECKI(n,8);

  for (i = 0; i < n; ++i)
    CHECKUi(i,c[i], (i & 1) ? 4 + i : 0);

  return r;
}
