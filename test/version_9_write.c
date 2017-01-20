/* Copyright (C) 2010-2011, 2013, 2014, 2016, 2017 D.V. Wiebe
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
  const char *format[] = {
    "dirfile/format",
    "dirfile/format1",
    "dirfile/format2",
    "dirfile/format3",
    "dirfile/format4"
  };
  uint16_t c[8];
  int i, e1, e2, e3, n, v, h1, h2, r = 0;
  DIRFILE *D;

  memset(c, 0, 16);
  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format[0],
      "/VERSION 8\n"
      "/INCLUDE format1\n");
  MAKEFORMATFILE(format[1],
      "/VERSION 9\n"
      "Xr RAW COMPLEX128 0xA\n"
      "Xy POLYNOM INDEX 8 055 0xAE 2\n"
      "ar WINDOW d INDEX SET 0x1\n"
      "/ALIAS BINDEX INDEX\n"
      "/ALIAS AINDEXYZ BINDEX\n"
      "/INCLUDE format2 A Z\n"
      "/ALIAS m AdYZ\n"
      "/INCLUDE format4\n"
      "/ALIAS z n\n"
      "/ALIAS d z\n"
      "/HIDDEN Xy\n"
      );
  MAKEFORMATFILE(format[2], "/INCLUDE format3 \"\" Y\n");
  MAKEFORMATFILE(format[3], "/ALIAS d INDEX\n/HIDDEN d\n");
  MAKEFORMATFILE(format[4], "/ALIAS n m\n");

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  h1 = gd_hidden(D, "AdYZ");
  CHECKI(h1,1);

  e1 = gd_dirfile_standards(D, 9);
  CHECKI(e1, 9);

  e2 = gd_rewrite_fragment(D, GD_ALL_FRAGMENTS);
  CHECKI(e2, 0);

  e3 = gd_close(D);
  CHECKI(e3, 0);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  v = gd_dirfile_standards(D, GD_VERSION_CURRENT);
  CHECKI(v,9);

  n = gd_getdata(D, "ar", 4, 0, 8, 0, GD_UINT16, c);
  CHECKI(n,8);

  h2 = gd_hidden(D, "AdYZ");
  CHECKI(h2,1);

  for (i = 0; i < n; ++i)
    CHECKUi(i,c[i], (i & 1) ? 4 + i : 0);

  gd_discard(D);

  for (i = 0; i < 5; ++i)
    unlink(format[i]);
  rmdir(filedir);

  return r;
}
