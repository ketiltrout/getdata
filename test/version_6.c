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
/* Open a Standards Version 6 conformant dirfile */
#include "test.h"

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/a#r";
  uint16_t c[8];
  int i, n, error, v, l, e, r = 0;
  DIRFILE *D;

  memset(c, 0, 8);
  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "/VERSION 6\na\\#r RAW UINT8 8\n");
  MAKEDATAFILE(data, unsigned char, i, 256);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n = gd_getdata(D, "a#r", 5, 0, 1, 0, GD_UINT16, c);
  error = gd_error(D);

  v = gd_dirfile_standards(D, GD_VERSION_CURRENT);
  l = gd_dirfile_standards(D, GD_VERSION_LATEST);
  e = gd_dirfile_standards(D, GD_VERSION_EARLIEST);

  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error,0);
  CHECKI(n,8);

  for (i = 0; i < 8; ++i)
    CHECKUi(i,c[i],40 + i);

  /* Version 6 is forward compatible with versions 7 and 8 */
  CHECKI(v,6);
  CHECKI(l,GD_DIRFILE_STANDARDS_VERSION);
  CHECKI(e,6);

  return r;
}
