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
  const char *subdir = "dirfile/sub";
  const char *format = "dirfile/format";
  const char *format1 = "dirfile/sub/format1";
  const char *data = "dirfile/sub/data";
  int e1, r = 0;
  off_t n;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);
  mkdir(subdir, 0700);

  MAKEFORMATFILE(format, "/INCLUDE sub/format1\n");
  MAKEFORMATFILE(format1, "data RAW UINT8 8\nFRAMEOFFSET 6\n");
  MAKEDATAFILE(data, unsigned char, i, 256);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n = gd_tell(D, "data");
  e1 = gd_error(D);
  CHECKI(e1, 0);
  CHECKI(n, 48);

  gd_discard(D);

  unlink(data);
  unlink(format1);
  unlink(format);
  rmdir(subdir);
  rmdir(filedir);

  return r;
}
