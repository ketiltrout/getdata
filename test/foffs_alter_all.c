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
  const char *format1 = "dirfile/format1";
  int ret, error, r = 0;
  off_t o0, o1, o2, o3;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "data RAW UINT8 8\n"
    "/FRAMEOFFSET 13\n"
    "/INCLUDE format1\n"
  );
  MAKEFORMATFILE(format1, "data1 RAW UINT8 8\n/FRAMEOFFSET 14\n");

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  o0 = gd_frameoffset(D, 0);
  o1 = gd_frameoffset(D, 1);
  ret = gd_alter_frameoffset(D, 16, GD_ALL_FRAGMENTS, 0);
  error = gd_error(D);
  o2 = gd_frameoffset(D, 0);
  o3 = gd_frameoffset(D, 1);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(ret, 0);
  CHECKI(o0, 13);
  CHECKI(o1, 14);
  CHECKI(o2, 16);
  CHECKI(o3, 16);

  return r;
}
