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
  unsigned long e0, e1, e2, e3;
  int ret, error, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "data RAW UINT16 8\n"
    "/ENDIAN little\n"
    "/INCLUDE format1\n"
  );
  MAKEFORMATFILE(format1, "data1 RAW UINT16 8\n/ENDIAN little arm\n");

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  e0 = gd_endianness(D, 0);
  e1 = gd_endianness(D, 1);
  ret = gd_alter_endianness(D, GD_BIG_ENDIAN, GD_ALL_FRAGMENTS, 0);
  error = gd_error(D);
  e2 = gd_endianness(D, 0);
  e3 = gd_endianness(D, 1);

  gd_discard(D);

  unlink(format);
  unlink(format1);
  rmdir(filedir);

  CHECKI(error,0);
  CHECKI(ret, 0);
  CHECKU(e0, GD_LITTLE_ENDIAN | GD_NOT_ARM_ENDIAN);
  CHECKU(e1, GD_LITTLE_ENDIAN | GD_ARM_ENDIAN);
  CHECKU(e2, GD_BIG_ENDIAN);
  CHECKU(e3, GD_BIG_ENDIAN);

  return r;
}
