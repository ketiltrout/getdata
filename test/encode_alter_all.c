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
  unsigned long e0, e1, e2, e3;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "data RAW UINT16 8\n"
    "/ENCODING none\n"
    "/INCLUDE format1\n"
  );
  MAKEFORMATFILE(format1, "data1 RAW UINT8 8\n/ENCODING sie\n");

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  e0 = gd_encoding(D, 0);
  e1 = gd_encoding(D, 1);
  ret = gd_alter_encoding(D, GD_TEXT_ENCODED, GD_ALL_FRAGMENTS, 0);
  error = gd_error(D);
  e2 = gd_encoding(D, 0);
  e3 = gd_encoding(D, 1);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(ret, 0);
  CHECKU(e0, GD_UNENCODED);
  CHECKU(e1, GD_SIE_ENCODED);
  CHECKU(e2, GD_TEXT_ENCODED);
  CHECKU(e3, GD_TEXT_ENCODED);

  return r;
}
