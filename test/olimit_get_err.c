/* Copyright (C) 2017 D.V. Wiebe
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
  const char *data1 = "dirfile/data1";
  const char *data2 = "dirfile/data2";
  const char *data3 = "dirfile/data3";
  const char *data4 = "dirfile/data4";
  int e1, e2, e3, e4, r = 0;
  int no0, no1, no2, no3, no4;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
      "data1 RAW UINT8 8\n"
      "data2 RAW UINT8 8\n"
      "data3 RAW UINT8 8\n"
      "data4 RAW UINT8 8\n"
      );
  MAKEDATAFILE(data1, unsigned char, i, 256);
  MAKEDATAFILE(data2, unsigned char, i, 256);
  MAKEDATAFILE(data3, unsigned char, i, 256);
  MAKEDATAFILE(data4, unsigned char, i, 256);

  D = gd_open(filedir, GD_RDONLY);

  gd_open_limit(D, 2);

  no0 = gd_open_limit(D, GD_OLIMIT_COUNT);
  CHECKI(no0, 0);

  gd_getdata(D, "data1", 5, 0, 1, 0, GD_NULL, NULL);
  e1 = gd_error(D);
  CHECKI(e1, 0);
  no1 = gd_open_limit(D, GD_OLIMIT_COUNT);
  CHECKI(no1, 1);

  gd_getdata(D, "data5", 5, 0, 1, 0, GD_NULL, NULL);
  e2 = gd_error(D);
  CHECKI(e2, GD_E_BAD_CODE);
  no2 = gd_open_limit(D, GD_OLIMIT_COUNT);
  CHECKI(no2, 1);

  gd_getdata(D, "data3", 5, 0, 1, 0, GD_NULL, NULL);
  e3 = gd_error(D);
  CHECKI(e3, 0);
  no3 = gd_open_limit(D, GD_OLIMIT_COUNT);
  CHECKI(no3, 2);

  gd_getdata(D, "data4", 5, 0, 1, 0, GD_NULL, NULL);
  e4 = gd_error(D);
  CHECKI(e4, 0);
  no4 = gd_open_limit(D, GD_OLIMIT_COUNT);
  CHECKI(no4, 2);

  gd_discard(D);

  unlink(data1);
  unlink(data2);
  unlink(data3);
  unlink(data4);
  unlink(format);
  rmdir(filedir);

  return r;
}
