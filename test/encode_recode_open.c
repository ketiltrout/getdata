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
  const char *data = "dirfile/data";
  const char *txtdata = "dirfile/data.txt";
  uint16_t c1, c2;
  int ret, e1, e2, unlink_txtdata, unlink_data, r = 0;
  DIRFILE *D;
  off_t n1, n2;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "data RAW UINT16 8\nENCODING none\n");
  MAKEDATAFILE(data, uint16_t, 0x201 * i, 128);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  n1 = gd_getdata(D, "data", 0, 3, 0, 1, GD_UINT16, &c1);
  CHECKI(n1, 1);
  CHECKI(c1, 0x603);

  ret = gd_alter_encoding(D, GD_TEXT_ENCODED, 0, 1);
  e1 = gd_error(D);
  CHECKI(ret, 0);
  CHECKI(e1, 0);

  n2 = gd_getdata(D, "data", 0, 3, 0, 1, GD_UINT16, &c2);
  CHECKI(n2, 1);
  CHECKI(c2, 0x603);

  e2 = gd_close(D);
  CHECKI(e2, 0);

  unlink_txtdata = unlink(txtdata);
  unlink_data = unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(unlink_txtdata, 0);
  CHECKI(unlink_data, -1);

  return r;
}
