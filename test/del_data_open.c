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
/* delete an open data file */
#include "test.h"

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  unsigned char data_data[256];
  int ret, e0, e1, n, e2, e3, unlink_data, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "data RAW UINT8 8\n");
  MAKEDATAFILE(data, unsigned char, i, 256);

  D = gd_open(filedir, GD_RDWR);
  gd_getdata(D, "data", 5, 0, 1, 0, GD_UINT8, data_data);
  e0 = gd_error(D);
  CHECKI(e0, GD_E_OK);

  ret = gd_delete(D, "data", GD_DEL_DATA);
  e1 = gd_error(D);
  CHECKI(ret, 0);
  CHECKI(e1, GD_E_OK);

  n = gd_getdata(D, "data", 5, 0, 1, 0, GD_UINT8, data_data);
  e2 = gd_error(D);
  CHECKI(e2, GD_E_BAD_CODE);
  CHECKI(n, 0);

  e3 = gd_close(D);
  CHECKI(e3, 0);

  unlink_data = unlink(data);
  unlink(format);
  rmdir(filedir);
  CHECKI(unlink_data, -1);

  return r;
}
