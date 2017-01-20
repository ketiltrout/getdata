/* Copyright (C) 2016, 2017 D.V. Wiebe
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
  const char *data = "dirfile/data";
  const char *txtdata = "dirfile/data.txt";
  int r = 0;
  int e1, unlink_data, unlink_txtdata;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "/INCLUDE format1\ndata RAW UINT16 11");
  MAKEFORMATFILE(format1, "/PROTECT data\n/ENCODING text\n");
  MAKEDATAFILE(data, uint16_t, i * 0x201, 128);

  D = gd_open(filedir, GD_RDWR | GD_UNENCODED);
  e1 = gd_move(D, "data", 1, GD_REN_DATA);
  CHECKI(e1, GD_E_PROTECTED);

  gd_discard(D);

  unlink(format1);
  unlink(format);
  unlink_data = unlink(data);
  unlink_txtdata = unlink(txtdata);
  rmdir(filedir);

  CHECKI(unlink_data, 0);
  CHECKI(unlink_txtdata, -1);

  return r;
}
