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
#include "test.h"

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *format1 = "dirfile/format1";
  int error1, error2, error3, error4, r = 0;
  off_t bof_data, bof_data2, bof_lincom, bof_lincom2;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "data RAW UINT16 2\n"
    "/FRAMEOFFSET 35\n"
    "lincom LINCOM 2 data2 1. 0. data 1. 0.\n"
    "lincom2 LINCOM 2 data 1. 0. data2 1. 0.\n"
    "INCLUDE format1\n"
  );
  MAKEFORMATFILE(format1, "data2 RAW UINT8 3\nFRAMEOFFSET 33\n");

  D = gd_open(filedir, GD_RDONLY);
  bof_data = gd_bof(D, "data");
  error1 = gd_error(D);
  bof_data2 = gd_bof(D, "data2");
  error2 = gd_error(D);
  bof_lincom = gd_bof(D, "lincom");
  error3 = gd_error(D);
  bof_lincom2 = gd_bof(D, "lincom2");
  error4 = gd_error(D);
  gd_discard(D);

  unlink(format1);
  unlink(format);
  rmdir(filedir);

  CHECKI(error1, 0);
  CHECKI(bof_data, 70);
  CHECKI(error2, 0);
  CHECKI(bof_data2, 99);
  CHECKI(error3, 0);
  CHECKI(bof_lincom, 105);
  CHECKI(error4, 0);
  CHECKI(bof_lincom2, 70);

  return r;
}
