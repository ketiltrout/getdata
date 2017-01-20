/* Copyright (C) 2008-2011, 2013, 2017 D.V. Wiebe
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
/* Parser check */
#include "test.h"

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";

  const char *data1 = "dirfile/data1";
  const char *format1 = "dirfile/format1";

  const char *data2 = "dirfile/data2";
  const char *format2 = "dirfile/format2";

  int fd, error, error2, error3, r = 0;
  uint8_t data_data[4] = { 0, 1, 2, 3 };
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "FRAMEOFFSET 1\nINCLUDE format1\nINCLUDE format2");
  MAKEFORMATFILE(format1, "data1 RAW UINT8 1\n");
  MAKEFORMATFILE(format2, "data2 RAW UINT8 1\nFRAMEOFFSET 2");

  fd = open(data1, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 4);
  close(fd);

  fd = open(data2, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 4);
  close(fd);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);

  error = gd_error(D);
  CHECKI(error, 0);

  gd_getdata(D, "data1", 3, 0, 1, 0, GD_UINT8, data_data);

  error2 = gd_error(D);
  CHECKI(error2, 0);
  CHECKU(data_data[0], 2);

  gd_getdata(D, "data2", 3, 0, 1, 0, GD_UINT8, data_data);

  error3 = gd_error(D);
  CHECKI(error3, 0);
  CHECKU(data_data[0], 1);

  gd_discard(D);

  unlink(format);
  unlink(format1);
  unlink(format2);
  unlink(data1);
  unlink(data2);
  rmdir(filedir);

  return r;
}
