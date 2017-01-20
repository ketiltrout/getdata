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
  const char *table = "dirfile/table";
  unsigned char c = 0;
  int fd, n1, error1, n2, error2, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "linterp LINTERP data ./table\ndata RAW UINT8 1\n");

  fd = open(table, O_CREAT | O_EXCL | O_WRONLY, 0666);
  close(fd);

  MAKEDATAFILE(data, unsigned char, i, 64);

  D = gd_open(filedir, GD_RDONLY);
  n1 = gd_getdata(D, "linterp", 5, 0, 1, 0, GD_UINT8, &c);
  error1 = gd_error(D);
  n2 = gd_getdata(D, "linterp", 5, 0, 1, 0, GD_UINT8, &c);
  error2 = gd_error(D);

  gd_discard(D);

  unlink(data);
  unlink(format);
  unlink(table);
  rmdir(filedir);

  CHECKI(n1, 0);
  CHECKI(error1, GD_E_LUT);
  CHECKI(n2, 0);
  CHECKI(error2, GD_E_LUT);

  return r;
}
