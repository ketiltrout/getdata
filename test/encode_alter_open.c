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
  const char *txt_data =
    "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n";
  uint16_t c1, c2;
  int fd, ret, error, r = 0;
  off_t n1, n2;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "data RAW UINT16 8\nENCODING none\n");

  fd = open(txtdata, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, txt_data, strlen(txt_data));
  close(fd);

  MAKEDATAFILE(data, uint16_t, 0x201 * i, 128);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  n1 = gd_getdata(D, "data", 0, 3, 0, 1, GD_UINT16, &c1);
  ret = gd_alter_encoding(D, GD_TEXT_ENCODED, 0, 0);
  error = gd_error(D);
  n2 = gd_getdata(D, "data", 0, 3, 0, 1, GD_UINT16, &c2);

  gd_discard(D);

  unlink(txtdata);
  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(ret, 0);
  CHECKI(n1, 1);
  CHECKI(n2, 1);
  CHECKI(c1, 0x603);
  CHECKI(c2, 3);

  return r;
}
