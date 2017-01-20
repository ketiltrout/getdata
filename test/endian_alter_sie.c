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
/* Test endianness */
#include "test.h"

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data.sie";
  const uint8_t data_in[] = {
    0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x12,
    0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x22,
    0x30, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x32
  };
  const uint8_t data_out[] = {
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x12,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x20, 0x22,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x30, 0x32
  };
  uint8_t c[8], d[3 * 9];
  int fd, ret, error, n, i = 0, r = 0;
  DIRFILE *D;

  memset(c, 0, 8);
  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "data RAW UINT8 8\nENDIAN little\nENCODING sie\n");

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_in, 3 * 9);
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  ret = gd_alter_endianness(D, GD_BIG_ENDIAN, 0, 1);
  error = gd_error(D);
  n = gd_getdata(D, "data", 3, 0, 1, 0, GD_UINT8, c);

  gd_discard(D);

  fd = open(data, O_RDONLY | O_BINARY);

  if (fd >= 0) {
    read(fd, d, 3 * 9);
    close(fd);
  } else {
    perror("open");
    r = 1;
  }

  unlink(data);
  unlink(format);
  rmdir(filedir);

  for (i = 0; i < 8; ++i)
    CHECKXi(i,c[i], 0x22);

  for (i = 0; i < 3 * 9; ++i)
    CHECKXi(i,d[i], data_out[i]);

  CHECKI(error,0);
  CHECKI(ret, 0);
  CHECKI(n, 8);

  return r;
}
