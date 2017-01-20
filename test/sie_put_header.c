/* Copyright (C) 2011, 2013, 2016, 2017 D.V. Wiebe
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
  const char *data = "dirfile/data.sie";
  unsigned char c[16] = {
    0x23, 0x23, 0x23, 0x34, 0x34, 0x34, 0x34, 0x34,
    0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34, 0x34
  };
  const uint8_t data_data[] = {
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x81,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x12,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x20, 0x22,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x30, 0x32
  };
#define NREC (4 + 1)
  const uint8_t data_out[] = {
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x81,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x12,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x13, 0x23,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x20, 0x34,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x30, 0x32
  };
  uint8_t check[(NREC + 1) * 9];
  DIRFILE *D;
  int fd, i, n, error, r = 0;
  ssize_t s;

  rmdirfile();
  mkdir(filedir, 0700); 

  MAKEFORMATFILE(format, "data RAW UINT8 8\n/ENCODING sie\n/ENDIAN big\n");

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, sizeof(data_data));
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  n = gd_putdata(D, "data", 0, 0x11, 2, 0, GD_UINT8, c);
  error = gd_error(D);

  CHECKI(error, 0);
  CHECKI(n, 16);

  gd_discard(D);

  fd = open(data, O_RDONLY | O_BINARY);
  s = read(fd, check, (NREC + 1) * 9);
  close(fd);
  CHECKI(s, NREC * 9);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  for (i = 0; i < NREC * 9; ++i)
    CHECKXi(i, check[i], data_out[i]);

  return r;
}
