/* Copyright (C) 2008-2011 D. V. Wiebe
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

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <inttypes.h>
#include <stdio.h>

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  const char *txtdata = "dirfile/data.txt";
  const char *format_data = "data RAW UINT16 8\nENCODING none\n";
  uint16_t data_data[128];
  uint16_t c[8];
  int fd, i, ret, error, n, unlink_txtdata, unlink_data, r = 0;
  DIRFILE *D;

  memset(c, 0, 8);
  rmdirfile();
  mkdir(filedir, 0777);

  for (fd = 0; fd < 128; ++fd)
    data_data[fd] = 0x201 * fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 256);
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  ret = gd_alter_encoding(D, GD_TEXT_ENCODED, 0, 1);
  error = gd_error(D);
  n = gd_getdata(D, "data", 5, 0, 1, 0, GD_UINT16, c);

  gd_close(D);

  unlink_txtdata = unlink(txtdata);
  unlink_data = unlink(data);
  unlink(format);
  rmdir(filedir);

  for (i = 0; i < 8; ++i)
    CHECKXi(i,c[i], (40 + i) * 0x201);

  CHECKI(error, 0);
  CHECKI(ret, 0);
  CHECKI(n, 8);
  CHECKI(unlink_txtdata, 0);
  CHECKI(unlink_data, -1);

  return r;
}
