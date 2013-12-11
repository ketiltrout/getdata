/* Copyright (C) 2013 D. V. Wiebe
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
  uint16_t c1, c2;
  int fd, ret, e1, e2, unlink_txtdata, unlink_data, r = 0;
  DIRFILE *D;
  off_t n1, n2;

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
