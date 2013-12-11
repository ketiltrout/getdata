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
#include <stdio.h>
#include <errno.h>

int main(void)
{
  const char *filedir = "dirfile";
  const char *data = "dirfile/data";
  const char *format = "dirfile/format";
  const char *format1 = "dirfile/format1";
  const char *format2 = "dirfile/format2";
  const char *format_data = "/INCLUDE format1\na CONST UINT8 1\n";
  const char *format1_data = "data RAW UINT8 11\n/INCLUDE format2\n";
  const char *format2_data = "c CONST UINT8 11\n";
  int fd, ret, e1, e2, unlink_format1, unlink_format2, r = 0;
  unsigned int nfields, nfragments;
  unsigned char data_data[256];
  uint8_t c = 3;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (unsigned char)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(format1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format1_data, strlen(format1_data));
  close(fd);

  fd = open(format2, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format2_data, strlen(format2_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 256);
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  gd_putdata(D, "data", 0, 0, 0, 1, GD_UINT8, &c);
  ret = gd_uninclude(D, 1, 0);
  e1 = gd_error(D);
  CHECKI(ret,0);
  CHECKI(e1, 0);

  nfields = gd_nfields(D);
  CHECKI(nfields,2);

  nfragments = gd_nfragments(D);
  CHECKI(nfragments,1);

  e2 = gd_close(D);
  CHECKI(e2, 0);

  unlink_format2 = unlink(format2);
  unlink_format1 = unlink(format1);
  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(unlink_format2,0);
  CHECKI(unlink_format1,0);

  return r;
}
