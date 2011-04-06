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
/* Attempt to read UINT8 with the opposite endianness */
#include "test.h"

#include <inttypes.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>

static int BigEndian(void)
{
  union {
    long int li;
    char ch[sizeof(long int)];
  } un;
  un.li = 1;
  return (un.ch[sizeof(long int) - 1] == 1);
}

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  char format_data[1000];
  uint8_t c = 0;
  uint8_t data_data[128];
  const int big_endian = BigEndian();
  int fd, n, error, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0777); 

  sprintf(format_data, "data RAW UINT8 1\nENDIAN %s\n", (big_endian)
      ? "little" : "big");

  for (fd = 0; fd < 128; ++fd)
    data_data[fd] = (uint8_t)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 128 * sizeof(uint8_t));
  close(fd);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n = gd_getdata(D, "data", 5, 0, 1, 0, GD_UINT8, &c);
  error = gd_error(D);

  gd_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(n, 1);
  CHECKX(c, 0x5);

  return r;
}
