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
#include "../src/config.h"
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
  const char *format_data = "data RAW UINT16 8\nENDIAN little\n";
  uint16_t data_data[128];
  uint16_t c[8], d;
  int fd, ret, error, n, i = 0, r = 0;
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
  ret = gd_alter_endianness(D, GD_BIG_ENDIAN, 0, 1);
  error = gd_error(D);
  n = gd_getdata(D, "data", 5, 0, 1, 0, GD_UINT16, c);

  gd_close(D);

  fd = open(data, O_RDONLY | O_BINARY);

  if (fd >= 0) {
    while (read(fd, &d, sizeof(uint16_t))) {
      CHECKX(d, i * 0x102);
      i++;
    }
    close(fd);
  } else {
    perror("open");
    r = 1;
  }

  unlink(data);
  unlink(format);
  rmdir(filedir);

#ifdef WORDS_BIGENDIAN
# define FACTOR 0x102
#else
# define FACTOR 0x201
#endif

  for (i = 0; i < 8; ++i)
    CHECKXi(i,c[i], (40 + i) * FACTOR);

  CHECKI(error, 0);
  CHECKI(ret, 0);
  CHECKI(n, 8);

  return r;
}
