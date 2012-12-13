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
/* Attempt to read INT64 */
#include "test.h"

#include <inttypes.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  const char *format_data = "data RAW INT64 8\n";
  int64_t c[8];
  int64_t data_data[64];
  int fd, i, n, error, r = 0;
  DIRFILE *D;

  memset(c, 0, 8);
  rmdirfile();
  mkdir(filedir, 0777);

  for (fd = 0; fd < 64; ++fd)
    data_data[fd] = fd * (0x0200000000000001) * (2 * (fd % 2) - 1);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 64 * sizeof(int64_t));
  close(fd);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n = gd_getdata(D, "data", 5, 0, 1, 0, GD_UINT64, c);

  error = gd_error(D);
  CHECKI(error, 0);
  CHECKI(n, 8);

  for (i = 0; i < 8; ++i)
    CHECKIi(i,c[i], (0x5000000000000028 + i * 0x0200000000000001)
        * (2 * (i % 2) - 1));

  gd_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  return r;
}
