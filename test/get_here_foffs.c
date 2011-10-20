/* Copyright (C) 2011 D. V. Wiebe
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
/* Check GD_HERE and FRAMEOFFSET */
#include "test.h"

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
  const char *format_data = "FRAMEOFFSET 2\ndata RAW UINT8 8\n";
  unsigned char c[8], d[8];
  unsigned char data_data[256];
  int fd, i, j, k, l, m, n, e1, e2, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (unsigned char)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 256);
  close(fd);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n = gd_seek(D, "data", 0, 4, GD_SEEK_SET);
  m = gd_getdata(D, "data", GD_HERE, 0, 1, 0, GD_UINT8, c);
  e1 = gd_error(D);
  l = gd_tell(D, "data");
  k = gd_getdata(D, "data", GD_HERE, 0, 1, 0, GD_UINT8, d);
  e2 = gd_error(D);
  j = gd_tell(D, "data");

  gd_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(e1, 0);
  CHECKI(e2, 0);
  CHECKI(n, 4);
  CHECKI(m, 8);
  CHECKI(l, 12);
  CHECKI(k, 8);
  CHECKI(j, 20);

  for (i = 0; i < 8; ++i) {
    CHECKUi(i, c[i], 0);
    CHECKUi(i, d[i], (i < 4) ? 0 : i - 4);
  }

  return r;
}
