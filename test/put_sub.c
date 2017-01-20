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
  const char *subdir = "dirfile/sub";
  const char *format = "dirfile/format";
  const char *format1 = "dirfile/sub/format1";
  const char *data = "dirfile/sub/data";
  uint8_t c[8], d;
  int fd, i, n, e1, e2, r = 0;
  DIRFILE *D;
  struct stat buf;

  for (i = 0; i < 8; ++i)
    c[i] = (uint8_t)(40 + i);

  rmdirfile();
  mkdir(filedir, 0700);
  mkdir(subdir, 0700);

  MAKEFORMATFILE(format, "/INCLUDE sub/format1\n");
  MAKEFORMATFILE(format1, "data RAW UINT8 8\n");

  D = gd_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);
  n = gd_putdata(D, "data", 5, 0, 1, 0, GD_UINT8, c);
  e1 = gd_error(D);
  CHECKI(n,8);
  CHECKI(e1, 0);

  e2 = gd_close(D);
  CHECKI(e2, 0);

  if (stat(data, &buf)) {
    perror("stat");
    r = 1;
  } else
    CHECKI(buf.st_size, 48 * sizeof(uint8_t));

  fd = open(data, O_RDONLY | O_BINARY);
  if (fd < 0) {
    perror("open");
    r = 1;
  } else {
    i = 0;
    while (read(fd, &d, sizeof(uint8_t))) {
      if (i < 40 || i > 48) {
        CHECKUi(i,d,0);
      } else
        CHECKUi(i,d,i);
      i++;
    }
    close(fd);
  }

  unlink(data);
  unlink(format1);
  unlink(format);
  rmdir(subdir);
  rmdir(filedir);

  return r;
}
