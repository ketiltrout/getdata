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
  const char *format = "dirfile/format";
  const char *cata = "dirfile/cata";
  const char *data = "dirfile/data";
  uint8_t c[8], d;
  int fd, i, n, e1, e2, r = 0;
  struct stat buf;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  for (i = 0; i < 8; ++i)
    c[i] = (uint8_t)(40 + i);

  MAKEFORMATFILE(format,
    "cata RAW UINT8 8\n"
    "data RAW UINT8 8\n"
    "mult MULTIPLY cata data\n"
  );

  D = gd_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);
  n = gd_putdata(D, "data", 5, 0, 1, 0, GD_UINT8, c);
  gd_flush(D, "mult");
  e1 = gd_error(D);
  CHECKI(e1, 0);
  CHECKI(n, 8);

  e2 = gd_close(D);
  CHECKI(e2, 0);

  if (stat(data, &buf)) {
    perror("stat(data)");
    r = 1;
  } else
    CHECKI(buf.st_size, 40 + 8 * sizeof(uint8_t));

  if (!stat(cata, &buf)) {
    perror("stat(cata)");
    r = 1;
  }

  fd = open(data, O_RDONLY | O_BINARY);
  i = 0;
  while (read(fd, &d, sizeof(uint8_t))) {
    CHECKIi(i, d, (i < 40 || i > 48) ? 0 : i);
    i++;
  }
  close(fd);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  return r;
}
