/* Copyright (C) 2015 D. V. Wiebe
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

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data.sie";
  uint8_t check[9];
  uint8_t c[6], d[16];
  DIRFILE *D;
  int fd, i, j, n1, n2, n3, e1, e2, e3, r = 0;

  rmdirfile();

  for (i = 0; i < 6; ++i)
    c[i] = i;

  D = gd_open(filedir, GD_RDWR | GD_SIE_ENCODED | GD_CREAT | GD_LITTLE_ENDIAN
      | GD_VERBOSE);
  n1 = gd_add_raw(D, "data", GD_UINT8, 20, 0);
  e1 = gd_error(D);
  CHECKI(n1, 0);
  CHECKI(e1, 0);
  n2 = gd_putdata(D, "data", 0, 10, 0, 6, GD_UINT8, c);
  e2 = gd_error(D);
  CHECKI(n2, 6);
  CHECKI(e2, 0);
  gd_close(D);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n3 = gd_getdata(D, "data", 0, 0, 0, 16, GD_UINT8, d);
  e3 = gd_error(D);
  CHECKI(n3, 16);
  CHECKI(e3, 0);

  for (i = 0; i < 16; ++i)
    if (i < 10)
      CHECKIi(i, d[i], 0);
    else
      CHECKIi(i, d[i], c[i - 10]);

  gd_discard(D);

  fd = open(data, O_RDONLY | O_BINARY);

  j = 0;
  while (read(fd, check, 9) == 9) {
    CHECKIi(j * 10 + 0, check[0], 10 + j);
    for (i = 1; i < 8; ++i)
      CHECKIi(j * 10 + i, check[i], 0);
    CHECKIi(j * 10 + 8, check[8], j);
    j++;
  }
  CHECKI(j, 6);

  close(fd);

  unlink(data);
  unlink(format);
  rmdir(filedir);
  return r;
}
