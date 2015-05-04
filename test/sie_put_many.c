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
  uint8_t c[60], d[60];
  DIRFILE *D;
  int i, n1, n2, n3, e1, e2, e3, r = 0;

  rmdirfile();

  for (i = 0; i < 60; ++i)
    c[i] = i;

  D = gd_open(filedir, GD_RDWR | GD_SIE_ENCODED | GD_CREAT | GD_LITTLE_ENDIAN
      | GD_VERBOSE);
  n1 = gd_add_raw(D, "data", GD_UINT8, 20, 0);
  e1 = gd_error(D);
  CHECKI(n1, 0);
  CHECKI(e1, 0);
  n2 = gd_putdata(D, "data", 0, 10, 0, 60, GD_UINT8, c);
  e2 = gd_error(D);
  CHECKI(n2, 60);
  CHECKI(e2, 0);
  gd_close(D);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n3 = gd_getdata(D, "data", 0, 10, 0, 60, GD_UINT8, d);
  e3 = gd_error(D);
  CHECKI(n3, 60);
  CHECKI(e3, 0);

  for (i = 0; i < 60; ++i)
    CHECKIi(i, c[i], d[i]);

  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);
  return r;
}
