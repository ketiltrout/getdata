/* Copyright (C) 2016 D. V. Wiebe
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
  int8_t c = 0;
  int e1, e2, r = 0;
  size_t n1, n2;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "data RAW UINT8 1\n");

  D = gd_open(filedir, GD_RDWR | GD_UNENCODED);
  n1 = gd_putdata(D, "data", 10, -10000, 0, 1, GD_INT8, &c);
  CHECKU(n1, 0);
  e1 = gd_error(D);
  CHECKI(e1, GD_E_RANGE);

  n2 = gd_putdata(D, "data", -10, 0, 0, 1, GD_INT8, &c);
  CHECKU(n2, 0);
  e2 = gd_error(D);
  CHECKI(e2, GD_E_RANGE);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
