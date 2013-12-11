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

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  uint8_t val[] = {0, 0, 0, 0, 0, 0, 0, 0};
  int r = 0, i;
  DIRFILE *D;

  rmdirfile();
  D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  gd_add_carray(D, "data", GD_UINT8, 8, GD_UINT8, &val, 0);
  gd_add_spec(D, "phase PHASE INDEX data<2>", 0);
  for (i = 0; i < 8; ++i)
    val[i] = i * (i + 1);

  gd_getdata(D, "phase", 0, 0, 0, 1, GD_UINT8, val);
  gd_put_carray(D, "data", GD_UINT8, &val);
  gd_getdata(D, "phase", 0, 0, 0, 1, GD_UINT8, val + 1);
  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(val[0], 0);
  CHECKI(val[1], 2 * (2 + 1));
  return r;
}
