/* Copyright (C) 2016, 2017 D.V. Wiebe
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
  const char *data[5];
  int e1, e2, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "sarray SARRAY 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14\n");

  D = gd_open(filedir, GD_RDONLY);
  e1 = gd_get_sarray_slice(D, "sarray", 4, 50, data);
  CHECKI(e1, GD_E_BOUNDS);

  e2 = gd_get_sarray_slice(D, "sarray", 40, 5, data);
  CHECKI(e2, GD_E_BOUNDS);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
