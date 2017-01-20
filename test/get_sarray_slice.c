/* Copyright (C) 2014, 2017 D.V. Wiebe
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
  int i, n, error, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  memset(data, 0, sizeof(data));

  MAKEFORMATFILE(format, "sarray SARRAY a b c d e f g h i j k l m\n");

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n = gd_get_sarray_slice(D, "sarray", 4, 5, data);
  error = gd_error(D);

  CHECKI(error, 0);
  CHECKI(n, 0);
  for (i = 0; i < 5; ++i) {
    CHECKPNi(i, data[i]);
    CHECKXi(i, data[i][0], 'e' + i);
    CHECKXi(i, data[i][1], 0);
  }

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
