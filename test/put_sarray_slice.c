/* Copyright (C) 2014 D. V. Wiebe
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
  const char *val[] = {"l", "m", "n", "o", "p", "q", "r", "s"};
  const char *val2[] = {"W", "X", "Y", "Z"};
  const char *result[] = {"l", "m", "W", "X", "Y", "Z", "r", "s"};
  int e1, e2, r = 0, i;
  DIRFILE *D;

  rmdirfile();
  D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  gd_add_sarray(D, "data", 8, val, 0);
  gd_put_sarray_slice(D, "data", 2, 4, val2);
  e1 = gd_error(D);
  CHECKI(e1, GD_E_OK);

  e2 = gd_close(D);
  CHECKI(e2, 0);

  /* check */
  memset(val, 0, 8 * sizeof(char*));
  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  gd_get_sarray(D, "data", val);

  for (i = 0; i < 8; ++i)
    CHECKSi(i, val[i], result[i]);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);
  return r;
}
