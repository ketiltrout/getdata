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
  const char *table = "dirfile/table";
  int8_t c[8];
  int i, n, e1, r = 0;
  DIRFILE *D;
  FILE *t;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
      "linterp LINTERP data.r table\n"
      "data RAW COMPLEX128 1\n"
      );

  for (i = 0; i < 8; ++i)
    c[i] = (int8_t)(40 + i);

  t = fopen(table, "wt");
  for (i = 0; i < 10; ++i)
    fprintf(t, "%i %i\n", i * 6, i * 3);
  fclose(t);

  D = gd_open(filedir, GD_RDWR | GD_UNENCODED);
  n = gd_putdata(D, "linterp", 5, 0, 1, 0, GD_INT8, c);
  CHECKI(n, 0);

  e1 = gd_error(D);
  CHECKI(e1, GD_E_BAD_CODE);

  gd_discard(D);

  unlink(table);
  unlink(format);
  rmdir(filedir);

  return r;
}
