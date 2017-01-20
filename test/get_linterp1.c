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
  const char *data = "dirfile/data";
  const char *table = "dirfile/table";
  unsigned char c = 0;
  int n, error, r = 0;
  DIRFILE *D;
  FILE *t;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "linterp LINTERP data ./table\ndata RAW UINT8 1\n");
  MAKEDATAFILE(data, unsigned char, i, 64);

  t = fopen(table, "wt");
  fputs("0 0\n", t);
  fclose(t);

  D = gd_open(filedir, GD_RDONLY);
  n = gd_getdata(D, "linterp", 5, 0, 1, 0, GD_UINT8, &c);
  error = gd_error(D);

  gd_discard(D);

  unlink(table);
  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, GD_E_LUT);
  CHECKI(n, 0);

  return r;
}
