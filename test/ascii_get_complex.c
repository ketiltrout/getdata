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
  const char *data = "dirfile/data.txt";
  double c[16];
  int i, n, error, r = 0;
  DIRFILE *D;
  FILE* stream;

  memset(c, 0, 8);
  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "data RAW COMPLEX128 8\n");

  stream = fopen(data, "w");
  for (i = 0; i < 256; ++i)
    fprintf(stream, "%i;%g\n", i, 0.1 * i);
  fclose(stream);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n = gd_getdata(D, "data", 5, 0, 1, 0, GD_COMPLEX128, c);
  error = gd_error(D);

  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(n, 8);
  for (i = 0; i < 16; ++i)
    CHECKFi(i,c[i], (i % 2) ? 0.1 * (40 + (i /2)) : 40 + (i / 2));

  return r;
}
