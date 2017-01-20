/* Copyright (C) 2015, 2017 D.V. Wiebe
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
  unsigned char c[8];
  int i, n1, n2, e1, e2, r = 0;
  DIRFILE *D;
  FILE* stream;

  memset(c, 0, 8);
  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "data RAW UINT8 8\n");

  stream = fopen(data, "w");
  for (i = 0; i < 256; ++i)
    fprintf(stream, "%i\n", i);
  fclose(stream);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n1 = gd_seek(D, "data", 5, 0, GD_SEEK_SET);
  e1 = gd_error(D);

  CHECKI(n1, 40);
  CHECKI(e1, 0);

  n2 = gd_getdata(D, "data", GD_HERE, 0, 1, 0, GD_UINT8, c);
  e2 = gd_error(D);

  CHECKI(n2, 8);
  CHECKI(e2, 0);
  for (i = 0; i < 8; ++i)
    CHECKIi(i,c[i], 40 + i);

  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);


  return r;
}
