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
  uint8_t c[8];
  int d, n1, n2, e1, e2, e3;
  int i, r = 0;
  struct stat buf;
  FILE* stream;
  DIRFILE *D;

  memset(c, 0, 8);
  rmdirfile();
  mkdir(filedir, 0700);

  for (i = 0; i < 8; ++i)
    c[i] = (uint8_t)(40 + i);

  MAKEFORMATFILE(format, "data RAW UINT8 8\n");

  D = gd_open(filedir, GD_RDWR | GD_TEXT_ENCODED | GD_VERBOSE);
  n1 = gd_seek(D, "data", 5, 0, GD_SEEK_SET | GD_SEEK_WRITE);
  e1 = gd_error(D);
  CHECKI(e1,0);
  CHECKI(n1,40);

  n2 = gd_putdata(D, "data", GD_HERE, 0, 1, 0, GD_UINT8, c);
  e2 = gd_error(D);
  CHECKI(e2,0);
  CHECKI(n2,8);

  e3 = gd_close(D);
  CHECKI(e3, 0);

  if (stat(data, &buf))
    return 1;

  stream = fopen(data, "r");
  i = 0;
  for (;;) {
    fscanf(stream, "%i", &d);
    if (feof(stream))
      break;
    if (i < 40 || i > 48) {
      CHECKI(d, 0);
    } else
      CHECKI(d, i);
    i++;
  }
  fclose(stream);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  return r;
}
