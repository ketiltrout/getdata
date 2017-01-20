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
  const char *data = "dirfile/data";
  int8_t c[8];
  struct stat buf;
  int i, n, e1, e2, r = 0;
  DIRFILE *D;

  memset(c, 0, 8);
  rmdirfile();
  mkdir(filedir, 0700);

  for (i = 0; i < 8; ++i)
    c[i] = (int8_t)(40 + i);

  MAKEFORMATFILE(format,
    "indir INDIR data carray\n"
    "carray CARRAY INT8 1 2 3 4 5 6 7 8 9 0\n"
    "data RAW INT8 8\n"
  );

  D = gd_open(filedir, GD_RDWR | GD_UNENCODED);
  n = gd_putdata(D, "indir", 5, 0, 1, 0, GD_INT8, c);
  e1 = gd_error(D);
  CHECKI(n,0);
  CHECKI(e1,GD_E_BAD_FIELD_TYPE);

  e2 = gd_close(D);
  CHECKI(e2, 0);

  if (!stat(data, &buf)) {
    perror("stat");
    r = 1;
  }

  unlink(format);
  rmdir(filedir);

  return r;
}
