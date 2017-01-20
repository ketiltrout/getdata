/* Copyright (C) 2010-2011, 2013, 2015, 2017 D.V. Wiebe
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
  int j, error, r = 0;
  size_t i;
  struct uint8_carrays {
    size_t n;
    uint8_t *d;
  } *field_list;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "data1 CARRAY UINT8 1 2 3 4 5\n"
    "data2 CARRAY UINT8 2 4 6 8 10 12\n"
    "data3 CARRAY UINT8 3 6 9 12 15 18 21\n"
    "data4 RAW UINT8 1\n"
    "data4/meta CARRAY UINT8 4 8 12 16 20 24 28 32\n"
    "/ALIAS data5 data4/meta\n"
  );

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  field_list = (struct uint8_carrays*)gd_carrays(D, GD_UINT8);

  error = gd_error(D);

  CHECKI(error, 0);

  if (!r)
    for (j = 0; j < 4; ++j) {
      CHECKUi(j,field_list[j].n, (size_t)(5 + j));
      for (i = 0; i < field_list[j].n; ++i)
        CHECKUi(j * 1000 + i,field_list[j].d[i], (j + 1) * (i + 1));
    }

  gd_discard(D);
  unlink(format);
  rmdir(filedir);

  return r;
}
