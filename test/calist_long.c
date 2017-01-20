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
  int j, error, r = 0;
  struct uint8_carrays {
    size_t n;
    uint8_t *d;
  } *field_list;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
      "data00 CARRAY UINT8 0\n"
      "data01 CARRAY UINT8 1\n"
      "data02 CARRAY UINT8 2\n"
      "data03 CARRAY UINT8 3\n"
      "data04 CARRAY UINT8 4\n"
      "data05 CARRAY UINT8 5\n"
      "data06 CARRAY UINT8 6\n"
      "data07 CARRAY UINT8 7\n"
      "data08 CARRAY UINT8 8\n"
      "data09 CARRAY UINT8 9\n"
      "data10 CARRAY UINT8 10\n"
      "data11 CARRAY UINT8 11\n"
      "data12 CARRAY UINT8 12\n"
      "data13 CARRAY UINT8 13\n"
      "data14 CARRAY UINT8 14\n"
      "data15 CARRAY UINT8 15\n"
      "data16 CARRAY UINT8 16\n"
      "data17 CARRAY UINT8 17\n"
      "data18 CARRAY UINT8 18\n"
      "data19 CARRAY UINT8 19\n"
      );

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  field_list = (struct uint8_carrays*)gd_carrays(D, GD_UINT8);

  error = gd_error(D);

  CHECKI(error, 0);

  if (!r) {
    for (j = 0; field_list[j].n; ++j) {
      CHECKUi(j,field_list[j].n, 1);
      CHECKUi(j,field_list[j].d[0], j);
    }
    CHECKI(j, 20);
  }

  gd_discard(D);
  unlink(format);
  rmdir(filedir);

  return r;
}
