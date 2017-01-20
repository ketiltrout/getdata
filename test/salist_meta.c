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
  size_t i;
  const char ***field_list;
  DIRFILE *D;
  const char *lists[4][8] = {
    { "alfa", "bravo", "charlie", "delta", "echo" },
    { "foxtrot", "golf", "hotel", "india", "juliet", "kilo" },
    { "lima", "mike", "november", "oscar", "papa", "quebec", "romeo" },
    { "sierra", "tango", "uniform", "victor", "whiskey", "xray", "yankee",
      "zulu" }
  };

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "data0 SARRAY sierra tango uniform victor whiskey xray yankee zulu\n"
    "parent SARRAY nil\n"
    "parent/data1 SARRAY alfa bravo charlie delta echo\n"
    "parent/data2 SARRAY foxtrot golf hotel india juliet kilo\n"
    "parent/data3 SARRAY lima mike november oscar papa quebec romeo\n"
    "META parent data4 LINTERP UINT8 1\n"
    "/ALIAS parent/data5 data0\n"
  );

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  field_list = gd_msarrays(D, "parent");

  error = gd_error(D);
  CHECKI(error, 0);

  if (!r)
    for (j = 0; field_list[j]; ++j) {
      if (j > 4) {
        CHECKI(j, 4);
        break;
      }
      for (i = 0; field_list[j][i]; ++i)
        CHECKSi(j * 1000 + i,field_list[j][i], lists[j][i]);
      CHECKIi(j, i, 5 + j);
    }

  gd_discard(D);
  unlink(format);
  rmdir(filedir);

  return r;
}
