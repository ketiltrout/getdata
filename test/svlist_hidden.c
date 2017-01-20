/* Copyright (C) 2011, 2013, 2015, 2017 D.V. Wiebe
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
  int i, error, r = 0;
  const char **data;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "data1 STRING valu2\n"
    "data2 STRING valu4\n"
    "/HIDDEN data2\n"
    "data3 STRING valu3\n"
    "/ALIAS data4 data2\n"
    "data5 STRING valu5\n"
    "/ALIAS data6 data5\n"
    "/HIDDEN data6\n"
  );

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  data = gd_strings(D);

  error = gd_error(D);
  CHECKI(error, 0);
  CHECKPN(data);

  for (i = 0; data[i]; ++i) {
    int len = strlen(data[i]);
    CHECKIi(i,len,5);

    if (len == 5) {
      CHECKIi(i,data[i][0], 'v');
      CHECKIi(i,data[i][1], 'a');
      CHECKIi(i,data[i][2], 'l');
      CHECKIi(i,data[i][3], 'u');
      CHECKIi(i,data[i][4], '2' + i);
    }
  }

  CHECKI(i,4);

  gd_discard(D);
  unlink(format);
  rmdir(filedir);

  return r;
}
