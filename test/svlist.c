/* Copyright (C) 2008-2011, 2013, 2017 D.V. Wiebe
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
/* Retreiving the number of fields of a field should succeed cleanly */
#include "test.h"

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  int i, error, r = 0;
  const char **field_list;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "data1 STRING valu1\n"
    "data2 STRING valu2\n"
    "data3 STRING valu3\n"
    "data4 CONST UINT8 1\n"
  );

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  field_list = gd_strings(D);

  error = gd_error(D);
  CHECKI(error, 0);
  CHECKPN(field_list);

  for (i = 0; field_list[i]; ++i) {
    int len = strlen(field_list[i]);
    CHECKIi(i,len,5);

    CHECKIi(i,field_list[i][0], 'v');
    CHECKIi(i,field_list[i][1], 'a');
    CHECKIi(i,field_list[i][2], 'l');
    CHECKIi(i,field_list[i][3], 'u');

    if (field_list[i][4] < '1' || field_list[i][4] > '3') {
      fprintf(stderr, "field_list[%i] = \"%s\"\n", i, field_list[i]);
      r = 1;
    }
  }

  CHECKI(i,3);

  gd_discard(D);
  unlink(format);
  rmdir(filedir);

  return r;
}
