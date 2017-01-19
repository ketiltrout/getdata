/* Copyright (C) 2008-2011, 2013, 2015, 2016, 2017 D. V. Wiebe
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
  const char **field_list;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
      "data1 RAW UINT8 1\n"
      "data2 RAW UINT8 1\n"
      "data3 RAW UINT8 1\n"
      "data4 CONST UINT8 1\n"
      "data4/sub LINCOM data1 1 0 data2 1 0\n"
      "data5 SINDIR in in\n");

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  field_list = gd_vector_list(D);

  error = gd_error(D);
  CHECKI(error,0);
  CHECKPN(field_list);

  for (i = 0; field_list[i]; ++i) {
    if (strcmp(field_list[i], "data1") == 0)
      continue;
    else if (strcmp(field_list[i], "data2") == 0)
      continue;
    else if (strcmp(field_list[i], "data3") == 0)
      continue;
    else if (strcmp(field_list[i], "INDEX") == 0)
      continue;

    r = 1;
  }

  CHECKI(i,4);

  gd_discard(D);
  unlink(format);
  rmdir(filedir);

  return r;
}
