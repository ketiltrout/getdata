/* Copyright (C) 2011, 2013, 2016, 2017 D.V. Wiebe
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
  const char *format1 = "dirfile/format1";
  int error, r = 0;
  unsigned int n;
  const char **entry_list;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
      "data1 RAW UINT8 1\n"
      "data2 RAW UINT8 1\n"
      "/HIDDEN data2\n"
      "/INCLUDE format1\n");
  MAKEFORMATFILE(format1, "data3 RAW UINT8 1\n");

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n = gd_match_entries(D, NULL, 1, GD_ALL_ENTRIES, 0, &entry_list);

  CHECKPN(entry_list);
  CHECKU(n, 1);
  CHECKP(entry_list[1]);
  CHECKS(entry_list[0], "data3");

  error = gd_error(D);

  CHECKI(error, 0);

  gd_discard(D);
  unlink(format);
  rmdir(filedir);

  return r;
}
