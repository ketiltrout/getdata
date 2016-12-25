/* Copyright (C) 2011, 2013, 2016 D. V. Wiebe
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
  const char **entry_list;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
      "data1 RAW UINT8 1\n"
      "data2 RAW UINT8 1\n"
      "data3 RAW UINT8 1\n"
      "/ALIAS data4 data32\n");

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  entry_list = gd_entry_list(D, NULL, GD_ALL_ENTRIES, GD_ENTRIES_NOALIAS);

  error = gd_error(D);

  CHECKI(error, 0);
  CHECKPN(entry_list);

  for (i = 0; ; ++i) {
    if (entry_list[i] == NULL)
      break;

    if (strcmp(entry_list[i], "data1") == 0)
      continue;
    else if (strcmp(entry_list[i], "data2") == 0)
      continue;
    else if (strcmp(entry_list[i], "data3") == 0)
      continue;
    else if (strcmp(entry_list[i], "INDEX") == 0)
      continue;

    fprintf(stderr, "entry_list[%i] = \"%s\"\n", i, entry_list[i]);
    r = 1;
  }

  CHECKI(i, 4);

  gd_discard(D);
  unlink(format);
  rmdir(filedir);

  return r;
}
