/* Copyright (C) 2016, 2017 D. V. Wiebe
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
#ifdef GD_NO_PCRE
  return 77; /* skip test */
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  int error, r = 0;
  const char **entry_list;
  unsigned int n;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
      "ae CONST UINT8 1\n"
      "abe CONST UINT8 1\n"
      "abet CONST UINT8 1\n"
      "ade CONST UINT8 1\n"
      "ape CONST UINT8 1\n"
      "abbe CONST UINT8 1\n"
      "ate CONST UINT8 1\n");

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n = gd_match_entries(D, "a[bcd]*# This is a comment\ne$", GD_ALL_FRAGMENTS,
      GD_ALL_ENTRIES, GD_REGEX_PCRE | GD_REGEX_EXTENDED, &entry_list);

  CHECKU(n, 4);

  error = gd_error(D);

  CHECKI(error, 0);
  CHECKPN(entry_list);
  CHECKSA(entry_list, n, 0, "ae");
  CHECKSA(entry_list, n, 1, "abe");
  CHECKSA(entry_list, n, 2, "ade");
  CHECKSA(entry_list, n, 3, "abbe");
  CHECKPA(entry_list, n, 4);

  gd_discard(D);
  unlink(format);
  rmdir(filedir);

  return r;
#endif
}
