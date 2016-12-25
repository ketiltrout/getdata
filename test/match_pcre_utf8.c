/* Copyright (C) 2016 D. V. Wiebe
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
      "a\"\\u2029\"b CONST UINT8 1\n"
      "a\"\\u85\"b CONST UINT8 2\n"
      "a\" \"b CONST UINT8 3\n"
      );

  D = gd_open(filedir, GD_RDONLY);
  n = gd_match_entries(D, "a\\Rb", GD_ALL_FRAGMENTS, GD_ALL_ENTRIES,
      GD_REGEX_PCRE | GD_REGEX_UNICODE, &entry_list);
  error = gd_error(D);

  if (error == GD_E_ARGUMENT) { /* No utf-8 support in PCRE */
    CHECKU(n, 0);
    CHECKP(entry_list);
  } else {
    CHECKI(error, 0);
    CHECKU(n, 2);
    CHECKPN(entry_list);
    CHECKS(entry_list[0], "a\xC2\x85""b");
    CHECKS(entry_list[1], "a\xe2\x80\xa9""b");
    CHECKP(entry_list[2]);
  }

  gd_discard(D);
  unlink(format);
  rmdir(filedir);

  return r;
#endif
}
