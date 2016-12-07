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
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *format1 = "dirfile/format1";
  int e1, e2, e3, e4, e5, e6, e7, r = 0;
  DIRFILE *D;
  gd_entry_t E;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "/INCLUDE format1 [ ]\n");
  MAKEFORMATFILE(format1,
      "aaaa CONST UINT8 1\n"
      "aaaa/sub CONST UINT8 2\n"
      "/ALIAS cccc aaaa\n"
      "/HIDDEN aaaa/sub\n"
      "dddd BIT in aaaa cccc/sub\n"
      );

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);

  e1 = gd_rewrite_fragment(D, 1);
  CHECKI(e1, 0);

  e2 = gd_close(D);
  CHECKI(e2, 0);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);

  e3 = gd_entry(D, "[aaaa]", &E);
  if (e3)
    CHECKI(e3, 0);
  else
    gd_free_entry_strings(&E);

  e4 = gd_entry(D, "[aaaa]/sub", &E);
  if (e4)
    CHECKI(e4, 0);
  else
    gd_free_entry_strings(&E);

  e5 = gd_entry(D, "[cccc]", &E);
  if (e5)
    CHECKI(e5, 0);
  else {
    CHECKS(E.field, "[aaaa]");
    gd_free_entry_strings(&E);
  }

  e6 = gd_hidden(D, "[aaaa]/sub");
  CHECKI(e6, 1);

  e7 = gd_entry(D, "[dddd]", &E);
  if (e7)
    CHECKI(e7, 0);
  else {
    CHECKS(E.scalar[0], "[aaaa]");
    CHECKS(E.scalar[1], "[cccc]/sub");
    CHECKI(E.EN(bit,bitnum), 1);
    CHECKI(E.EN(bit,numbits), 2);
    gd_free_entry_strings(&E);
  }

  gd_discard(D);

  unlink(format1);
  unlink(format);
  rmdir(filedir);

  return r;
}
