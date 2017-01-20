/* Copyright (C) 2013, 2017 D.V. Wiebe
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
  int ret, e1, e2, e3, e4, e5, r = 0;
  gd_entry_t E;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "INCLUDE format1 A Z\n"
    "data RAW UINT8 11\n"
    "data/meta1 CONST UINT8 1\n"
    "data/meta2 CONST UINT8 2\n"
    "/ALIAS data/meta3 data/meta2\n"
  );
  MAKEFORMATFILE(format1, "#\n");

  D = gd_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);
  ret = gd_move(D, "data", 1, 0);
  e1 = gd_error(D);

  CHECKI(ret, 0);
  CHECKI(e1, GD_E_OK);

  gd_entry(D, "AdataZ", &E);
  e2 = gd_error(D);
  CHECKI(e2, 0);
  CHECKI(E.fragment_index, 1);
  gd_free_entry_strings(&E);

  gd_entry(D, "AdataZ/meta1", &E);
  e3 = gd_error(D);
  CHECKI(e3, 0);
  CHECKI(E.fragment_index, 1);
  gd_free_entry_strings(&E);

  gd_entry(D, "AdataZ/meta2", &E);
  e4 = gd_error(D);
  CHECKI(e4, 0);
  CHECKI(E.fragment_index, 1);
  gd_free_entry_strings(&E);

  gd_entry(D, "AdataZ/meta3", &E);
  e5 = gd_error(D);
  CHECKI(e5, 0);
  CHECKI(E.fragment_index, 1);
  gd_free_entry_strings(&E);

  gd_discard(D);

  unlink(format1);
  unlink(format);
  rmdir(filedir);

  gd_free_entry_strings(&E);

  return r;
}
