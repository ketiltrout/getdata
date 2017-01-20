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
  const char *format2 = "dirfile/format2";
  int r1, r2, r3, e1, e2, e3, r = 0;
  const char *s1;
  gd_entry_t E;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "INCLUDE format1 A B\n"
    "INCLUDE format2 C D\n"
    "phase PHASE AdataB 0\n"
    "/ALIAS alias AdataB\n"
  );
  MAKEFORMATFILE(format1,
    "data RAW UINT8 11\n"
    "data/meta CONST UINT8 1\n"
  );
  MAKEFORMATFILE(format2, "#\n");

  D = gd_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);

  r1 = gd_move(D, "AdataB", 2, GD_REN_UPDB);
  e1 = gd_error(D);
  CHECKI(r1, 0);
  CHECKI(e1, GD_E_OK);

  r2 = gd_entry(D, "CdataD", &E);
  CHECKI(r2, 0);
  CHECKI(E.fragment_index, 2);
  gd_free_entry_strings(&E);

  gd_validate(D, "phase");
  e2 = gd_error(D);
  CHECKI(e2, 0);

  gd_validate(D, "CdataD/meta");
  e3 = gd_error(D);
  CHECKI(e3, 0);

  s1 = gd_alias_target(D, "alias");
  CHECKS(s1, "CdataD");

  r3 = gd_close(D);
  CHECKI(r3, 0);

  unlink(format2);
  unlink(format1);
  unlink(format);
  rmdir(filedir);


  return r;
}
