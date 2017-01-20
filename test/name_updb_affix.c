/* Copyright (C) 2013, 2016, 2017 D.V. Wiebe
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
  int r1, r2, r3, e1, e2, e3, e4, e5, e6, r = 0;
  DIRFILE *D;
  gd_entry_t E;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "data RAW UINT8 8\n"
    "AphaseZ PHASE data 0\n"
    "AphaseZ/meta CONST UINT8 3\n"
    "/INCLUDE format1 A Z\n"
  );
  MAKEFORMATFILE(format1, "bit BIT phase phase/meta 1\n");

  D = gd_open(filedir, GD_RDWR);

  gd_validate(D, "AbitZ");
  e1 = gd_error(D);
  CHECKI(e1, 0);

  gd_entry(D, "AbitZ", &E);
  e2 = gd_error(D);
  CHECKI(e2, 0);
  CHECKSi(1, E.in_fields[0], "AphaseZ");
  CHECKSi(1, E.scalar[0], "AphaseZ/meta");
  gd_free_entry_strings(&E);

  r1 = gd_rename(D, "AphaseZ", "zata", GD_REN_UPDB);
  e3 = gd_error(D);
  CHECKI(r1, GD_E_BAD_CODE);
  CHECKI(e3, GD_E_BAD_CODE);

  r2 = gd_rename(D, "AphaseZ", "zata", GD_REN_UPDB | GD_REN_FORCE);
  e4 = gd_error(D);
  CHECKI(r2, 0);
  CHECKI(e4, 0);

  gd_validate(D, "AbitZ");
  e5 = gd_error(D);
  CHECKI(e5, GD_E_BAD_SCALAR);

  gd_entry(D, "AbitZ", &E);
  e6 = gd_error(D);
  CHECKI(e6, 0);
  CHECKSi(2, E.in_fields[0], "AphaseZ");
  CHECKSi(2, E.scalar[0], "AphaseZ/meta");
  gd_free_entry_strings(&E);

  r3 = gd_close(D);
  CHECKI(r3, 0);

  unlink(format);
  unlink(format1);
  rmdir(filedir);

  return r;
}
