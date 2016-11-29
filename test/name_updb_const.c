/* Copyright (C) 2011-2013, 2016 D. V. Wiebe
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
  int e1, e2, e3, r = 0;
  const char *s3;
  DIRFILE *D;
  gd_entry_t E;

  memset(&E, 0, sizeof(E));
  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "early RAW UINT8 c\n"
    "late LINCOM early c 0\n"
    "polynom POLYNOM INDEX 1 c 1\n"
    "/ALIAS b c\n"
    "c CONST UINT8 2\n"
    );

  D = gd_open(filedir, GD_RDWR);
  gd_validate(D, "early");

  gd_rename(D, "c", "d", GD_REN_UPDB);
  e1 = gd_error(D);
  CHECKI(e1,0);

  gd_spf(D, "early");
  e2 = gd_error(D);
  CHECKI(e2,0);

  gd_entry(D, "early", &E);
  CHECKSi(1, E.scalar[0], "d");
  gd_free_entry_strings(&E);

  gd_entry(D, "late", &E);
  CHECKSi(2, E.scalar[0], "d");
  gd_free_entry_strings(&E);

  gd_entry(D, "b", &E);
  e3 = gd_error(D);
  s3 = gd_alias_target(D, "b");
  CHECKI(e3,0);
  CHECKS(s3, "d");
  gd_free_entry_strings(&E);

  gd_entry(D, "polynom", &E);
  CHECKS(E.scalar[1], "d");
  gd_free_entry_strings(&E);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
