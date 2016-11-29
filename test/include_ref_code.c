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
  int e1, e2, e3, e4, e5, r = 0;
  unsigned int n;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "remaining CONST UINT8 1\n");
  
  MAKEFORMATFILE(format1,
      "one CONST UINT8 1\n"
      "before CONST UINT8 1\n"
      "after CONST UINT8 1\n"
      "/REFERENCE missing\n"
      );

  D = gd_open(filedir, GD_RDWR);
  e1 = gd_include(D, "format1", 0, 0);
  CHECKI(e1, GD_E_BAD_REFERENCE);

  e2 = gd_validate(D, "BEFORE");
  CHECKI(e2, GD_E_BAD_CODE);

  e3 = gd_validate(D, "before");
  CHECKI(e3, GD_E_BAD_CODE);

  e4 = gd_validate(D, "afer");
  CHECKI(e4, GD_E_BAD_CODE);

  e5 = gd_validate(D, "remaining");
  CHECKI(e5, 0);

  n = gd_nfields(D);
  CHECKU(n, 2);
  
  gd_discard(D);

  unlink(format1);
  unlink(format);
  rmdir(filedir);

  return r;
}
