/* Copyright (C) 2008-2013, 2017 D.V. Wiebe
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
  int ret, e1, e2, e3, e4, e5, e6, r = 0;
  DIRFILE *D;
  const char *s1, *s2;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "data CONST UINT8 8\n"
    "data/meta1 CONST UINT8 8\n"
    "data/meta2 CONST UINT8 8\n"
    "/ALIAS data/meta3 data\n"
    "/ALIAS data/meta4 data/meta2\n"
  );

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  ret = gd_rename(D, "data", "zata", 0);
  e1 = gd_error(D);
  CHECKI(ret,0);
  CHECKI(e1,0);

  gd_get_constant(D, "zata", GD_NULL, NULL);
  e2 = gd_error(D);
  CHECKI(e2,0);

  gd_get_constant(D, "zata/meta1", GD_NULL, NULL);
  e3 = gd_error(D);
  CHECKI(e3,0);

  gd_get_constant(D, "zata/meta2", GD_NULL, NULL);
  e4 = gd_error(D);
  CHECKI(e4,0);

  gd_get_constant(D, "zata/meta3", GD_NULL, NULL);
  e5 = gd_error(D);
  s1 = gd_alias_target(D, "zata/meta3");
  CHECKI(e5,0);
  CHECKS(s1, "zata");

  gd_get_constant(D, "zata/meta4", GD_NULL, NULL);
  e6 = gd_error(D);
  s2 = gd_alias_target(D, "zata/meta4");
  CHECKI(e6,0);
  CHECKS(s2, "zata/meta2");

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
