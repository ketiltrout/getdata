/* Copyright (C) 2012, 2014 D. V. Wiebe
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
  const char *format1 = "dirfile/format";
  const char *data = "dirfile/data";
  const char *target1, *target2;
  int e1, e2, e3, e4, e5, i, r = 0;
  unsigned int n;
  DIRFILE *D;

  rmdirfile();

  D = gd_open(filedir, GD_RDWR | GD_CREAT);

  gd_include_affix(D, "format1", 0, "A", "Z", GD_CREAT);
  gd_add_raw(D, "AdataZ", GD_UINT8, 1, 1);
  gd_madd_alias(D, "data", "AaliasZ", "AdataZ");
  e1 = gd_error(D);
  gd_madd_alias(D, "AdataZ", "alias", "AdataZ");
  e2 = gd_error(D);
  gd_madd_alias(D, "AdataZ", "AaliasZ", "data");
  e3 = gd_error(D);
  gd_madd_alias(D, "AdataZ", "AaliasZ", "AdataZ");
  e4 = gd_error(D);
  gd_add_alias(D, "AdataZ/alias2", "AdataZ", 1);
  e5 = gd_error(D);

  /* check */
  target1 = gd_alias_target(D, "AdataZ/AaliasZ");
  CHECKS(target1, "AdataZ");
  target2 = gd_alias_target(D, "AdataZ/alias2");
  CHECKS(target2, "AdataZ");

  i = gd_fragment_index(D, "AdataZ/AaliasZ");
  n = gd_naliases(D, "AdataZ/AaliasZ");

  gd_discard(D);

  unlink(data);
  unlink(format);
  unlink(format1);
  rmdir(filedir);

  CHECKI(e1, GD_E_BAD_CODE);
  CHECKI(e2, GD_E_OK);
  CHECKI(e3, GD_E_BAD_CODE);
  CHECKI(e4, GD_E_OK);
  CHECKI(e5, GD_E_OK);
  CHECKI(i, 1);
  CHECKU(n, 4);

  return r;
}
