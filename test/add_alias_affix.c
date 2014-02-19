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
  const char *target;
  int e1, e2, e3, i, r = 0;
  unsigned int n;
  DIRFILE *D;

  rmdirfile();

  D = gd_open(filedir, GD_RDWR | GD_CREAT);

  gd_include_affix(D, "format1", 0, "A", "Z", GD_CREAT);
  gd_add_raw(D, "AdataZ", GD_UINT8, 1, 1);
  gd_add_alias(D, "alias", "AdataZ", 1);
  e1 = gd_error(D);
  gd_add_alias(D, "AaliasZ", "data", 1);
  e2 = gd_error(D);
  gd_add_alias(D, "AaliasZ", "AdataZ", 1);
  e3 = gd_error(D);

  /* check */
  target = gd_alias_target(D, "AaliasZ");
  CHECKS(target, "AdataZ");
  i = gd_fragment_index(D, "AaliasZ");
  n = gd_naliases(D, "AaliasZ");

  gd_discard(D);

  unlink(data);
  unlink(format);
  unlink(format1);
  rmdir(filedir);

  CHECKI(e1, GD_E_BAD_CODE);
  CHECKI(e2, GD_E_BAD_CODE);
  CHECKI(e3, GD_E_OK);
  CHECKI(i, 1);
  CHECKU(n, 2);

  return r;
}
