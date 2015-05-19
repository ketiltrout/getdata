/* Copyright (C) 2014 D. V. Wiebe
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
  DIRFILE *D;

  rmdirfile();

  D = gd_open(filedir, GD_RDWR | GD_CREAT);
  gd_add_alias(D, "name", "target", 0);
  e1 = gd_error(D);
  CHECKI(e1, GD_E_OK);

  gd_add_raw(D, "name", GD_UINT8, 1, 0);
  e2 = gd_error(D);
  CHECKI(e2, GD_E_DUPLICATE);

  gd_add_alias(D, "name", "target", 0);
  e3 = gd_error(D);
  CHECKI(e3, GD_E_DUPLICATE);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
