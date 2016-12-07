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
  int e1, e2, e3, r = 0;
  DIRFILE *D;

  rmdirfile();

  D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_EXCL | GD_PRETTY_PRINT |
      GD_VERBOSE);
  gd_add_spec(D, "s1 CONST UINT8 1", 0);
  gd_add_spec(D, "s2 CONST UINT8 1", 0);
  gd_add_spec(D, "s3 CONST UINT8 1", 0);
  gd_add_spec(D,
      "loooooooooooooooooooooooooooooooooooooooooooong CONST UINT8 1", 0);

  e1 = gd_metaflush(D);
  CHECKI(e1, GD_E_OK);

  e2 = gd_close(D);
  CHECKI(e2, 0);

  D = gd_open(filedir, GD_RDONLY);
  e3 = gd_validate(D, "loooooooooooooooooooooooooooooooooooooooooooong");
  CHECKI(e3, 0);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
