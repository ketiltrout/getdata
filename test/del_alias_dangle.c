/* Copyright (C) 2016, 2017 D.V. Wiebe
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
  int e1, v1, v2, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
      "data RAW UINT8 1\n"
      "/ALIAS alias data\n"
      );

  D = gd_open(filedir, GD_RDWR);
  v1 = gd_validate(D, "alias");
  CHECKI(v1, 0);

  e1 = gd_delete(D, "data", GD_DEL_FORCE);
  CHECKI(e1, 0);

  v2 = gd_validate(D, "alias");
  CHECKI(v2, GD_E_BAD_CODE);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
