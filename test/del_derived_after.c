/* Copyright (C) 2011, 2013, 2017 D.V. Wiebe
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
  int ret, v, e1, e2, r = 0;
  size_t n;
  double c;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "data RAW UINT8 8\nlincom LINCOM 1 data 1 0\n");

  D = gd_open(filedir, GD_RDWR);
  /* in order to initialise the lincom's entry cache */
  v = gd_validate(D, "lincom");
  ret = gd_delete(D, "data", GD_DEL_FORCE);
  e1 = gd_error(D);
  n = gd_getdata(D, "lincom", 0, 0, 0, 1, GD_FLOAT64, &c);
  e2 = gd_error(D);
  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(e1, GD_E_OK);
  CHECKI(e2, GD_E_BAD_CODE);
  CHECKI(ret, 0);
  CHECKI(n, 0);
  CHECKI(v, 0);

  return r;
}
