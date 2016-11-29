/* Copyright (C) 2008-2011, 2013, 2016 D. V. Wiebe
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
  unsigned char c[8];
  int n, error, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "lincom LINCOM 1 lincom 1 0\n");

  D = gd_open(filedir, GD_RDWR | GD_UNENCODED);
  n = gd_putdata(D, "lincom", 0, 0, 0, 1, GD_UINT8, c);
  CHECKI(n,0);
  error = gd_error(D);
  CHECKI(error,GD_E_RECURSE_LEVEL);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
