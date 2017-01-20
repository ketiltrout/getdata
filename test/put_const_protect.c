/* Copyright (C) 2008-2011, 2013, 2016, 2017 D.V. Wiebe
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
  uint8_t d = 3;
  int n, error, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "data CONST UINT8 8\nPROTECT all\n");

  D = gd_open(filedir, GD_RDWR | GD_UNENCODED);
  n = gd_put_constant(D, "data", GD_UINT8, &d);
  error = gd_error(D);

  CHECKI(error, GD_E_PROTECTED);
  CHECKI(n, GD_E_PROTECTED);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);
  return r;
}
