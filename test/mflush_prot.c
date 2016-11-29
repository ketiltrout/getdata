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
  int e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, r = 0;
  DIRFILE *D;

  rmdirfile();
  D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_EXCL | GD_VERBOSE);
  e1 = gd_alter_protection(D, GD_PROTECT_DATA, 0);
  CHECKI(e1, 0);

  e2 = gd_close(D);
  CHECKI(e2, 0);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  e3 = gd_protection(D, 0);
  CHECKI(e3, GD_PROTECT_DATA);

  e4 = gd_alter_protection(D, GD_PROTECT_ALL, 0);
  CHECKI(e4, 0);

  e5 = gd_close(D);
  CHECKI(e5, 0);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  e6 = gd_protection(D, 0);
  CHECKI(e6, GD_PROTECT_ALL);

  e7 = gd_alter_protection(D, GD_PROTECT_FORMAT, 0);
  CHECKI(e7, 0);

  e8 = gd_close(D);
  CHECKI(e8, 0);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  e9 = gd_protection(D, 0);
  CHECKI(e9, GD_PROTECT_FORMAT);

  e10 = gd_alter_protection(D, GD_PROTECT_NONE, 0);
  CHECKI(e10, 0);

  e11 = gd_close(D);
  CHECKI(e11, 0);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  e12 = gd_protection(D, 0);
  CHECKI(e12, GD_PROTECT_NONE);
  
  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
