/* Copyright (C) 2017 D.V. Wiebe
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
  int r = 0;
  long no0, no1, no2, no3, no4;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "#\n");

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);

  no0 = gd_open_limit(D, GD_OLIMIT_CURRENT);
  CHECKI(no0, 0);
  no1 = gd_open_limit(D, 1000);
  CHECKI(no1, 1000);
  no2 = gd_open_limit(D, GD_OLIMIT_CURRENT);
  CHECKI(no2, 1000);
  no3 = gd_open_limit(D, GD_OLIMIT_NONE);
  CHECKI(no3, 0);
  no4 = gd_open_limit(D, GD_OLIMIT_CURRENT);
  CHECKI(no4, 0);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
