/* Copyright (C) 2013, 2017 D.V. Wiebe
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
  const char *format1 = "dirfile/format1";
  int ret, p0, p1, error, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "data RAW UINT8 8\n"
    "INCLUDE format1\n"
    "PROTECT format\n"
  );
  MAKEFORMATFILE(format1, "PROTECT none\n");

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  ret = gd_alter_protection(D, GD_PROTECT_DATA, GD_ALL_FRAGMENTS);
  error = gd_error(D);
  CHECKI(error, 0);
  CHECKI(ret, 0);

  p0 = gd_protection(D, 0);
  CHECKI(p0, GD_PROTECT_DATA);

  p1 = gd_protection(D, 1);
  CHECKI(p1, GD_PROTECT_DATA);

  gd_discard(D);

  unlink(format1);
  unlink(format);
  rmdir(filedir);

  return r;
}
