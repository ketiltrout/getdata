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
  const char *format1 = "dirfile/format1";
  int e1, e2, e3, e4, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "/INCLUDE format1 NS.");
  MAKEFORMATFILE(format1, "ns2.data CONST UINT8 1");

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  e1 = gd_validate(D, "NS.ns2.data");
  CHECKI(e1, 0);

  e2 = gd_rewrite_fragment(D, GD_ALL_FRAGMENTS);
  CHECKI(e2, GD_E_OK);

  e3 = gd_close(D);
  CHECKI(e3, 0);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  e4 = gd_validate(D, "NS.ns2.data");
  CHECKI(e4, 0);

  gd_discard(D);

  unlink(format1);
  unlink(format);
  rmdir(filedir);

  return r;
}
