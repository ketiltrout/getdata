/* Copyright (C) 2011, 2013, 2014, 2017 D.V. Wiebe
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
  int e, r = 0;
  unsigned int n;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "/ALIAS e f\n"
    "/ALIAS a b\n"
    "/ALIAS b d\n"
    "/ALIAS c d\n"
    "/ALIAS d f\n"
    "f CONST UINT8 1\n"
  );

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);

  n = gd_naliases(D, "a");
  e = gd_error(D);

  CHECKI(e, GD_E_OK);
  CHECKU(n, 6);

  gd_discard(D);
  unlink(format);
  rmdir(filedir);

  return r;
}
