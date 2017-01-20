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
  int e, r = 0;
  DIRFILE *D;
  const char **al;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "/ALIAS e f\n"
    "/ALIAS a b\n"
    "/ALIAS b d\n"
    "/ALIAS c d\n"
    "/ALIAS d f\n"
  );

  D = gd_open(filedir, GD_RDONLY);

  al = gd_aliases(D, "a");
  e = gd_error(D);

  CHECKI(e, GD_E_BAD_CODE);
  CHECKP(al);

  gd_discard(D);
  unlink(format);
  rmdir(filedir);

  return r;
}
