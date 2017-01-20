/* Copyright (C) :e2008-2011, 2013, 2017 D.V. Wiebe
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
  int i, r = 0;
  int n[7], e[7];
  const int v[7] = {
             0 | GD_PRETTY_PRINT,
    GD_VERBOSE | GD_PRETTY_PRINT,
    GD_VERBOSE | 0,
             0 | GD_PRETTY_PRINT,
    GD_VERBOSE | 0,
             0 | GD_PRETTY_PRINT,
    GD_VERBOSE | 0
  };
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEEMPTYFILE(format, 0600);

  D = gd_open(filedir, GD_RDONLY | GD_PRETTY_PRINT);
  n[0] = gd_flags(D, 0, 0);
  e[0] = gd_error(D);

  n[1] = gd_flags(D, GD_VERBOSE, 0);
  e[1] = gd_error(D);

  n[2] = gd_flags(D, 0, GD_PRETTY_PRINT);
  e[2] = gd_error(D);

  n[3] = gd_flags(D, GD_VERBOSE | GD_PRETTY_PRINT,
      GD_VERBOSE | GD_PRETTY_PRINT);
  e[3] = gd_error(D);

  n[4] = gd_flags(D, GD_VERBOSE, GD_PRETTY_PRINT);
  e[4] = gd_error(D);

  n[5] = gd_flags(D, GD_VERBOSE | GD_PRETTY_PRINT, GD_VERBOSE);
  e[5] = gd_error(D);

  n[6] = gd_flags(D, GD_VERBOSE, GD_VERBOSE | GD_PRETTY_PRINT);
  e[6] = gd_error(D);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  for (i = 0; i < 7; ++i) {
    CHECKIi(i, e[i], 0);
    CHECKXi(i, n[i], v[i]);
  }

  return r;
}
