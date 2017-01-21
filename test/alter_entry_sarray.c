/* Copyright (C) 2008-2011, 2013, 2017 D.V. Wiebe
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
/* Test field modifying */
#include "test.h"

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  int i, n, error, r = 0;
  const char *d[6];
  const char *v[6] = {"a", "b", "c", "d", "", ""};
  gd_entry_t E;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "sarray SARRAY a b c d\n");

  E.flags = 0;
  E.field_type = GD_SARRAY_ENTRY;
  E.EN(scalar,array_len) = 6;

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  n = gd_alter_entry(D, "sarray", &E, 0);
  error = gd_error(D);

  CHECKI(n, 0);
  CHECKI(error, 0);

  gd_get_sarray(D, "sarray", d);
  for (i = 0; i < 6; ++i) {
    CHECKPNi(i, d[i]);
    CHECKSi(i, d[i], v[i]);
  }

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
