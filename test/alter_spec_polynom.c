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
  int i, ret, error, r = 0;
  DIRFILE *D;
  gd_entry_t E;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "polynom POLYNOM a 3 5 7 9\n");

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  ret = gd_alter_spec(D, "polynom POLYNOM b 1 2 3\n", 0);
  error = gd_error(D);

  gd_entry(D, "polynom", &E);

  CHECKI(E.field_type, GD_POLYNOM_ENTRY);
  CHECKS(E.in_fields[0], "b");
  CHECKI(E.EN(polynom,poly_ord), 2);
  for (i = 0; i < 3; ++i)
    CHECKFi(i, E.EN(polynom,a)[i], i + 1);
 
  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error,0);
  CHECKI(ret, 0);
  gd_free_entry_strings(&E);

  return r;
}
