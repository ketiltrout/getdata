/* Copyright (C) 2013 D. V. Wiebe
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
  int error, r = 0;
  DIRFILE *D;

  gd_entry_t E, e;
  memset(&E, 0, sizeof(E));
  E.field = "data";
  E.field_type = GD_POLYNOM_ENTRY;
  E.fragment_index = 0;
  E.EN(polynom,poly_ord) = 3;
  E.in_fields[0] = "INDEX";
  E.scalar[0] = "A";
  E.scalar[1] = "B";
  E.scalar[2] = "C";
  E.scalar[3] = "D";

  rmdirfile();
  D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  gd_add_spec(D, "A CONST INT64 1", 0);
  gd_add_spec(D, "B CONST INT64 2", 0);
  gd_add_spec(D, "C CONST INT64 3", 0);
  gd_add_spec(D, "D CONST INT64 4", 0);
  gd_add(D, &E);
  error = gd_error(D);

  /* check */
  gd_entry(D, "data", &e);
  if (gd_error(D))
    r = 1;
  else {
    CHECKI(e.field_type, GD_POLYNOM_ENTRY);
    CHECKI(e.fragment_index, 0);
    CHECKI(e.EN(polynom,poly_ord), 3);
    CHECKF(e.EN(polynom,a)[0], 1);
    CHECKF(e.EN(polynom,a)[1], 2);
    CHECKF(e.EN(polynom,a)[2], 3);
    CHECKF(e.EN(polynom,a)[3], 4);
    CHECKS(e.scalar[0], "A");
    CHECKS(e.scalar[1], "B");
    CHECKS(e.scalar[2], "C");
    CHECKS(e.scalar[3], "D");
    gd_free_entry_strings(&e);
  }

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error, GD_E_OK);

  return r;
}
