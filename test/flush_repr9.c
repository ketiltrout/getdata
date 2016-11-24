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
  int e1, e2, e3, e4, e5, e6, r = 0;
  DIRFILE *D;
  gd_entry_t E1, E2;

  rmdirfile();

  D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_EXCL | GD_VERBOSE);
  e1 = gd_add_spec(D, "i LINCOM r.r 1 0 q/r 1 0 r/q 1 0", 0);
  CHECKI(e1, 0);
  e2 = gd_add_spec(D, "z LINCOM m.z 1 0 q/m.z 1 0 m/q.z 1 0", 0);
  CHECKI(e2, 0);

  e3 = gd_dirfile_standards(D, 9);
  CHECKI(e3, 9);

  e4 = gd_close(D);
  CHECKI(e4, 0);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);

  e5 = gd_entry(D, "i", &E1);
  if (e5)
    CHECKI(e5, 0);
  else {
    CHECKS(E1.in_fields[0], "r.r");
    CHECKS(E1.in_fields[1], "q/r");
    CHECKS(E1.in_fields[2], "r/q");
    gd_free_entry_strings(&E1);
  }

  e6 = gd_entry(D, "z", &E2);
  if (e6)
    CHECKI(e6, 0);
  else {
    CHECKS(E2.in_fields[0], "m.z");
    CHECKS(E2.in_fields[1], "q/m.z");
    CHECKS(E2.in_fields[2], "m/q.z");
    gd_free_entry_strings(&E2);
  }

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
