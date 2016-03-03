/* Copyright (C) 2014, 2016 D. V. Wiebe
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
  int e1, e2, i = 0, r = 0;
  DIRFILE *D;
  gd_entry_t E;

  /* For no good, but long standing, reason, these are all valid field names,
   * I guess... */
  char *code[] = {"0", "0x1", "02", "3.00", "4e0", "0x5p0"};

  memset(&E, 0, sizeof(E));
  E.field = "pathological";
  E.field_type = GD_POLYNOM_ENTRY;
  E.EN(polynom,poly_ord) = 5;
  E.in_fields[0] = "INDEX";

  E.scalar[0] = code[0];
  E.scalar[1] = code[1];
  E.scalar[2] = code[2];
  E.scalar[3] = code[3];
  E.scalar[4] = code[4];
  E.scalar[5] = code[5];

  E.scalar_ind[0] = -1;
  E.scalar_ind[1] = -1;
  E.scalar_ind[2] = -1;
  E.scalar_ind[3] = -1;
  E.scalar_ind[4] = -1;
  E.scalar_ind[5] = -1;

  rmdirfile();
  D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_TRUNC | GD_VERBOSE);

  gd_add(D, &E);
  e1 = gd_error(D);
  CHECKI(e1, 0);

  gd_close(D);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  gd_entry(D, "pathological", &E);
  e2 = gd_error(D);
  CHECKI(e2, 0);

  for (i = 0; i < 6; ++i)
    CHECKSi(i, E.scalar[i], code[i]);
  gd_free_entry_strings(&E);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
