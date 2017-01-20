/* Copyright (C) 2012-2013, 2017 D.V. Wiebe
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
  int n, error, r = 0;
  DIRFILE *D;
  gd_entry_t E;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "a CONST UINT16 1\n"
    "b CARRAY UINT16 2 3 4 5\n"
    "data MPLEX in1 in2 a b<3>\n"
  );

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);

  n = gd_entry(D, "data", &E);
  error = gd_error(D);

  gd_discard(D);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, GD_E_OK);
  CHECKI(n, 0);
  CHECKS(E.field, "data");
  CHECKX(E.field_type, GD_MPLEX_ENTRY);
  CHECKS(E.in_fields[0], "in1");
  CHECKS(E.in_fields[1], "in2");
  CHECKS(E.scalar[0], "a");
  CHECKS(E.scalar[1], "b");
  CHECKI(E.scalar_ind[0], -1);
  CHECKI(E.scalar_ind[1], 3);
  CHECKI(E.EN(mplex,count_val), 1);
  CHECKI(E.EN(mplex,period), 5);
  gd_free_entry_strings(&E);

  return r;
}
