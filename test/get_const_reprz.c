/* Copyright (C) 2016, 2017 D.V. Wiebe
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
  int e1, e2, e3, e4, r1, r2, r3, r4, r = 0;
  double c1[2], c2[2], c3[2], c4[2];
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
      "code CONST UINT64 1\n"
      "code.r CONST COMPLEX128 2;3\n"
      "more.r CONST COMPLEX64 4;5\n"
      );

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);

  /* Real part of "code" */
  r1 = gd_get_constant(D, "code.r", GD_COMPLEX128, c1);
  e1 = gd_error(D);
  CHECKU(r1, 0);
  CHECKI(e1, 0);
  CHECKF(c1[0], 1);
  CHECKF(c1[1], 0);

  /* All of code.r */
  r2 = gd_get_constant(D, "code.r.z", GD_COMPLEX128, c2);
  e2 = gd_error(D);
  CHECKU(r2, 0);
  CHECKI(e2, 0);
  CHECKF(c2[0], 2);
  CHECKF(c2[1], 3);

  /* Real part of code.r */
  r3 = gd_get_constant(D, "code.r.r", GD_COMPLEX128, c3);
  e3 = gd_error(D);
  CHECKU(r3, 0);
  CHECKI(e3, 0);
  CHECKF(c3[0], 2);
  CHECKF(c3[1], 0);

  /* All of more.r */
  r4 = gd_get_constant(D, "more.r", GD_COMPLEX128, c4);
  e4 = gd_error(D);
  CHECKU(r4, 0);
  CHECKI(e4, 0);
  CHECKF(c4[0], 4);
  CHECKF(c4[1], 5);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
