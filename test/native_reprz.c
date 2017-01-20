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
  int e1, e2, e3, e4, r = 0;
  DIRFILE *D;
  gd_type_t t1, t2, t3, t4;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
      "code RAW UINT64 1\n"
      "code.r RAW COMPLEX128 1\n"
      "more.r RAW COMPLEX64 1\n"
      );

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);

  /* Real part of "code" */
  t1 = gd_native_type(D, "code.r");
  e1 = gd_error(D);
  CHECKU(t1, GD_UINT64);
  CHECKI(e1, 0);

  /* Native type of code.r */
  t2 = gd_native_type(D, "code.r.z");
  e2 = gd_error(D);
  CHECKU(t2, GD_COMPLEX128);
  CHECKI(e2, 0);

  /* Real part of code.r */
  t3 = gd_native_type(D, "code.r.r");
  e3 = gd_error(D);
  CHECKU(t3, GD_FLOAT64);
  CHECKI(e3, 0);

  /* Native type of more.r */
  t4 = gd_native_type(D, "more.r");
  e4 = gd_error(D);
  CHECKU(t4, GD_COMPLEX64);
  CHECKI(e4, 0);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
