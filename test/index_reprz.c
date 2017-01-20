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
  const char *code = "dirfile/code";
  int e1, e2, e3, e4, r = 0;
  double c1, c2, c3, c4;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
      "code RAW COMPLEX128 1\n"
      "code.i PHASE code 10\n"
      "more.r PHASE code 20\n"
      );
  MAKEDATAFILE(code, double, i, 256);

  D = gd_open(filedir, GD_RDONLY);

  /* Imaginary part of code */
  c1 = gd_framenum(D, "code.i", 43.5);
  e1 = gd_error(D);
  CHECKI(e1, 0);
  CHECKF(c1, 43.5 / 2 - 0.5);

  /* All of code.i */
  c2 = gd_framenum(D, "code.i.z", 43.5);
  e2 = gd_error(D);
  CHECKI(e2, GD_E_DOMAIN);
  CHECKNAN(c2);

  /* Real part of code.i */
  c3 = gd_framenum(D, "code.i.r", 43.5);
  e3 = gd_error(D);
  CHECKI(e3, 0);
  CHECKF(c3, 43.5 / 2 - 10);

  /* All of more.r */
  c4 = gd_framenum(D, "more.r", 43.5);
  e4 = gd_error(D);
  CHECKI(e4, GD_E_DOMAIN);
  CHECKNAN(c4);

  gd_discard(D);

  unlink(code);
  unlink(format);
  rmdir(filedir);

  return r;
}
