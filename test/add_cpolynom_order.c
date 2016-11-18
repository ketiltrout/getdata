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
  int r = 0;
  int error;
  DIRFILE *D;
#ifdef GD_NO_C99_API
  const double a[12] = {1, 29.03, 0.3, 12.34, 0.5, 99.55, 1.8, 45.32,
    98.2, -17.3, 0.01, 82.11};
#else
  const double complex a[6] = {1 + _Complex_I * 29.03, 0.3 + _Complex_I * 12.34,
    0.5 + _Complex_I * 99.55, 1.8 + _Complex_I * 45.32,
    98.2 - _Complex_I * 17.3, 0.01 + _Complex_I * 82.11};
#endif

  rmdirfile();
  D = gd_open(filedir, GD_RDWR | GD_CREAT);
  gd_add_cpolynom(D, "new", -5, "in", a, 0);
  error = gd_error(D);
  CHECKI(error, GD_E_BAD_ENTRY);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
