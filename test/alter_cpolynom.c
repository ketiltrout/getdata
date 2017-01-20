/* Copyright (C) 2009-2011, 2013, 2017 D.V. Wiebe
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
  const char *data = "dirfile/data";
#ifdef GD_NO_C99_API
  double c[16];
  const double a[] = {2, 1, 1, 2, 1, 3};
#else
  double complex c[8];
  const double complex a[] = {2 + _Complex_I * 1,
    1 + _Complex_I * 2, 1 + _Complex_I * 3};
#endif
  int i, ret, error, n, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "data RAW INT32 8\n"
    "polynom POLYNOM data 1 2 1\n"
  );
  MAKEDATAFILE(data, int32_t, i, 256);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  ret = gd_alter_cpolynom(D, "polynom", 0, NULL, a);
  error = gd_error(D);
  n = gd_getdata(D, "polynom", 5, 0, 1, 0, GD_COMPLEX128, c);

  gd_discard(D);

  for (i = 0; i < 8; ++i) {
    int x = i + 40;
#ifdef GD_NO_C99_API
    const double v[2] = {2 + x + x * x, 1 + 2 * x + 3 * x * x};
    CHECKCi(i,c + 2 * i, v);
#else
    const double complex v = (2 + _Complex_I * 1) + (1 + _Complex_I * 2) * x
      + (1 + _Complex_I * 3) * x * x;
    CHECKCi(i,c[i], v);
#endif
  }

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(n, 8);
  CHECKI(ret, 0);

  return r;
}
