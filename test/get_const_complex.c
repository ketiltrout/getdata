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
/* Attempt to read complex constant */
#include "test.h"

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
#ifdef GD_NO_C99_API
  double c[2];
  const double v[2] = {8.3, 9.2};
#else
  double complex c;
  const double complex v = 8.3 + _Complex_I * 9.2;
#endif
  int n, error, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "const CONST COMPLEX128 8.3;9.2\n");

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
#ifdef GD_NO_C99_API
  n = gd_get_constant(D, "const", GD_COMPLEX128, c);
#else
  n = gd_get_constant(D, "const", GD_COMPLEX128, &c);
#endif
  error = gd_error(D);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(n, 0);
  CHECKC(c, v);

  return r;
}
