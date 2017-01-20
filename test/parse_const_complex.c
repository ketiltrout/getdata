/* Copyright (C) 2008-2011, 2013, 2017 D.V. Wiebe
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
  int e1, e2, r = 0;
#ifdef GD_NO_C99_API
  double c[2];
  const double v[2] = {1, 2};
#else
  complex double c;
#endif
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "const CONST COMPLEX128 1;2\n");

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  e1 = gd_error(D);

#ifdef GD_NO_C99_API
  gd_get_constant(D, "const", GD_COMPLEX128, c);
#else
  gd_get_constant(D, "const", GD_COMPLEX128, &c);
#endif
  e2 = gd_error(D);
  
  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(e1,GD_E_OK);
  CHECKI(e2,GD_E_OK);
#ifdef GD_NO_C99_API
  CHECKC(c,v);
#else
  CHECKC(c,1 + _Complex_I * 2);
#endif
  return r;
}
