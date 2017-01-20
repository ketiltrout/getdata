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
/* Attempt to read COMPLEX64 as INT64 */
#include "test.h"

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  int64_t c = 0;
#ifdef GD_NO_C99_API
  float d[] = {8, 0};
#else
  float complex d = 8;
#endif
  int n1, n2, error, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "data CONST INT64 0\n");

  D = gd_open(filedir, GD_RDWR);
#ifdef GD_NO_C99_API
  n1 = gd_put_constant(D, "data", GD_COMPLEX64, d);
#else
  n1 = gd_put_constant(D, "data", GD_COMPLEX64, &d);
#endif
  n2 = gd_get_constant(D, "data", GD_INT64, &c);
  error = gd_error(D);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(n1, 0);
  CHECKI(n2, 0);
  CHECKI(c, 8);

  return r;
}
