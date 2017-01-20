/* Copyright (C) 2010-2011, 2013, 2017 D.V. Wiebe
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
  double c[8];
#ifdef GD_NO_C99_API
  double v[2] = {1093, 3290};
#else
  double complex v = 1093 + _Complex_I * 3290;
#endif
  int i, ret, error, n, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "data RAW INT32 8\nphase PHASE data 1\n"
    "div RECIP data 230.\n"
  );
  MAKEDATAFILE(data, int32_t, i, 256);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  ret = gd_alter_crecip(D, "div", "phase", v);
  error = gd_error(D);
  n = gd_getdata(D, "div", 5, 0, 1, 0, GD_FLOAT64, c);

  gd_discard(D);

  for (i = 0; i < 8; ++i)
    CHECKFi(i,c[i], creal(v) / (i + 41.));

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error,0);
  CHECKI(n,8);
  CHECKI(ret,0);

  return r;
}
