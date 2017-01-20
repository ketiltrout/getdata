/* Copyright (C) 2013, 2017 D.V. Wiebe
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
  const char *data = "dirfile/data";
  int ret, error, r = 0;
  DIRFILE *D;
  gd_entry_t E;
#ifdef GD_NO_C99_API
  const double m[4] = {1, 2, 3, 4};
  const double b[4] = {8, 7, 6, 5};
#else
  const double complex m[2] = {1 + _Complex_I * 2, 3 + _Complex_I * 4};
  const double complex b[2] = {8 + _Complex_I * 7, 6 + _Complex_I * 5};
#endif

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "data RAW INT32 8\nphase PHASE data 1\n"
    "lincom LINCOM 2 data 1 0 data 1 0\n"
  );
  MAKEDATAFILE(data, int32_t, i, 256);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  ret = gd_alter_clincom(D, "lincom", 0, NULL, m, b);
  error = gd_error(D);
  gd_entry(D, "lincom", &E);

  CHECKX(E.flags, GD_EN_COMPSCAL | GD_EN_CALC);
  CHECKI(E.EN(lincom,n_fields), 2);
#ifdef GD_NO_C99_API
  CHECKC(E.EN(lincom,cm)[0], m);
  CHECKC(E.EN(lincom,cm)[1], m + 2);
  CHECKC(E.EN(lincom,cb)[0], b);
  CHECKC(E.EN(lincom,cb)[1], b + 2);
#else
  CHECKC(E.EN(lincom,cm)[0], m[0]);
  CHECKC(E.EN(lincom,cm)[1], m[1]);
  CHECKC(E.EN(lincom,cb)[0], b[0]);
  CHECKC(E.EN(lincom,cb)[1], b[1]);
#endif
  gd_free_entry_strings(&E);

  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(ret, 0);

  return r;
}
