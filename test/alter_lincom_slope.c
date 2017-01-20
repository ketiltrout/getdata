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
/* Test field modifying */
#include "test.h"

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  int32_t c[8];
  int i, ret, error, n, r = 0;
  DIRFILE *D;
  const char *in_fields[3] = {"data", "phase", NULL};
  double m[3] = {2, 3, 0};

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "data RAW INT32 8\nphase PHASE data 1\n"
    "lincom LINCOM 2 data 1 0 data 1 0\n"
  );
  MAKEDATAFILE(data, int32_t, i, 256);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  ret = gd_alter_lincom(D, "lincom", 0, in_fields, m, NULL);
  error = gd_error(D);
  n = gd_getdata(D, "lincom", 5, 0, 1, 0, GD_INT32, c);

  gd_discard(D);

  for (i = 0; i < 8; ++i)
    CHECKIi(i,c[i], i * 5 + 203);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(n, 8);
  CHECKI(ret, 0);

  return r;
}
