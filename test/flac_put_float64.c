/* Copyright (C) 2015, 2016, 2017 D.V. Wiebe
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
#if !defined USE_FLAC || !defined TEST_FLAC
  return 77;
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data.flac";
  double c[8];
  double d[13];
  off_t nf;
  int r = 0, i, e1, e2, e3;
  size_t n1, n2;
  DIRFILE *D;

  memset(c, 0, 8);
  rmdirfile();
  mkdir(filedir, 0700);

  for (i = 0; i < 8; ++i)
    c[i] = 1.234 * i;

  D = gd_open(filedir, GD_RDWR | GD_FLAC_ENCODED | GD_LITTLE_ENDIAN
      | GD_VERBOSE | GD_CREAT | GD_EXCL);
  gd_add_spec(D, "data RAW FLOAT64 1", 0);
  n1 = gd_putdata(D, "data", 5, 0, 8, 0, GD_FLOAT64, c);
  CHECKI(n1, 8);

  e1 = gd_error(D);
  CHECKI(e1, GD_E_OK);

  e2 = gd_close(D);
  CHECKI(e2, 0);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  nf = gd_nframes(D);
  CHECKU(nf, 13);

  n2 = gd_getdata(D, "data", 0, 0, 0, 13, GD_FLOAT64, d);
  CHECKI(n2, 13);

  e3 = gd_error(D);
  CHECKI(e3, GD_E_OK);

  for (i = 0; i < 13; ++i) {
    if (i < 5)
      CHECKFi(i, d[i], 0);
    else
      CHECKFi(i, d[i], 1.234 * (i - 5));
  }

  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  return r;
#endif
}
