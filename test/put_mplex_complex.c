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
  const char *count = "dirfile/count";
  double c[16], d[16];
  double data_data[512];
  uint8_t count_data[256];
  int i, n, m, error, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "mplex MPLEX data count 1 3\n"
    "count RAW UINT8 8\n"
    "data RAW COMPLEX128 8\n"
  );

  for (i = 0; i < 256; ++i) {
    data_data[2 * i] = i;
    data_data[2 * i + 1] = i * 2;
  }

  i = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(i, data_data, 512 * sizeof(double));
  close(i);

  for (i = 0; i < 256; ++i)
    count_data[i] = i % 3;

  i = open(count, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(i, count_data, 256);
  close(i);

  for (i = 0; i < 8; ++i) {
    d[2 * i] = i + 3.;
    d[2 * i + 1] = i + 4.;
  }

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  n = gd_putdata(D, "mplex", 5, 0, 1, 0, GD_COMPLEX128, &d);
  error = gd_error(D);
  m = gd_getdata(D, "data", 5, 0, 1, 0, GD_COMPLEX128, &c);

  gd_discard(D);

  unlink(count);
  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(n, 8);
  CHECKI(m, 8);
  for (i = 0; i < 8; ++i) {
    CHECKIi(i, c[2 * i], ((i % 3) == 0) ? 3 + i : 40 + i);
    CHECKIi(i, c[2 * i + 1], ((i % 3) == 0) ? 4 + i : (40 + i) * 2);
  }

  return r;
}
