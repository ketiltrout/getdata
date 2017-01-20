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
  double c[16];
  double data_data[512];
  int i, fd, n, error, r = 0;
  DIRFILE *D;

  memset(c, 0, sizeof(double) * 16);
  rmdirfile();
  mkdir(filedir, 0700);

  for (fd = 0; fd < 256; ++fd) {
    data_data[2 * fd] = (double)fd;
    data_data[2 * fd + 1] = fd / 256.;
  }

  MAKEFORMATFILE(format,
    "polynom POLYNOM data 3 2 1\n"
    "data RAW COMPLEX128 1\n"
  );

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 512 * sizeof(double));
  close(fd);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n = gd_getdata(D, "polynom", 5, 0, 8, 0, GD_COMPLEX128, &c);
  error = gd_error(D);

  CHECKI(error, 0);
  CHECKI(n, 8);
  for (i = 0; i < 8; ++i) {
    const double a = 5. + i; /* Re(data) */
    const double b = a / 256.; /* Im(data) */
    CHECKFi(i, c[i * 2], 3 + 2 * a + a * a - b * b);
    CHECKFi(i, c[i * 2 + 1], 2 * b + 2 * a * b);
  }

  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  return r;
}
