/* Copyright (C) 2009-2011 D. V. Wiebe
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
/* Attempt to read argument representation */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <math.h>

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  const char *format_data = "data RAW COMPLEX128 1\n";
  double c[8];
#ifdef GD_NO_C99_API
  double data_data[100][2];
#else
  double complex data_data[100];
#endif
  int i, n, error, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0777);

  for (i = 0; i < 100; ++i) {
#ifdef GD_NO_C99_API
    const double v = i * 3.14159265358979323846 / 5.;
    data_data[i][0] = cos(v);
    data_data[i][1] = sin(v);
#else
    data_data[i] = cexp(_Complex_I * i * 3.14159265358979323846 / 5.);
#endif
  }

  i = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(i, format_data, strlen(format_data));
  close(i);

  i = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(i, data_data, 200 * sizeof(double));
  close(i);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n = gd_getdata(D, "data.i", 5, 0, 8, 0, GD_FLOAT64, &c);
  error = gd_error(D);

  gd_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(n,8);
  CHECKI(error,0);
  for (i = 0; i < 8; ++i)
    CHECKFi(i,c[i],cimag(data_data[5 + i]));

  return r;
}
