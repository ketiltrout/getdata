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
/* Frameindex look-up */
#include "test.h"

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <math.h>
#include <errno.h>

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  const char *format_data = "data RAW FLOAT64 1\n";
  double d[1000], f1, f2, f3;
  int i, error, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0777);

  for (i = 0; i < 1000; ++i)
    d[i] = sqrt((i+600) / 500.);

  i = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(i, format_data, strlen(format_data));
  close(i);

  i = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(i, d, 1000 * sizeof(double));
  close(i);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  f1 = gd_framenum(D, "data", 1.09);
  f2 = gd_framenum(D, "data", 1.49);
  f3 = gd_framenum(D, "data", 1.79);
  error = gd_error(D);

  gd_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKF(f1, -5.96730894763915);
  CHECKF(f2, 510.050010695549);
  CHECKF(f3, 1002.04807025292);

  return r;
}
