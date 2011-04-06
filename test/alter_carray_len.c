/* Copyright (C) 2010-2011 D. V. Wiebe
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

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <inttypes.h>
#include <errno.h>
#include <stdio.h>

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *format_data = "carray CARRAY FLOAT32 8.3 7.2 6.1 5.0 3.9 2.8 1.7\n";
  int fd, i, ret, error, n, r = 0;
  size_t z;
  DIRFILE *D;
  double d[5];

  rmdirfile();
  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  ret = gd_alter_carray(D, "carray", GD_NULL, 5);
  error = gd_error(D);
  z = gd_carray_len(D, "carray");
  n = gd_get_carray(D, "carray", GD_FLOAT64, &d);

  gd_close(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(n, 0);
  CHECKI(ret, 0);
  CHECKU(z, 5);
  for (i = 0; i < 5; ++i)
    CHECKFi(i, d[i], 8.3 - i * 1.1);

  return r;
}
