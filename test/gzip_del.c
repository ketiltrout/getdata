/* Copyright (C) 2013 D. V. Wiebe
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

#include <inttypes.h>
#include <stdlib.h>
#include <sys/types.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>

/* this tests discarding OOP-writable (also temporary) RAW files */
int main(void)
{
#if !(defined TEST_GZIP)
  return 77;
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data.gz";
  const char *format_data = "data RAW UINT8 8\n";
  uint8_t c[8];
  int fd, i, n, error1, error2, stat_data, unlink_data, ret, r = 0;
  int rmdir_filedir;
  DIRFILE *D;

  memset(c, 0, 8);
  rmdirfile();
  mkdir(filedir, 0777);

  for (i = 0; i < 8; ++i)
    c[i] = (uint8_t)(40 + i);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_GZIP_ENCODED | GD_VERBOSE);
  n = gd_putdata(D, "data", 5, 0, 1, 0, GD_UINT8, c);
  error1 = gd_error(D);
  ret = gd_delete(D, "data", GD_DEL_DATA);
  error2 = gd_error(D);

  gd_discard(D);

  unlink_data = unlink(data);
  unlink(format);
  rmdir_filedir = rmdir(filedir);

  CHECKI(unlink_data, -1);
  CHECKI(rmdir_filedir, 0);
  CHECKI(error1, GD_E_OK);
  CHECKI(error2, GD_E_OK);
  CHECKI(n, 8);
  CHECKI(ret, 0);

  return r;
#endif
}
