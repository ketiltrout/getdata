/* Copyright (C) 2008-2011 D. V. Wiebe
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
  const char *table = "dirfile/table";
  const char *table1 = "dirfile/table1";
  const char *data = "dirfile/data";
  const char *format_data = "data RAW INT32 8\nlut LINTERP data table\n";
  const char *tabledata = "0 0\n1000 5000\n";
  const char *table1data = "0 0\n1000 10000\n";
  int32_t data_data[256];
  int32_t c[8];
  int fd, i, ret, error, n, unlink_table, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (int32_t)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(table, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, tabledata, strlen(tabledata));
  close(fd);

  fd = open(table1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, table1data, strlen(table1data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 256 * sizeof(int32_t));
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  ret = gd_alter_linterp(D, "lut", NULL, "table1", 1);
  error = gd_error(D);
  n = gd_getdata(D, "lut", 5, 0, 1, 0, GD_INT32, c);

  gd_close(D);

  for (i = 0; i < 8; ++i)
    CHECKIi(i,c[i], (i + 40) * 5);

  unlink(data);
  unlink_table = unlink(table);
  unlink(table1);
  unlink(format);
  rmdir(filedir);

  CHECKI(error,0);
  CHECKI(n,8);
  CHECKI(ret,0);
  CHECKI(unlink_table,-1);

  return r;
}
