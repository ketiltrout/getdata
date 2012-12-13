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
/* Parser check */
#include "test.h"

#include <inttypes.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *format_data =
    "data1 RAW UINT8 1\n"
    "data2 RAW UINT8 1\n"
    "REFERENCE data2\n"
    ;

  const char *data1 = "dirfile/data1";
  const char *data2 = "dirfile/data2";

  uint8_t data_data[4] = { 0, 1, 2, 3 };
  int fd, error, error2, r = 0;
  DIRFILE *D;
  off_t nf;

  rmdirfile();
  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data1, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 4);
  close(fd);

  fd = open(data2, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 3);
  close(fd);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);

  error = gd_error(D);
  CHECKI(error,0);

  nf = gd_nframes(D);

  error2 = gd_error(D);
  CHECKI(error2,0);
  CHECKI(nf,3);

  gd_close(D);

  unlink(format);
  unlink(data1);
  unlink(data2);
  rmdir(filedir);

  return r;
}
