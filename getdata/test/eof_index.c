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
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  const char *format_data =
    "data RAW UINT16 1\n"
    "mult1 MULTIPLY data INDEX\n"
    "mult2 MULTIPLY INDEX INDEX\n"
    "mult3 MULTIPLY INDEX data\n";
  int fd, error0, error1, error2, error3, r = 0;
  const size_t len = strlen(data);
  off_t eof_INDEX, eof_mult1, eof_mult2, eof_mult3;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data, len);
  close(fd);

  D = gd_open(filedir, GD_RDONLY);
  eof_INDEX = gd_eof(D, "INDEX");
  error0 = gd_error(D);
  eof_mult1 = gd_eof(D, "mult1");
  error1 = gd_error(D);
  eof_mult2 = gd_eof(D, "mult2");
  error2 = gd_error(D);
  eof_mult3 = gd_eof(D, "mult3");
  error3 = gd_error(D);
  gd_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error0, GD_E_BAD_FIELD_TYPE);
  CHECKI(eof_INDEX, -1);
  CHECKI(error1, GD_E_OK);
  CHECKI(eof_mult1, (int)len / 2);
  CHECKI(error2, GD_E_BAD_FIELD_TYPE);
  CHECKI(eof_mult2, -1);
  CHECKI(error3, GD_E_OK);
  CHECKI(eof_mult3, (int)len / 2);

  return r;
}
