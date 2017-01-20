/* Copyright (C) 2010-2011, 2013, 2017 D.V. Wiebe
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
  const char *data2 = "dirfile/data2";
  int fd, error, error2, r = 0;
  const size_t len = strlen(data);
  off_t n, m;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "data RAW UINT16 1\n"
    "data2 RAW UINT8 1\n"
    "lincom LINCOM 2 data2 1. 0. data 1. 0.\n"
    "lincom2 LINCOM 2 data 1. 0. data2 1. 0.\n"
  );

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data, len);
  close(fd);

  fd = open(data2, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data, len);
  close(fd);

  D = gd_open(filedir, GD_RDONLY);
  n = gd_eof(D, "lincom");
  error = gd_error(D);
  m = gd_eof(D, "lincom");
  error2 = gd_error(D);
  gd_discard(D);

  unlink(data2);
  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(n, (int)len / 2);
  CHECKI(error2, 0);
  CHECKI(m, (int)len / 2);

  return r;
}
