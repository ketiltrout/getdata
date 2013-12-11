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
  const char *data = "dirfile/data";
  const char *format_data =
    "data RAW UINT8 8\n"
    "cata RAW UINT8 8\n"
    "/REFERENCE data\n";
  unsigned char c[8];
  unsigned char data_data[256];
  int fd, ret, e1, e2, r = 0;
  const char *ref1, *ref2;
  DIRFILE *D;

  memset(c, 0, 8);
  rmdirfile();
  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (unsigned char)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 256);
  close(fd);

  D = gd_open(filedir, GD_RDWR);
  ref1 = gd_reference(D, NULL);
  CHECKS(ref1, "data");

  ret = gd_delete(D, "data", 0);
  e1 = gd_error(D);
  CHECKI(ret, 0);
  CHECKI(e1, GD_E_OK);

  ref2 = gd_reference(D, NULL);
  CHECKS(ref2, "cata");

  e2 = gd_discard(D);
  CHECKI(e2, 0);

  fd = unlink(data);
  unlink(format);
  rmdir(filedir);
  CHECKI(fd, 0);

  return r;
}
