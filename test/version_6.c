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
/* Open a Standards Version 6 conformant dirfile */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <stdio.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/a#r";
  const char *format_data = "/VERSION 6\na\\#r RAW UINT8 8\n";
  uint16_t c[8];
  unsigned char data_data[256];
  int fd, i, n, error, v, l, e, r = 0;
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

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n = gd_getdata(D, "a#r", 5, 0, 1, 0, GD_UINT16, c);
  error = gd_error(D);

  v = gd_dirfile_standards(D, GD_VERSION_CURRENT);
  l = gd_dirfile_standards(D, GD_VERSION_LATEST);
  e = gd_dirfile_standards(D, GD_VERSION_EARLIEST);

  gd_close(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error,0);
  CHECKI(n,8);

  for (i = 0; i < 8; ++i)
    CHECKUi(i,c[i],40 + i);

  /* Version 6 is forward compatible with versions 7 and 8 */
  CHECKI(v,6);
  CHECKI(l,8);
  CHECKI(e,6);

  return r;
}
