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
#include <inttypes.h>
#include <stdio.h>

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *format1 = "dirfile/format1";
  const char *format_data =
    "data RAW UINT16 8\n"
    "/ENDIAN little\n"
    "/INCLUDE format1\n";
  const char *format1_data = "data1 RAW UINT16 8\n/ENDIAN little arm\n";
  unsigned long e0, e1, e2, e3;
  int fd, ret, error, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(format1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format1_data, strlen(format1_data));
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  e0 = gd_endianness(D, 0);
  e1 = gd_endianness(D, 1);
  ret = gd_alter_endianness(D, GD_BIG_ENDIAN, GD_ALL_FRAGMENTS, 0);
  error = gd_error(D);
  e2 = gd_endianness(D, 0);
  e3 = gd_endianness(D, 1);

  gd_discard(D);

  unlink(format);
  unlink(format1);
  rmdir(filedir);

  CHECKI(error,0);
  CHECKI(ret, 0);
  CHECKU(e0, GD_LITTLE_ENDIAN | GD_NOT_ARM_ENDIAN);
  CHECKU(e1, GD_LITTLE_ENDIAN | GD_ARM_ENDIAN);
  CHECKU(e2, GD_BIG_ENDIAN);
  CHECKU(e3, GD_BIG_ENDIAN);

  return r;
}
