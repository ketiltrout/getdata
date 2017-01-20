/* Copyright (C) 2008-2011, 2013, 2017 D.V. Wiebe
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
/* Test endianness */
#include "test.h"

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *format1 = "dirfile/format1";
  const char *format2 = "dirfile/format2";
  const char *format3 = "dirfile/format3";
  const char *format_data1 = "ENDIAN big\nINCLUDE format2\n";
  const char *format_data2 = "ENDIAN big arm\nINCLUDE format3\n";
  const char *format_data3 = "ENDIAN little arm\n";
  int fd, error, r = 0;
  unsigned long n, m, l, k;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "ENDIAN little\nINCLUDE format1\n");

  fd = open(format1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data1, strlen(format_data1));
  close(fd);

  fd = open(format2, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data2, strlen(format_data2));
  close(fd);

  fd = open(format3, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data3, strlen(format_data3));
  close(fd);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n = gd_endianness(D, 0);
  m = gd_endianness(D, 1);
  l = gd_endianness(D, 2);
  k = gd_endianness(D, 3);
  error = gd_error(D);

  gd_discard(D);

  unlink(format);
  unlink(format1);
  unlink(format2);
  unlink(format3);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKX(n, GD_LITTLE_ENDIAN | GD_NOT_ARM_ENDIAN);
  CHECKX(m, GD_BIG_ENDIAN | GD_NOT_ARM_ENDIAN);
  CHECKX(l, GD_BIG_ENDIAN | GD_ARM_ENDIAN);
  CHECKX(k, GD_LITTLE_ENDIAN | GD_ARM_ENDIAN);

  return r;
}
