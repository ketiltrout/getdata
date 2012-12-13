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
/* Test include */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *format1 = "dirfile/format1";
  const char *format2 = "dirfile/format2";
  const char *format_data = "/INCLUDE format1\na CONST UINT8 1\n";
  const char *format1_data = "b CONST UINT8 11\n/INCLUDE format2\n";
  const char *format2_data = "c CONST UINT8 11\n";
  int fd, ret, error, unlink_format1, unlink_format2, r = 0;
  unsigned int nfields, nfragments;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(format1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format1_data, strlen(format1_data));
  close(fd);

  fd = open(format2, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format2_data, strlen(format2_data));
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  ret = gd_uninclude(D, 1, 0);
  error = gd_error(D);
  nfields = gd_nfields(D);
  nfragments = gd_nfragments(D);
  gd_close(D);

  unlink_format2 = unlink(format2);
  unlink_format1 = unlink(format1);
  unlink(format);
  rmdir(filedir);

  CHECKI(error,0);
  CHECKI(ret,0);
  CHECKI(nfields,2);
  CHECKI(nfragments,1);
  CHECKI(unlink_format2,0);
  CHECKI(unlink_format1,0);

  return r;
}
