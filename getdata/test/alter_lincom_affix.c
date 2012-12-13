/* Copyright (C) 2012 D. V. Wiebe
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
  const char *format1 = "dirfile/format1";
  const char *data = "dirfile/data";
  const char *format_data = "INCLUDE format1 A Z\n";
  const char *format1_data = "data RAW INT32 8\nphase PHASE data 1\n"
    "lincom LINCOM 2 data 1 0 data 1 0\n";
  int32_t data_data[256];
  int32_t c[8];
  int fd, i, e1, e2, n, r = 0;
  DIRFILE *D;
  const char *in_fields[3] = {"data", "phase", NULL};
  const char *Ain_fieldsZ[3] = {"AdataZ", "AphaseZ", NULL};

  rmdirfile();
  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (int32_t)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(format1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format1_data, strlen(format1_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 256 * sizeof(int32_t));
  close(fd);

  D = gd_open(filedir, GD_RDWR);
  gd_alter_lincom(D, "AlincomZ", 0, in_fields, NULL, NULL);
  e1 = gd_error(D);
  gd_alter_lincom(D, "AlincomZ", 0, Ain_fieldsZ, NULL, NULL);
  e2 = gd_error(D);
  n = gd_getdata(D, "AlincomZ", 5, 0, 1, 0, GD_INT32, c);

  gd_discard(D);

  for (i = 0; i < 8; ++i)
    CHECKIi(i,c[i], i * 2 + 81);

  unlink(data);
  unlink(format1);
  unlink(format);
  rmdir(filedir);

  CHECKI(e1, GD_E_BAD_CODE);
  CHECKI(e2, 0);
  CHECKI(n, 8);

  return r;
}
