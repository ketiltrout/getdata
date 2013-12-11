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
  const char *format_data = "data BIT in 3 3\n";
  int fd, ret, error, n, r = 0;
  DIRFILE *D;
  gd_entry_t E;

  rmdirfile();
  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);

  memset(&E, 0, sizeof(E));
  E.field_type = GD_BIT_ENTRY;
  E.bitnum = 4;
  E.numbits = 3;
  E.in_fields[0] = "in";
  E.scalar[0] = "";

  ret = gd_alter_entry(D, "data", &E, 0);
  error = gd_error(D);

  n = gd_entry(D, "data", &E);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error,0);
  CHECKI(n,0);
  CHECKI(ret,0);
  CHECKI(E.EN(bit,bitnum), 4);
  CHECKP(E.scalar[0]);
  gd_free_entry_strings(&E);

  return r;
}