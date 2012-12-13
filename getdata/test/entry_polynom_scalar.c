/* Copyright (C) 2009-2011 D. V. Wiebe
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
/* Try to read LINCOM entry */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#include <stdio.h>

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *format_data =
    "a0 CONST FLOAT64 1\n"
    "a1 CONST FLOAT64 2\n"
    "a2 CONST FLOAT64 3\n"
    "a3 CONST FLOAT64 4\n"
    "a4 CONST FLOAT64 5\n"
    "data POLYNOM in a0 a1 a2 a3 a4\n";
  int fd, n, error, r = 0;
  DIRFILE *D;
  gd_entry_t E;

  rmdirfile();
  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);

  n = gd_entry(D, "data", &E);
  error = gd_error(D);

  gd_close(D);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, GD_E_OK);
  CHECKI(n, 0);
  CHECKS(E.field, "data");
  CHECKX(E.field_type, GD_POLYNOM_ENTRY);
  CHECKI(E.comp_scal, 0);
  CHECKI(E.EN(polynom,poly_ord), 4);
  CHECKS(E.in_fields[0], "in");
  CHECKS(E.scalar[0], "a0");
  CHECKS(E.scalar[1], "a1");
  CHECKS(E.scalar[2], "a2");
  CHECKS(E.scalar[3], "a3");
  CHECKS(E.scalar[4], "a4");
  for (fd = 0; fd < 4; ++fd)
    CHECKFi(fd,E.EN(polynom,a)[fd], fd + 1.);
  gd_free_entry_strings(&E);

  return r;
}
