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
/* Try to read LINCOM entry */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <math.h>

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *format_data = "data LINCOM 3 in1 1 2 in2 3 4 in3 5 6\n";
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
  CHECKI(E.field_type, GD_LINCOM_ENTRY);
  CHECKI(E.comp_scal, 0);
  CHECKI(E.EN(lincom,n_fields), 3);
  CHECKS(E.in_fields[0], "in1");
  CHECKS(E.in_fields[1], "in2");
  CHECKS(E.in_fields[2], "in3");
  CHECKF(E.EN(lincom,m)[0], 1.);
  CHECKF(E.EN(lincom,b)[0], 2.);
  CHECKF(E.EN(lincom,m)[1], 3.);
  CHECKF(E.EN(lincom,b)[1], 4.);
  CHECKF(E.EN(lincom,m)[2], 5.);
  CHECKF(E.EN(lincom,b)[2], 6.);
  gd_free_entry_strings(&E);

  return r;
}
