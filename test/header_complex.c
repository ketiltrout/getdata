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
/* Check if GD_C89_API produces a useable API */
#define GD_C89_API
#include "test.h"

#include <math.h>
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
  const char *format_data =
    "lincom LINCOM data 3.3;4.4 5.5;6.6 data 7.7;8.8 9.9;1.1\n";
  int fd, error, error2, error3, r = 0;
  const double ca[] = { 2.1, 3.2, 4.3, 5.4, 6.5, 7.6 };
  gd_entry_t E;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  gd_entry(D, "lincom", &E);

  error = gd_error(D);
  CHECKI(error, 0);
  CHECKIi(0,E.comp_scal, 1);
  CHECKFi(0,E.EN(lincom,cm)[0][0], 3.3);
  CHECKFi(0,E.EN(lincom,cm)[0][1], 4.4);
  CHECKFi(0,E.EN(lincom,cb)[0][0], 5.5);
  CHECKFi(0,E.EN(lincom,cb)[0][1], 6.6);
  CHECKFi(0,E.EN(lincom,cm)[1][0], 7.7);
  CHECKFi(0,E.EN(lincom,cm)[1][1], 8.8);
  CHECKFi(0,E.EN(lincom,cb)[1][0], 9.9);
  CHECKFi(0,E.EN(lincom,cb)[1][1], 1.1);
  gd_free_entry_strings(&E);

  gd_add_cpolynom(D, "polynom", 2, "in", ca, 0);

  error2 = gd_error(D);
  CHECKI(error2, 0);

  gd_entry(D, "polynom", &E);

  error3 = gd_error(D);
  CHECKI(error3, 0);
  CHECKIi(1,E.EN(polynom,poly_ord),2);
  CHECKIi(1,E.comp_scal,1);
  CHECKFi(1,E.EN(polynom,ca)[0][0], ca[0]);
  CHECKFi(1,E.EN(polynom,ca)[0][1], ca[1]);
  CHECKFi(1,E.EN(polynom,ca)[1][0], ca[2]);
  CHECKFi(1,E.EN(polynom,ca)[1][1], ca[3]);
  CHECKFi(1,E.EN(polynom,ca)[2][0], ca[4]);
  CHECKFi(1,E.EN(polynom,ca)[2][1], ca[5]);
  gd_free_entry_strings(&E);

  gd_close(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
