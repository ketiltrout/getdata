/* Copyright (C) 2009-2011, 2013 D. V. Wiebe
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
/* Add a complex LINCOM field */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <math.h>

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  DIRFILE *D;
  const char *in_fields[2] = {"in1", "in2"};
#ifdef GD_NO_C99_API
  const double m[4] = {1, 3.3, 0.3, 18.3};
  const double b[4] = {2, 3.8, 2.1, 9.8};
#else
  const double complex m[2] = {1 + _Complex_I * 3.3, 0.3 + _Complex_I * 18.3};
  const double complex b[2] = {2 + _Complex_I * 3.8, 2.1 + _Complex_I * 9.8};
#endif
  int error, r = 0;
  gd_entry_t e;

  rmdirfile();
  D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);

  gd_add_clincom(D, "new", 2, in_fields, m, b, 0);
  error = gd_error(D);

  /* check */
  gd_entry(D, "new", &e);
  if (gd_error(D))
    r = 1;
  else {
    CHECKI(e.field_type, GD_LINCOM_ENTRY);
    CHECKI(e.fragment_index, 0);
    CHECKI(e.EN(lincom,n_fields),2);
    CHECKS(e.in_fields[0], "in1");
    CHECKS(e.in_fields[1], "in2");
#ifdef GD_NO_C99_API
    CHECKC(e.EN(lincom,cm)[0], m);
    CHECKC(e.EN(lincom,cm)[1], m + 2);
    CHECKC(e.EN(lincom,cb)[0], b);
    CHECKC(e.EN(lincom,cb)[1], b + 2);
#else
    CHECKC(e.EN(lincom,cm)[0], m[0]);
    CHECKC(e.EN(lincom,cm)[1], m[1]);
    CHECKC(e.EN(lincom,cb)[0], b[0]);
    CHECKC(e.EN(lincom,cb)[1], b[1]);
#endif
    CHECKX(e.flags, GD_EN_COMPSCAL | GD_EN_CALC);
    gd_free_entry_strings(&e);
  }

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error, GD_E_OK);

  return r;
}
