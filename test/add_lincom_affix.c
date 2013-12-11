/* Copyright (C) 2012-2013 D. V. Wiebe
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
/* Add a LINCOM field */
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
  const char *format1 = "dirfile/format1";
  int e1, e2, e3, r = 0;
  DIRFILE *D;
  const char *in_fields[2] = {"in1", "in2"};
  const char *Ain_fieldsZ[2] = {"Ain1Z", "Ain2Z"};
  const double m[2] = {1, 0.3};
  const double b[2] = {5, 0.9};
  gd_entry_t e;

  rmdirfile();
  D = gd_open(filedir, GD_RDWR | GD_CREAT);
  gd_include_affix(D, "format1", 0, "A", "Z", GD_CREAT);

  gd_add_lincom(D, "new", 2, Ain_fieldsZ, m, b, 1);
  e1 = gd_error(D);
  gd_add_lincom(D, "AnewZ", 2, in_fields, m, b, 1);
  e2 = gd_error(D);
  gd_add_lincom(D, "AnewZ", 2, Ain_fieldsZ, m, b, 1);
  e3 = gd_error(D);

  /* check */
  gd_entry(D, "AnewZ", &e);
  if (gd_error(D))
    r = 1;
  else {
    CHECKI(e.field_type, GD_LINCOM_ENTRY);
    CHECKI(e.fragment_index, 1);
    CHECKI(e.EN(lincom,n_fields), 2);
    CHECKS(e.in_fields[0], "Ain1Z");
    CHECKS(e.in_fields[1], "Ain2Z");
    CHECKF(e.EN(lincom,m)[0], m[0]);
    CHECKF(e.EN(lincom,m)[1], m[1]);
    CHECKF(e.EN(lincom,b)[0], b[0]);
    CHECKF(e.EN(lincom,b)[1], b[1]);
    CHECKX(e.flags, GD_EN_CALC);
    gd_free_entry_strings(&e);
  }

  gd_discard(D);

  unlink(format1);
  unlink(format);
  rmdir(filedir);

  CHECKI(e1, GD_E_BAD_CODE);
  CHECKI(e2, GD_E_BAD_CODE);
  CHECKI(e3, GD_E_OK);
  
  return r;
}
