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
/* Add a POLYNOM field */
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
  int r = 0;
  int j, error;
  gd_entry_t e;
  DIRFILE *D;
  const double a[4] = {1, 0.3, 0.5, 1.8};

  rmdirfile();
  D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  gd_add_polynom(D, "new", 3, "in", a, 0);
  error = gd_error(D);

  /* check */
  gd_entry(D, "new", &e);
  if (gd_error(D))
    r = 1;
  else {
    CHECKI(e.field_type, GD_POLYNOM_ENTRY);
    CHECKS(e.in_fields[0], "in");
    CHECKI(e.fragment_index, 0);
    CHECKI(e.EN(polynom,poly_ord), 3);
    CHECKI(e.comp_scal, 0);
    for (j = 0; j < 4; ++j)
      CHECKFi(j,e.EN(polynom,a)[j], a[j]);
    gd_free_entry_strings(&e);
  }

  gd_close(D);

  unlink(format);
  rmdir(filedir);
  
  CHECKI(error, GD_E_OK);
  return r;
}
