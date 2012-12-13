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
/* Add a complex POLYNOM field */
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
  int error, j;
  gd_entry_t e;
  DIRFILE *D;
#ifdef GD_NO_C99_API
  const double a[8] = {1, 29.03, 0.3, 12.34, 0.5, 99.55, 1.8, 45.32};
#else
  const double complex a[4] = {1 + _Complex_I * 29.03, 0.3 + _Complex_I * 12.34,
    0.5 + _Complex_I * 99.55, 1.8 + _Complex_I * 45.32};
#endif

  rmdirfile();
  D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  gd_add_cpolynom(D, "new", 3, "in", a, 0);
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
    CHECKI(e.comp_scal, 1);
    for (j = 0; j < 4; ++j) {
#ifdef GD_NO_C99_API
      CHECKCi(j,e.EN(polynom,ca)[j], a + 2 * j);
#else
      CHECKCi(j,e.EN(polynom,ca)[j], a[j]);
#endif
    }
    gd_free_entry_strings(&e);
  }

  gd_close(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error, GD_E_OK);
  return r;
}
