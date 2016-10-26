/* Copyright (C) 2014 D. V. Wiebe
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

#include <inttypes.h>
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
  const char *val[] = {"q", "r", "s", "t", "u", "v"};
  const char *data[6] = {NULL, NULL, NULL, NULL, NULL, NULL};
  int n, error;
  int r = 0;
  gd_entry_t e;
  DIRFILE *D;

  rmdirfile();
  D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  gd_add_phase(D, "new", "in", 3, 0);
  gd_madd_sarray(D, "new", "data", 6, val);
  error = gd_error(D);
  CHECKI(error, GD_E_OK);

  /* check */
  gd_entry(D, "new/data", &e);
  if (gd_error(D))
    r = 1;
  else {
    CHECKI(e.field_type, GD_SARRAY_ENTRY);
    gd_free_entry_strings(&e);
  }
  n = (int)gd_array_len(D, "new/data");
  CHECKI(n, 6);

  gd_get_sarray(D, "new/data", data);
  for (n = 0; n < 6; ++n)
    CHECKSi(n, data[n], val[n]);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
