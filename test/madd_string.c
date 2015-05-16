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
/* Add a RAW field */
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
  int error, mf_error, ge_error, r = 0;
  gd_entry_t e;
  char val[1000];
  DIRFILE *D;

  rmdirfile();
  D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  gd_add_phase(D, "new", "in", 3, 0);
  gd_madd_string(D, "new", "meta", "A string.");
  error = gd_error(D);
  CHECKI(error, GD_E_OK);

  gd_metaflush(D);
  mf_error = gd_error(D);
  CHECKI(mf_error, GD_E_OK);

  /* check */
  gd_entry(D, "new/meta", &e);
  ge_error = gd_error(D);
  CHECKI(ge_error, 0);
  if (!r) {
    CHECKI(e.field_type, GD_STRING_ENTRY);
    CHECKI(e.fragment_index, 0);
    gd_get_string(D, "new/meta", 1000, val);
    CHECKS(val, "A string.");
    gd_free_entry_strings(&e);
  }

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
