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

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  int error, r = 0;
  DIRFILE *D;
  gd_entry_t E, e;

  rmdirfile();
  memset(&E, 0, sizeof(E));
  E.field = "dat.a";
  E.field_type = GD_PHASE_ENTRY;
  E.in_fields[0] = "INDEX";
  E.EN(phase,shift) = 0;
  E.scalar[0] = NULL;

  D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  gd_dirfile_standards(D, 5);
  gd_add(D, &E);
  error = gd_error(D);

  CHECKI(error, GD_E_OK);

  /* check */
  gd_entry(D, "dat.a", &e);
  if (gd_error(D))
    r = 1;
  else {
    CHECKI(e.field_type, GD_PHASE_ENTRY);
    CHECKS(e.in_fields[0], "INDEX");
    CHECKI(e.fragment_index, 0);
    CHECKI(e.EN(phase,shift), 0);
    CHECKP(e.scalar[0]);
    gd_free_entry_strings(&e);
  }

  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  return r;
}
