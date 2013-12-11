/* Copyright (C) 2008-2011, 2013 D. V. Wiebe
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
/* Add a dirfile field */
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
  int e1, e2, r = 0;
  DIRFILE *D;
  gd_entry_t E, e;

  rmdirfile();
  memset(&E, 0, sizeof(E));
  E.field = "data";
  E.field_type = GD_CONST_ENTRY;
  E.fragment_index = 0;
  E.EN(scalar,const_type) = GD_UINT8;

  D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  gd_add(D, &E);
  e1 = gd_error(D);
  E.field = "data/data";
  gd_add(D, &E);
  e2 = gd_error(D);

  /* check */
  gd_entry(D, "data/data", &e);
  if (gd_error(D))
    r = 1;
  else {
    CHECKI(e.field_type, GD_CONST_ENTRY);
    CHECKI(e.fragment_index, 0);
    CHECKI(e.EN(scalar,const_type), GD_UINT8);
    gd_free_entry_strings(&e);
  }

  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(e1, GD_E_OK);
  CHECKI(e2, GD_E_OK);

  return r;
}
