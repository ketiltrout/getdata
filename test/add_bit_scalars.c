/* Copyright (C) 2013 D. V. Wiebe
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
  int e1, e2, e3, r = 0;
  DIRFILE *D;
  gd_entry_t E;

  rmdirfile();
  memset(&E, 0, sizeof(E));
  E.field = "bit1";
  E.field_type = GD_BIT_ENTRY;
  E.in_fields[0] = "INDEX";
  E.EN(bit,bitnum) = -1;
  E.EN(bit,numbits) = 1;
  E.scalar[0] = "const";

  D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  gd_add(D, &E);
  e1 = gd_error(D);

  E.field = "bit2";
  E.EN(bit,bitnum) = 1;
  E.EN(bit,numbits) = 0;
  E.scalar[0] = NULL;
  E.scalar[1] = "const";
  gd_add(D, &E);
  e2 = gd_error(D);

  E.field = "bit3";
  E.EN(bit,bitnum) = 61;
  E.EN(bit,numbits) = 5;
  E.scalar[0] = "const";
  E.scalar[1] = "const";
  gd_add(D, &E);
  e3 = gd_error(D);

  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(e1, GD_E_OK);
  CHECKI(e2, GD_E_OK);
  CHECKI(e3, GD_E_OK);

  return r;
}
