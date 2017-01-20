/* Copyright (C) 2009-2011, 2013, 2017 D.V. Wiebe
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
/* Test field modifying */
#include "test.h"

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  int ret, error, n, r = 0;
  DIRFILE *D;
  gd_entry_t E;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "data BIT in c1 3\nc1 CONST INT64 3\n");

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  gd_entry(D, "data", &E);
  free(E.scalar[0]);
  E.scalar[0] = NULL;
  ret = gd_alter_entry(D, "data", &E, 0);
  error = gd_error(D);

  gd_free_entry_strings(&E);
  n = gd_entry(D, "data", &E);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error,0);
  CHECKI(n,0);
  CHECKI(ret,0);
  CHECKI(E.EN(bit,bitnum), 3);
  CHECKP(E.scalar[0]);
  gd_free_entry_strings(&E);

  return r;
}
