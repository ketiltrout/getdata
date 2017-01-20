/* Copyright (C) 2013, 2017 D.V. Wiebe
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
  int ret, error, r = 0;
  gd_entry_t E;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "lincom LINCOM a 1 2\n");

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);

  gd_entry(D, "lincom", &E);
  E.EN(lincom,n_fields) = 2;
  E.EN(lincom,m[0]) = 3;
  E.EN(lincom,b[0]) = 4;
  E.EN(lincom,m[1]) = 5;
  E.EN(lincom,b[1]) = 6;
  E.in_fields[1] = "b";
  ret = gd_alter_entry(D, "lincom", &E, 0);
  error = gd_error(D);

  E.in_fields[1] = NULL;
  gd_free_entry_strings(&E);
  memset(&E, 0, sizeof(E));
  gd_entry(D, "lincom", &E);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(ret, 0);
  CHECKS(E.in_fields[0], "a");
  CHECKS(E.in_fields[1], "b");
  CHECKI(E.EN(lincom,n_fields), 2);
  CHECKF(E.EN(lincom,m[0]), 3);
  CHECKF(E.EN(lincom,b[0]), 4);
  CHECKF(E.EN(lincom,m[1]), 5);
  CHECKF(E.EN(lincom,b[1]), 6);
  gd_free_entry_strings(&E);

  return r;
}
