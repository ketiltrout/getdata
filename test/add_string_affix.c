/* Copyright (C) 2011, 2013, 2017 D.V. Wiebe
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

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *format1 = "dirfile/format1";
  const char *data = "dirfile/data";
  int e1, e2, r = 0;
  char val[1000];
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "/INCLUDE format1 A Z\n");
  MAKEEMPTYFILE(format1, 0600);

  D = gd_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);
  gd_add_string(D, "AdataZ", "A string.", 1);
  e1 = gd_error(D);

  /* check */
  gd_get_string(D, "AdataZ", 1000, val);
  e2 = gd_error(D);
  gd_discard(D);

  unlink(data);
  unlink(format);
  unlink(format1);
  rmdir(filedir);

  CHECKI(e1, GD_E_OK);
  CHECKI(e2, GD_E_OK);
  CHECKS(val, "A string.");

  return r;
}
