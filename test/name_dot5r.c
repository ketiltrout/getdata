/* Copyright (C) 2014, 2016, 2017 D.V. Wiebe
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
  int r1, e1, r = 0;
  DIRFILE *D;
  gd_entry_t E1, E2;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "/VERSION 5\nda.ta RAW UINT8 8\n");

  D = gd_open(filedir, GD_RDWR);

  if (gd_entry(D, "da.ta", &E1) == 0)
    gd_free_entry_strings(&E1);
  else
    r = 1;

  /* This should always work */
  r1 = gd_rename(D, "da.ta", "data", 0);
  e1 = gd_error(D);
  CHECKI(r1,0);
  CHECKI(e1,0);

  if (gd_entry(D, "data", &E2) == 0)
    gd_free_entry_strings(&E2);
  else
    r = 1;

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
