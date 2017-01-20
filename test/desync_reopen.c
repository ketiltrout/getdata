/* Copyright (C) 2012-2013, 2017 D.V. Wiebe
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
  int e1, e2, e3, n1, n2, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);
  MAKEEMPTYFILE(format, 0600);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  e1 = gd_error(D);
  
  /* ensure mtime ticks over */
  sleep(1);

  /* modify the format file */
#undef O_EXCL
#define O_EXCL 0
  MAKEFORMATFILE(format, "data RAW UINT8 1\n");

  n1 = gd_desync(D, GD_DESYNC_REOPEN);
  e2 = gd_error(D);
  n2 = gd_validate(D, "data");
  e3 = gd_error(D);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(e1, 0);
  CHECKI(e2, 0);
  CHECKI(e3, 0);
  CHECKI(n1, 1);
  CHECKI(n2, 0);
  return r;
}
