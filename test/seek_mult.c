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
#include "test.h"

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  int e0, e1, e2, e3, r = 0;
  off_t m, n1, n2, n3;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "mult MULTIPLY cata data\n"
    "/ENCODING none\n"
    "cata RAW UINT8 8\n"
    "data RAW UINT8 8\n"
  );

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  m = gd_seek(D, "mult", 6, 0, GD_SEEK_SET | GD_SEEK_WRITE);
  e0 = gd_error(D);
  n1 = gd_tell(D, "mult");
  e1 = gd_error(D);
  n2 = gd_tell(D, "cata");
  e2 = gd_error(D);
  n3 = gd_tell(D, "data");
  e3 = gd_error(D);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(e0, 0);
  CHECKI(e1, 0);
  CHECKI(e2, 0);
  CHECKI(e3, 0);
  CHECKI(m, 48);
  CHECKI(n1, 48);
  CHECKI(n2, 48);
  CHECKI(n3, 48);

  return r;
}
