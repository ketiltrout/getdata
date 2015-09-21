/* Copyright (C) 2015 D. V. Wiebe
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
  int e1, e2, r = 0;
  const char *in =
    "\xF\x1E\x2D\x3C\x4B\x5A\x69\x78\x87\x96\xA5\xB4\xC3\xD2\xE1\xF0";
  char s[1000];
  DIRFILE *D;

  rmdirfile();
  D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_TRUNC |
      GD_VERBOSE);
  gd_add_string(D, "STRING", in, 0);
  e1 = gd_error(D);
  CHECKI(e1, 0);

  gd_close(D);
  
  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  gd_get_string(D, "STRING", 1000, s);
  e2 = gd_error(D);
  CHECKI(e2, 0);
  CHECKS(s, in);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
