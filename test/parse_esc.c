/* Copyright (C) 2013, 2017 D. V. Wiebe
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
  int e1, r = 0;
  const char *s;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "string STRING \\a\\b\\c\\d\\e\\f\\g\\h\\i\\j\\k\\l"
      "\\m\\n\\o\\p\\q\\r\\s\\t\\v\\w\\y\\z\\\\\\\"\\#");

  D = gd_open(filedir, GD_RDONLY);
  e1 = gd_error(D);
  CHECKI(e1, 0);

  gd_get_sarray(D, "string", &s);
  CHECKS(s, "\a\bcd\x1B\fghijklm\nopq\rs\t\vwyz\\\"#");
  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
