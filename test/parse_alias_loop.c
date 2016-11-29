/* Copyright (C) 2016 D. V. Wiebe
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
  int e1, e2, e3, e4, e5, r = 0;
  const char *s1, *s2, *s3, *s4;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
      "/ALIAS loop1 loop1\n"
      "/ALIAS loop3a loop3b\n"
      "/ALIAS loop3b loop3c\n"
      "/ALIAS loop3c loop3a\n"
      );

  D = gd_open(filedir, GD_RDONLY);
  e1 = gd_error(D);
  CHECKI(e1, 0);

  e2 = gd_validate(D, "loop1");
  CHECKI(e2, GD_E_BAD_CODE);

  s1 = gd_alias_target(D, "loop1");
  CHECKS(s1, "loop1");

  e3 = gd_validate(D, "loop3a");
  CHECKI(e3, GD_E_BAD_CODE);

  s2 = gd_alias_target(D, "loop3a");
  CHECKS(s2, "loop3b");

  e4 = gd_validate(D, "loop3b");
  CHECKI(e4, GD_E_BAD_CODE);

  s3 = gd_alias_target(D, "loop3b");
  CHECKS(s3, "loop3c");

  e5 = gd_validate(D, "loop3c");
  CHECKI(e5, GD_E_BAD_CODE);

  s4 = gd_alias_target(D, "loop3c");
  CHECKS(s4, "loop3a");

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
