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
  const char *data = "dirfile/data";
  int e1, r = 0;
  DIRFILE *D;
  gd_entry_t E;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
      "c1 CONST UINT8 1\n"
      "c2 CONST UINT8 2\n"
      "c3 CONST UINT8 3\n"
      "bit BIT in1 c1 c2\n"
      );

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  e1 = gd_alter_spec(D, "bit BIT in1 c2 c3", 0);
  CHECKI(e1, 0);

  gd_entry(D, "bit", &E);
  CHECKI(E.EN(bit,bitnum), 2);
  CHECKI(E.EN(bit,numbits), 3);
  CHECKS(E.scalar[0], "c2");
  CHECKS(E.scalar[1], "c3");
  gd_free_entry_strings(&E);

  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  return r;
}
