/* Copyright (C) 2016, 2017 D.V. Wiebe
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
  int v1, v2, v3, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "/NAMESPACE ns\n"
    "/INCLUDE format1 ns2.pre post\n"
    "after RAW UINT8 1\n"
  );
  MAKEFORMATFILE(format1, "data RAW UINT8 1\n");

  D = gd_open(filedir, GD_RDONLY);
  v1 = gd_validate(D, "data");
  CHECKU(v1, GD_E_BAD_CODE);
  v2 = gd_validate(D, "ns2.predatapost");
  CHECKU(v2, 0);
  v3 = gd_validate(D, "ns.after");
  CHECKU(v3, 0);
  gd_discard(D);

  unlink(format1);
  unlink(format);
  rmdir(filedir);

  return r;
}
