/* Copyright (C) 2014, 2017 D.V. Wiebe
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
  const char *format2 = "dirfile/format2";
  int r = 0;
  DIRFILE *D;
  unsigned int spf;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "INCLUDE format1 ns.pre post\n");
  MAKEFORMATFILE(format1, "INCLUDE format2\n");
  MAKEFORMATFILE(format2,
    "/NAMESPACE .\n"
    "data RAW UINT8 11\n"
  );

  D = gd_open(filedir, GD_RDONLY);
  spf = gd_spf(D, "ns.predatapost");
  CHECKU(spf, 11);
  gd_discard(D);

  unlink(format2);
  unlink(format1);
  unlink(format);
  rmdir(filedir);

  return r;
}
