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
  int r = 0;
  off_t e1, e2, e3;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
      "data RAW UINT8 1\n"
      "lincom1 LINCOM missing 1 0 data 1 0 INDEX 1 0\n"
      "lincom2 LINCOM INDEX 1 0 missing 1 0 data 1 0\n"
      "lincom3 LINCOM data 1 0 INDEX 1 0 missing 1 0\n"
      );
  MAKEDATAFILE(data, uint8_t, i, 256);

  D = gd_open(filedir, GD_RDONLY);
  e1 = gd_eof(D, "lincom1");
  CHECKI(e1, GD_E_BAD_CODE);
  e2 = gd_eof(D, "lincom2");
  CHECKI(e2, GD_E_BAD_CODE);
  e3 = gd_eof(D, "lincom3");
  CHECKI(e3, GD_E_BAD_CODE);

  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  return r;
}
