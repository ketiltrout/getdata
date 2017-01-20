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
  const char *data = "dirfile/data";
  const char *c[8];
  int i, n, error, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "sindir SINDIR data sarray\n"
    "sarray SARRAY a b c d e f g h i j k l m n o\n"
    "data RAW UINT8 8\n"
  );
  MAKEDATAFILE(data, unsigned char, (i + 2), 256);

  D = gd_open(filedir, GD_RDONLY);
  n = gd_getdata(D, "sindir", 1, 0, 1, 0, GD_STRING, c);
  error = gd_error(D);

  CHECKI(error, 0);
  CHECKI(n, 8);
  for (i = 0; i < 8; ++i) {
    if (i < 5) {
      CHECKPNi(i, c[i]);
      CHECKXi(i, c[i][0], 'k' + i);
    } else {
      CHECKPi(i, c[i]);
    }
  }

  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  return r;
}
