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
  const char *data = "dirfile/data";
  const char *dati[8], *datz[8], *datzz[8];
  int i, e1, e2, e3, r = 0;
  size_t n1, n2, n3;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  /* Because representation .i is not allowed on SARRAYs, "sarray.i" here ends
   * up being interpreted as the field called "i" in the namespace "sarray".
   * "sarray.z", however, is interpreted as the field "sarray" with the
   * do-nothing representation suffix ".z".  So, to get the field "z" in the
   * "sarray" namespace, we must write it as "sarray.z.z".  Craziness.
   */
  MAKEFORMATFILE(format,
    "sindir+i  SINDIR data sarray.i\n"
    "sindir+z  SINDIR data sarray.z\n"
    "sindir+zz SINDIR data sarray.z.z\n"
    "sarray   SARRAY a b c d e f g h i j k l m\n"
    "sarray.i SARRAY A B C D E F G H I J K L M\n"
    "sarray.z SARRAY 0 1 2 3 4 5 6 7 8 9 : ; <\n"
    "data RAW UINT8 8\n");

  MAKEDATAFILE(data, uint8_t, i, 256);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n1 = gd_getdata(D, "sindir+i", 0, 0, 1, 0, GD_STRING, dati);
  CHECKU(n1, 8);
  e1 = gd_error(D);
  CHECKI(e1, 0);

  for (i = 0; i < 8; ++i)
    CHECKXi(i, dati[i][0], 'A' + i);

  n2 = gd_getdata(D, "sindir+z", 0, 0, 1, 0, GD_STRING, datz);
  CHECKU(n2, 8);
  e2 = gd_error(D);
  CHECKI(e2, 0);

  for (i = 0; i < 8; ++i)
    CHECKXi(i, datz[i][0], 'a' + i);

  n3 = gd_getdata(D, "sindir+zz", 0, 0, 1, 0, GD_STRING, datzz);
  CHECKU(n3, 8);
  e3 = gd_error(D);
  CHECKI(e3, 0);

  for (i = 0; i < 8; ++i)
    CHECKXi(i, datzz[i][0], '0' + i);

  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  return r;
}
