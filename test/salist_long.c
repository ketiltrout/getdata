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
  const char *format_data =
    "data SARRAY a\n"
    "datb SARRAY b\n"
    "datc SARRAY c\n"
    "datd SARRAY d\n"
    "date SARRAY e\n"
    "datf SARRAY f\n"
    "datg SARRAY g\n"
    "dath SARRAY h\n"
    "dati SARRAY i\n"
    "datj SARRAY j\n"
    "datk SARRAY k\n"
    "datl SARRAY l\n"
    "datm SARRAY m\n"
    "datn SARRAY n\n"
    "dato SARRAY o\n"
    "datp SARRAY p\n"
    "datq SARRAY q\n"
    "datr SARRAY r\n"
    "dats SARRAY s\n"
    "datt SARRAY t\n"
    "datu SARRAY u\n"
    "datv SARRAY v\n"
    "datw SARRAY w\n"
    "datx SARRAY x\n"
    "daty SARRAY y\n"
    "datz SARRAY z\n"
    ;
  int j, error, r = 0;
  const char ***field_list;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  j = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(j, format_data, strlen(format_data));
  close(j);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  field_list = gd_sarrays(D);

  error = gd_error(D);

  CHECKPN(field_list);
  CHECKI(error, 0);

  if (!r)
    for (j = 0; field_list[j]; ++j) {
      CHECKPNi(j, field_list[j][0]);
      CHECKIi(j, field_list[j][0][0], 'a' + j);
      CHECKIi(j, field_list[j][0][1], 0);
      CHECKPi(j, field_list[j][1]);
    }


  gd_discard(D);
  unlink(format);
  rmdir(filedir);

  return r;
}
