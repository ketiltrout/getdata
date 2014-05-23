/* Copyright (C) 2014 D. V. Wiebe
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
    "data1 SARRAY one two\n"
    "data2 SARRAY three four five\n"
    "data3 STRING six\n"
    "data4 CONST UINT8 1\n";
  int fd, i, j, error, r = 0;
  const char ***l;
  const char *d1[] = {"one", "two"};
  const char *d2[] = {"three", "four", "five"};
  const char **d[] = {d1, d2};
  const int n[] = {2, 3};
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  l = gd_sarrays(D);

  error = gd_error(D);
  CHECKI(error, 0);
  CHECKPN(l);

  for (i = 0; l[i]; ++i) {
    if (i >= 2) {
      i++;
      break;
    }

    for (j = 0; l[i][j]; ++j) {
      if (j >= n[i]) {
        j++;
        break;
      }

      CHECKSi(i * 1000 + j, l[i][j], d[i][j]);
    }
    CHECKIi(i,j,n[i]);
  }

  CHECKI(i,2);

  gd_discard(D);
  unlink(format);
  rmdir(filedir);

  return r;
}
