/* Copyright (C) 2011 D. V. Wiebe
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
    "/ALIAS e f\n"
    "/ALIAS a b\n"
    "/ALIAS b d\n"
    "/ALIAS c d\n"
    "/ALIAS d f\n"
    "f CONST UINT8 1\n";
  int fd, e, i = 0, r = 0;
  int x[256];
  DIRFILE *D;
  const char **al;

  rmdirfile();
  mkdir(filedir, 0777);

  memset(x, 0, 256 * sizeof(int));

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);

  al = gd_aliases(D, "a");
  e = gd_error(D);

  CHECKI(e, GD_E_OK);
  CHECKPN(al);

  if (!r)
    for (i = 0; al[i]; ++i) {
      CHECKUi(i, al[i][1], 0);
      x[al[i][0]]++;
    }

  CHECKI(i, 6);
  for (i = 0; i < 256; ++i)
    CHECKIi(i, x[i], (i >= 'a' && i <= 'f') ? 1 : 0);

  gd_close(D);
  unlink(format);
  rmdir(filedir);

  return r;
}
