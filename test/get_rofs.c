/* Copyright (C) 2008-2011, 2013, 2017 D.V. Wiebe
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
/* Should be able to read from a R/O dirfile */
#include "test.h"

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  unsigned char c[8];
  int i, fd, n, error, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "data RAW UINT8 8\n");
  MAKEDATAFILE(data, unsigned char, i, 256);
  chmod(data, 0500);

  /* ensure filesystem honours read-onlyness */
  if ((fd = open(data, O_RDWR)) >= 0 || errno != EACCES) {
    if (fd >= 0)
      close(fd);
    unlink(format);
    unlink(data);
    rmdir(filedir);
    return 77;
  }

  D = gd_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);
  n = gd_getdata(D, "data", 5, 0, 1, 0, GD_UINT8, c);

  error = gd_error(D);
  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(n, 8);
  for (i = 0; i < 8; ++i)
    CHECKUi(i, c[i], 40 + i);

  return r;
}
