/* Copyright (C) 2008-2011, 2013, 2017 D.V. Wiebe
 * Copyright (C) 2019 Matthew Petroff
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
#ifdef HAVE_ZZIP_LIB_H
  const char *filedir = "dirfile";
  const char *filedirzip = "dirfile.zip";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  const char *command = "zip -jq0 dirfile.zip dirfile/format dirfile/data";
  int32_t c[8];
  int i, n, error, r = 0;
  DIRFILE *D;

  memset(c, 0, 8 * sizeof(*c));
  rmdirfile();
  unlink(filedirzip);
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "/ENCODING none\ndata RAW INT32 8\n");
  MAKEDATAFILE(data, int32_t, i * (0x02000001) * (2 * (i % 2) - 1), 64);

  if (gd_system(command))
    return 1;

  D = gd_open(filedirzip, GD_RDONLY | GD_VERBOSE);
  n = gd_getdata(D, "data", 5, 0, 1, 0, GD_INT32, c);

  error = gd_error(D);
  CHECKI(error, 0);
  CHECKI(n, 8);

  for (i = 0; i < 8; ++i)
    CHECKIi(i, c[i], (0x50000028 + i * 0x02000001)  * (2 * (i % 2) - 1));

  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);
  unlink(filedirzip);

  return r;
#else
  return 77;
#endif
}
