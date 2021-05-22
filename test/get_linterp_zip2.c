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
/* Attempt to read LINTERP */
#include "test.h"

int main(void)
{
#ifdef HAVE_ZZIP_LIB_H
  const char *filedir = "dirfile";
  const char *filedirzip = "dirfile.zip";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  const char *table = "dirfile/table";
  /* DEFLATE compress table but STORE data and format file*/
  const char *command = "zip -jqn table dirfile.zip dirfile/format dirfile/data dirfile/table";
  unsigned char c = 0;
  int i, n, error, r = 0;
  DIRFILE *D;
  FILE *t;

  rmdirfile();
  unlink(filedirzip);
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "/ENCODING none\nlinterp LINTERP data table\ndata RAW UINT8 1\n");
  MAKEDATAFILE(data, unsigned char, i, 64);

  t = fopen(table, "wt");
  for (i = 0; i < 2 * GD_LUT_CHUNK; ++i)
    fprintf(t, "%i %i\n", i * 6, i * 12);
  fclose(t);

  if (gd_system(command))
    return 1;

  D = gd_open(filedirzip, GD_RDONLY | GD_VERBOSE);
  n = gd_getdata(D, "linterp", 5, 0, 1, 0, GD_UINT8, &c);
  error = gd_error(D);

  gd_discard(D);

  unlink(table);
  unlink(data);
  unlink(format);
  rmdir(filedir);
  unlink(filedirzip);

  CHECKI(error, 0);
  CHECKI(n, 1);
  CHECKU(c, 10);

  return r;
#else
  return 77;
#endif
}
