/* Copyright (C) 2011, 2013, 2017 D.V. Wiebe
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
#ifndef TEST_ZZIP
  return 77;
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  const char *rawzip = "dirfile/raw.zip";
  uint16_t c[8];
  char command[4096];
  int n, error, r = 0;
#ifdef USE_ZZIP
  int i;
#endif
  DIRFILE *D;

  memset(c, 0, 8);
  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "data RAW UINT16 8\n");
  MAKEDATAFILE(data, uint16_t, i, 256);

  /* compress */
  chdir(filedir);
  snprintf(command, 4096, "%s raw data > /dev/null", ZIP);
  if (gd_system(command))
    return 1;
  chdir("..");
  unlink(data);

#ifdef USE_ZZIP
  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
#else
  D = gd_open(filedir, GD_RDONLY);
#endif
  n = gd_getdata(D, "data", 5, 0, 1, 0, GD_UINT16, c);
  error = gd_error(D);

  gd_discard(D);

  unlink(rawzip);
  unlink(format);
  rmdir(filedir);

#ifdef USE_ZZIP
  CHECKI(error, 0);
  CHECKI(n, 8);
  for (i = 0; i < 8; ++i)
    CHECKIi(i,c[i], 40 + i);
#else
  CHECKI(error, GD_E_UNKNOWN_ENCODING);
  CHECKI(n,0);
#endif

  return r;
#endif
}
