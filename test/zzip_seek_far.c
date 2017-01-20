/* Copyright (C) 2015, 2017 D.V. Wiebe
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
#if !defined TEST_ZZIP || !defined USE_ZZIP
  return 77;
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  const char *rawzip = "dirfile/raw.zip";
  char command[4096];
  int n, error, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "data RAW UINT16 8\n");
  MAKEDATAFILE(data, uint16_t, i, 256);

  chdir(filedir);
  snprintf(command, 4096, "%s raw data > /dev/null", ZIP);
  if (gd_system(command))
    return 1;
  chdir("..");
  unlink(data);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);

  n = gd_seek(D, "data", 500, 0, GD_SEEK_SET);
  CHECKI(n, 256);

  error = gd_error(D);
  CHECKI(error, 0);

  gd_discard(D);

  unlink(rawzip);
  unlink(format);
  rmdir(filedir);

  return r;
#endif
}
