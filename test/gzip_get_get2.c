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
/* Attempt to read UINT8 */
#include "test.h"

int main(void)
{
#if !defined USE_GZIP || !defined TEST_GZIP
  return 77; /* skip test */
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  const char *gzipdata = "dirfile/data.gz";
  uint16_t c1[8], c2[8];
  char command[4096];
  int i, n1, error1, n2, error2, r = 0;
  DIRFILE *D;

  memset(c1, 0, 16);
  memset(c2, 0, 16);
  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "data RAW UINT16 8\n");
  MAKEDATAFILE(data, uint16_t, i, 256);

  /* compress */
  snprintf(command, 4096, "%s -f %s > /dev/null", GZIP, data);
  if (gd_system(command))
    return 1;

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n1 = gd_getdata(D, "data", 0, 0, 1, 0, GD_UINT16, c1);
  error1 = gd_error(D);
  n2 = gd_getdata(D, "data", 0, 0, 1, 0, GD_UINT16, c2);
  error2 = gd_error(D);
  gd_discard(D);

  unlink(gzipdata);
  unlink(format);
  rmdir(filedir);

  CHECKI(error1, 0);
  CHECKI(error2, 0);
  CHECKI(n1, 8);
  CHECKI(n2, 8);
  for (i = 0; i < 8; ++i) {
    CHECKUi(i,c1[i], i);
    CHECKUi(i,c2[i], i);
  }

  return r;
#endif
}
