/* Copyright (C) 2015 D. V. Wiebe
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

uint32_t d[GD_BUFFER_SIZE];

int main(void)
{
#if !defined(USE_GZIP) || defined(NO_LARGE_TESTS)
  return 77;
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data.gz";
  int i, e1, e2, r = 0;
  size_t n1, n2;
  off_t nf;
  DIRFILE *D;

  for (i = 0; i < GD_BUFFER_SIZE; ++i)
    d[i] = i;

  rmdirfile();

  D = gd_open(filedir,
      GD_RDWR | GD_GZIP_ENCODED | GD_CREAT | GD_EXCL | GD_VERBOSE);

  gd_add_raw(D, "data", GD_UINT32, 1, 0);

  n1 = gd_putdata(D, "data", 0, 0, 0, GD_BUFFER_SIZE, GD_UINT32, d);
  CHECKU(n1, GD_BUFFER_SIZE);

  e1 = gd_error(D);
  CHECKI(e1, GD_E_OK);

  gd_close(D);

  for (i = 0; i < GD_BUFFER_SIZE; ++i)
    d[i] = i + 0x100000;

  D = gd_open(filedir, GD_RDWR | GD_GZIP_ENCODED | GD_VERBOSE);

  n2 = gd_putdata(D, "data", 0, GD_BUFFER_SIZE, 0, GD_BUFFER_SIZE, GD_UINT32,
      d);
  CHECKU(n2, GD_BUFFER_SIZE);

  e2 = gd_error(D);
  CHECKI(e2, GD_E_OK);

  nf = gd_nframes(D);
  CHECKU(nf, GD_BUFFER_SIZE * 2);

  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  return r;
#endif
}
