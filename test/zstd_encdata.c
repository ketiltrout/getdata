/* Copyright (C) 2026 G. Smecher
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

/* A malformed zstd frame size parameter must fail both reads and writes */
int main(void)
{
#ifndef USE_ZSTD
  return 77;
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *zstddata = "dirfile/data.zst";
  uint16_t d[8], c[8];
  int i, n, error1, error2, r = 0;
  DIRFILE *D;

  for (i = 0; i < 8; ++i)
    d[i] = (uint16_t)(i + 1);
  memset(c, 0, sizeof(c));

  rmdirfile();
  mkdir(filedir, 0700);

  /* create a valid zstd-encoded data file, with a well-formed frame size
   * and compression level */
  MAKEFORMATFILE(format, "data RAW UINT16 8\n/ENCODING zstd size=65536,level=3\n");

  D = gd_open(filedir, GD_RDWR);
  gd_putdata(D, "data", 0, 0, 1, 0, GD_UINT16, d);
  n = gd_getdata(D, "data", 0, 0, 1, 0, GD_UINT16, c);
  CHECKI(n, 8);
  for (i = 0; i < 8; ++i)
    CHECKUi(i, c[i], d[i]);
  gd_close(D);

  /* a malformed frame size fails both reads and writes */
  unlink(format);
  MAKEFORMATFILE(format, "data RAW UINT16 8\n/ENCODING zstd bogus\n");

  D = gd_open(filedir, GD_RDWR);
  gd_getdata(D, "data", 0, 0, 1, 0, GD_UINT16, c);
  error1 = gd_error(D);
  gd_putdata(D, "data", 1, 0, 1, 0, GD_UINT16, d);
  error2 = gd_error(D);
  gd_discard(D);

  CHECKI(error1, GD_E_IO);
  CHECKI(error2, GD_E_IO);

  /* an out-of-range compression level also fails */
  unlink(format);
  MAKEFORMATFILE(format, "data RAW UINT16 8\n/ENCODING zstd size=65536,level=123456\n");

  D = gd_open(filedir, GD_RDWR);
  gd_getdata(D, "data", 0, 0, 1, 0, GD_UINT16, c);
  error1 = gd_error(D);
  gd_discard(D);

  CHECKI(error1, GD_E_IO);

  unlink(zstddata);
  unlink(format);
  rmdir(filedir);

  return r;
#endif
}
