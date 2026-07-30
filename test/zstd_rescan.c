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

/* A read-only handle's frame index goes stale when a writer rewrites the
 * file in place (a backward write moves every frame after the rewrite
 * point).  The reader must detect this and rebuild its index rather than
 * failing. */
int main(void)
{
#ifndef USE_ZSTD
  return 77;
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *zstddata = "dirfile/data.zst";
  uint8_t d[64], c[64];
  int i, n, error, r = 0;
  DIRFILE *W, *R;

  rmdirfile();
  mkdir(filedir, 0700);

  /* small frames, so the data spans several */
  MAKEFORMATFILE(format, "data RAW UINT8 1\n/ENCODING zstd size=16\n");

  /* write compressible data */
  memset(d, 0, sizeof(d));
  W = gd_open(filedir, GD_RDWR);
  gd_putdata(W, "data", 0, 0, 0, 64, GD_UINT8, d);
  gd_flush(W, "data");

  /* a reader builds its index from these frames */
  R = gd_open(filedir, GD_RDONLY);
  n = (int)gd_getdata(R, "data", 0, 0, 0, 64, GD_UINT8, c);
  CHECKI(n, 64);

  /* rewrite everything in place with different (larger) frames */
  for (i = 0; i < 64; ++i)
    d[i] = (uint8_t)(i * 37 + 11);
  gd_putdata(W, "data", 0, 0, 0, 64, GD_UINT8, d);
  gd_flush(W, "data");

  /* the reader's index is now stale; the read must recover, not fail */
  memset(c, 0, sizeof(c));
  n = (int)gd_getdata(R, "data", 0, 0, 0, 64, GD_UINT8, c);
  error = gd_error(R);

  CHECKI(n, 64);
  CHECKI(error, GD_E_OK);
  for (i = 0; i < 64; ++i)
    CHECKUi(i, c[i], d[i]);

  /* a same-size in-place rewrite leaves no error to trip over: the
   * reader's cached frame must be revalidated, not trusted */
  for (i = 0; i < 64; ++i)
    d[i] = (uint8_t)(i * 73 + 5);
  gd_putdata(W, "data", 0, 0, 0, 64, GD_UINT8, d);
  gd_flush(W, "data");

  memset(c, 0, sizeof(c));
  n = (int)gd_getdata(R, "data", 0, 0, 0, 64, GD_UINT8, c);
  error = gd_error(R);

  CHECKI(n, 64);
  CHECKI(error, GD_E_OK);
  for (i = 0; i < 64; ++i)
    CHECKUi(i, c[i], d[i]);

  /* rewrite only the tail: the reader's index prefix stays valid, and
   * recovery must pick up from the rewrite point */
  for (i = 32; i < 64; ++i)
    d[i] = (uint8_t)(i * 11 + 3);
  gd_putdata(W, "data", 0, 32, 0, 32, GD_UINT8, d + 32);
  gd_flush(W, "data");

  memset(c, 0, sizeof(c));
  n = (int)gd_getdata(R, "data", 0, 0, 0, 64, GD_UINT8, c);
  error = gd_error(R);

  CHECKI(n, 64);
  CHECKI(error, GD_E_OK);
  for (i = 0; i < 64; ++i)
    CHECKUi(i, c[i], d[i]);

  gd_discard(R);
  gd_discard(W);

  unlink(zstddata);
  unlink(format);
  rmdir(filedir);

  return r;
#endif
}
