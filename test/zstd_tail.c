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

/* A reader must see frames committed by a writer after the reader opened
 * the dirfile (the reader's frame index is extended on demand). */
int main(void)
{
#ifndef USE_ZSTD
  return 77;
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *zstddata = "dirfile/data.zst";
  uint16_t d[8], c[8];
  int i, n1, n2, error, r = 0;
  DIRFILE *W, *R;

  for (i = 0; i < 8; ++i)
    d[i] = (uint16_t)(i + 1);
  memset(c, 0, sizeof(c));

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "data RAW UINT16 8\n/ENCODING zstd\n");

  /* writer: commit one frame */
  W = gd_open(filedir, GD_RDWR);
  gd_putdata(W, "data", 0, 0, 1, 0, GD_UINT16, d);
  gd_flush(W, "data");

  /* independent reader sees it */
  R = gd_open(filedir, GD_RDONLY);
  n1 = gd_getdata(R, "data", 0, 0, 1, 0, GD_UINT16, c);
  CHECKI(n1, 8);

  /* writer appends and commits another frame; the previously-opened reader
   * must see it too */
  gd_putdata(W, "data", 1, 0, 1, 0, GD_UINT16, d);
  gd_flush(W, "data");

  n2 = gd_getdata(R, "data", 1, 0, 1, 0, GD_UINT16, c);
  error = gd_error(R);

  CHECKI(n2, 8);
  CHECKI(error, GD_E_OK);
  for (i = 0; i < 8; ++i)
    CHECKUi(i, c[i], d[i]);

  gd_discard(R);
  gd_discard(W);

  unlink(zstddata);
  unlink(format);
  rmdir(filedir);

  return r;
#endif
}
