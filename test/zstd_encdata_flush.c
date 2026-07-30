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
/* The zstd encoding parameters (/ENCODING zstd size=...,level=...) must
 * survive a metadata rewrite. */
#include "test.h"

int main(void)
{
#if ! (defined TEST_ZSTD) || ! (defined USE_ZSTD)
  return 77;
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data.zst";
  uint16_t d[8], c[8];
  char line[4096];
  size_t nb;
  int i, n, e1, e2, e3, m, unlink_data, r = 0;
  DIRFILE *D;
  FILE *f;

  for (i = 0; i < 8; ++i)
    d[i] = (uint16_t)(i + 1);
  memset(c, 0, sizeof(c));
  memset(line, 0, sizeof(line));

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
      "data RAW UINT16 8\n/ENCODING zstd size=65536,level=3\n");

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  n = gd_putdata(D, "data", 0, 0, 1, 0, GD_UINT16, d);
  e1 = gd_error(D);
  CHECKI(e1, GD_E_OK);
  CHECKI(n, 8);

  /* force a metadata rewrite */
  m = gd_rewrite_fragment(D, 0);
  CHECKI(m, 0);

  e2 = gd_close(D);
  CHECKI(e2, 0);

  /* the rewritten format file must retain the encoding parameters */
  f = fopen(format, "r");
  CHECKPN(f);
  if (f != NULL) {
    nb = fread(line, 1, sizeof(line) - 1, f);
    line[nb] = 0;
    fclose(f);
    CHECKSS(line, "/ENCODING zstd size=65536,level=3");
  }

  /* and the dirfile must still be readable */
  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n = gd_getdata(D, "data", 0, 0, 1, 0, GD_UINT16, c);
  e3 = gd_error(D);
  CHECKI(e3, GD_E_OK);
  CHECKI(n, 8);
  for (i = 0; i < 8; ++i)
    CHECKUi(i, c[i], d[i]);

  gd_discard(D);

  unlink_data = unlink(data);
  unlink(format);
  rmdir(filedir);
  CHECKI(unlink_data, 0);

  return r;
#endif
}
