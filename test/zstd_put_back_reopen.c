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
/* Test _GD_ZstdBuildIndex on a file that has been rewritten by RewriteSpan.
 * Writes 40 samples, does a backward write at frame 0, closes, reopens,
 * and reads back to verify the scan correctly indexes the rewritten file. */
#include "test.h"

int main(void)
{
#if ! (defined TEST_ZSTD) || ! (defined USE_ZSTD)
  return 77;
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data.zst";
  uint8_t c[40], ov[8], d[40];
  int i, n1, n2, m, e1, e2, e3, e4, unlink_data, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  /* 16-byte frames, spf=1, UINT8: 40 samples = 2 full frames + 8 in write_buf */
  MAKEFORMATFILE(format, "/ENCODING zstd size=16\ndata RAW UINT8 1\n");

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);

  for (i = 0; i < 40; ++i)
    c[i] = (uint8_t)(40 + i);
  for (i = 0; i < 8; ++i)
    ov[i] = (uint8_t)(10 + i);

  /* Write 40 samples: commits multiple frames */
  n1 = gd_putdata(D, "data", 0, 0, 0, 40, GD_UINT8, c);
  e1 = gd_error(D);
  CHECKI(e1, GD_E_OK);
  CHECKI(n1, 40);

  /* Backward write at frame 0: triggers RewriteSpan on committed frames */
  n2 = gd_putdata(D, "data", 0, 0, 0, 8, GD_UINT8, ov);
  e2 = gd_error(D);
  CHECKI(e2, GD_E_OK);
  CHECKI(n2, 8);

  e3 = gd_close(D);
  CHECKI(e3, 0);

  /* Reopen read-only: exercises BuildIndex on the rewritten file */
  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  m = gd_getdata(D, "data", 0, 0, 0, 40, GD_UINT8, d);
  e4 = gd_error(D);
  CHECKI(e4, GD_E_OK);
  CHECKI(m, 40);

  /* First 8 samples overwritten */
  for (i = 0; i < 8; ++i)
    CHECKIi(i, d[i], ov[i]);
  /* Remaining 32 samples unchanged */
  for (i = 8; i < 40; ++i)
    CHECKIi(i, d[i], c[i]);

  gd_discard(D);

  unlink_data = unlink(data);
  unlink(format);
  rmdir(filedir);
  CHECKI(unlink_data, 0);

  return r;
#endif
}
