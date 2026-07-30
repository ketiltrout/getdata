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
/* Test zero-fill gap: write at frame 0, then skip ahead to frame 20,
 * exercising the current_end < write_pos path in _GD_ZstdWrite. */
#include "test.h"

int main(void)
{
#if ! (defined TEST_ZSTD) || ! (defined USE_ZSTD)
  return 77;
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data.zst";
  uint8_t a[4] = { 1, 2, 3, 4 };
  uint8_t b[4] = { 5, 6, 7, 8 };
  uint8_t d[24];
  int i, n1, n2, m1, m2, e1, e2, e3, e4, e5, unlink_data, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  /* 16-byte frames, spf=1, UINT8 */
  MAKEFORMATFILE(format, "/ENCODING zstd size=16\ndata RAW UINT8 1\n");

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);

  /* Write 4 samples at frame 0 */
  n1 = gd_putdata(D, "data", 0, 0, 0, 4, GD_UINT8, a);
  e1 = gd_error(D);
  CHECKI(e1, GD_E_OK);
  CHECKI(n1, 4);

  /* Write 4 samples at frame 20, skipping 16 bytes -- zero-fill gap */
  n2 = gd_putdata(D, "data", 20, 0, 0, 4, GD_UINT8, b);
  e2 = gd_error(D);
  CHECKI(e2, GD_E_OK);
  CHECKI(n2, 4);

  /* Read back all 24 samples without closing */
  m1 = gd_getdata(D, "data", 0, 0, 0, 24, GD_UINT8, d);
  e3 = gd_error(D);
  CHECKI(e3, GD_E_OK);
  CHECKI(m1, 24);
  for (i = 0; i < 4; ++i)
    CHECKIi(i, d[i], a[i]);
  for (i = 4; i < 20; ++i)
    CHECKIi(i, d[i], 0);
  for (i = 0; i < 4; ++i)
    CHECKIi(20 + i, d[20 + i], b[i]);

  e4 = gd_close(D);
  CHECKI(e4, 0);

  /* Reopen and verify scan on gapped file */
  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  m2 = gd_getdata(D, "data", 0, 0, 0, 24, GD_UINT8, d);
  e5 = gd_error(D);
  CHECKI(e5, GD_E_OK);
  CHECKI(m2, 24);
  for (i = 0; i < 4; ++i)
    CHECKIi(i, d[i], a[i]);
  for (i = 4; i < 20; ++i)
    CHECKIi(i, d[i], 0);
  for (i = 0; i < 4; ++i)
    CHECKIi(20 + i, d[20 + i], b[i]);

  gd_discard(D);

  unlink_data = unlink(data);
  unlink(format);
  rmdir(filedir);
  CHECKI(unlink_data, 0);

  return r;
#endif
}
