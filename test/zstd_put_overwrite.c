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
/* Test overwrite within the in-progress (unflushed) write_buf.
 * Uses 16-byte frames so 8 samples stay buffered; overwrites 2 of them,
 * then overwrites 4 more extending past the buffered end.  Both patches
 * happen in memory: the frame must stay open (nothing written to disk). */
#include "test.h"

int main(void)
{
#if ! (defined TEST_ZSTD) || ! (defined USE_ZSTD)
  return 77;
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data.zst";
  /* 8 samples written first, then 2 overwritten at offset 2 */
  uint8_t c[8]  = { 10, 11, 12, 13, 14, 15, 16, 17 };
  uint8_t ov[2] = { 99, 98 };
  uint8_t ox[4] = { 50, 51, 52, 53 };
  uint8_t d[10];
  struct stat statbuf;
  int i, n1, n2, n3, m1, m2, s1, e1, e2, e3, e4, e5, e6, unlink_data, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  /* 16-byte frames, spf=1, UINT8: 8 samples fit in one frame (< 16 bytes) */
  MAKEFORMATFILE(format, "/ENCODING zstd size=16\ndata RAW UINT8 1\n");

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);

  /* Write 8 samples at frame 0 -- stays in write_buf, not yet committed */
  n1 = gd_putdata(D, "data", 0, 0, 0, 8, GD_UINT8, c);
  e1 = gd_error(D);
  CHECKI(e1, GD_E_OK);
  CHECKI(n1, 8);

  /* Overwrite 2 samples at frame 2: write_pos=2 < current_end=8
   * → patched in write_buf without committing the frame */
  n2 = gd_putdata(D, "data", 2, 0, 0, 2, GD_UINT8, ov);
  e2 = gd_error(D);
  CHECKI(e2, GD_E_OK);
  CHECKI(n2, 2);

  /* Overwrite 4 samples at frame 6, extending the open frame to 10 */
  n3 = gd_putdata(D, "data", 6, 0, 0, 4, GD_UINT8, ox);
  e3 = gd_error(D);
  CHECKI(e3, GD_E_OK);
  CHECKI(n3, 4);

  /* Both writes landed in the open frame: nothing on disk yet */
  s1 = stat(data, &statbuf);
  CHECKI(s1, 0);
  CHECKI((int)statbuf.st_size, 0);

  /* Read back without closing */
  m1 = gd_getdata(D, "data", 0, 0, 0, 10, GD_UINT8, d);
  e4 = gd_error(D);
  CHECKI(e4, GD_E_OK);
  CHECKI(m1, 10);
  for (i = 0; i < 10; ++i) {
    uint8_t expect = (i == 2) ? 99 : (i == 3) ? 98 :
      (i >= 6) ? ox[i - 6] : c[i];
    CHECKIi(i, d[i], expect);
  }

  e5 = gd_close(D);
  CHECKI(e5, 0);

  /* Reopen and verify the flushed frame */
  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  m2 = gd_getdata(D, "data", 0, 0, 0, 10, GD_UINT8, d);
  e6 = gd_error(D);
  CHECKI(e6, GD_E_OK);
  CHECKI(m2, 10);
  for (i = 0; i < 10; ++i) {
    uint8_t expect = (i == 2) ? 99 : (i == 3) ? 98 :
      (i >= 6) ? ox[i - 6] : c[i];
    CHECKIi(i, d[i], expect);
  }

  gd_discard(D);

  unlink_data = unlink(data);
  unlink(format);
  rmdir(filedir);
  CHECKI(unlink_data, 0);

  return r;
#endif
}
