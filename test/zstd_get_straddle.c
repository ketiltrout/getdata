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
/* Test read straddling a committed frame and the unflushed write_buf.
 * 16-byte frames: write 20 samples (first 16 committed, last 4 in write_buf),
 * then read 8 samples starting at offset 12 -- spans both regions. */
#include "test.h"

int main(void)
{
#if ! (defined TEST_ZSTD) || ! (defined USE_ZSTD)
  return 77;
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data.zst";
  uint8_t c[20];
  uint8_t d[8];
  int i, n, m, e1, e2, e3, unlink_data, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  /* 16-byte frames, spf=1, UINT8 */
  MAKEFORMATFILE(format, "/ENCODING zstd size=16\ndata RAW UINT8 1\n");

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);

  for (i = 0; i < 20; ++i)
    c[i] = (uint8_t)i;

  /* Write 20 samples: first 16 fill and commit a frame, last 4 sit in write_buf */
  n = gd_putdata(D, "data", 0, 0, 0, 20, GD_UINT8, c);
  e1 = gd_error(D);
  CHECKI(e1, GD_E_OK);
  CHECKI(n, 20);

  /* Read 8 samples starting at offset 12: bytes 12-15 from committed frame,
   * bytes 16-19 from write_buf -- exercises the straddle path */
  m = gd_getdata(D, "data", 12, 0, 0, 8, GD_UINT8, d);
  e2 = gd_error(D);
  CHECKI(e2, GD_E_OK);
  CHECKI(m, 8);
  for (i = 0; i < 8; ++i)
    CHECKIi(i, d[i], (uint8_t)(12 + i));

  e3 = gd_close(D);
  CHECKI(e3, 0);

  unlink_data = unlink(data);
  unlink(format);
  rmdir(filedir);
  CHECKI(unlink_data, 0);

  return r;
#endif
}
