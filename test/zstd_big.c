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
/* Cross the 2^32 byte/sample boundary: a marker written just below the
 * boundary extends past it, with the preceding 4 GiB supplied by the
 * encoding's zero-fill gap path.  Zeros compress well enough that this is
 * cheap in both time and disk, while exercising 64-bit offset arithmetic
 * in the frame index, gap fill, and straddled reads. */
#include "test.h"

int main(void)
{
#if ! (defined TEST_ZSTD) || ! (defined USE_ZSTD)
  return 77;
#else
  const int64_t boundary = ((int64_t)1) << 32;
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data.zst";
  uint8_t mark[16], c[32];
  int i, n1, m1, m2, e1, e2, e3, e4, unlink_data, r = 0;
  int64_t nf1, nf2;
  DIRFILE *D;

  /* needs a 64-bit off_t to address samples past 2^32 */
  if (sizeof(gd_off64_t) < 8 || sizeof(off_t) < 8)
    return 77;

  for (i = 0; i < 16; ++i)
    mark[i] = (uint8_t)(0xA0 + i);
  memset(c, 0, sizeof(c));

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "data RAW UINT8 1\n/ENCODING zstd\n");

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);

  /* write a 16-byte marker straddling the 4 GiB boundary; everything
   * before it is zero-filled */
  n1 = gd_putdata(D, "data", 0, (off_t)(boundary - 8), 0, 16, GD_UINT8,
      mark);
  e1 = gd_error(D);
  CHECKI(e1, GD_E_OK);
  CHECKI(n1, 16);

  nf1 = gd_nframes(D);
  CHECKI(nf1, boundary + 8);

  /* straddled read from the writer's own handle: 8 zeros, then the marker,
   * then EOF */
  m1 = gd_getdata(D, "data", 0, (off_t)(boundary - 16), 0, 32, GD_UINT8, c);
  e2 = gd_error(D);
  CHECKI(e2, GD_E_OK);
  CHECKI(m1, 24);
  for (i = 0; i < 8; ++i)
    CHECKIi(i, c[i], 0);
  for (i = 0; i < 16; ++i)
    CHECKIi(8 + i, c[8 + i], mark[i]);

  e3 = gd_close(D);
  CHECKI(e3, 0);

  /* reopen read-only: index is rebuilt by scanning the frame headers */
  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  nf2 = gd_nframes(D);
  CHECKI(nf2, boundary + 8);

  memset(c, 0, sizeof(c));
  m2 = gd_getdata(D, "data", 0, (off_t)(boundary - 16), 0, 32, GD_UINT8, c);
  e4 = gd_error(D);
  CHECKI(e4, GD_E_OK);
  CHECKI(m2, 24);
  for (i = 0; i < 8; ++i)
    CHECKIi(i, c[i], 0);
  for (i = 0; i < 16; ++i)
    CHECKIi(8 + i, c[8 + i], mark[i]);

  gd_discard(D);

  unlink_data = unlink(data);
  unlink(format);
  rmdir(filedir);
  CHECKI(unlink_data, 0);

  return r;
#endif
}
