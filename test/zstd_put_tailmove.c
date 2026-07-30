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
/* Backward writes recompress only the overlapping frames and relocate the
 * unchanged frames after them.  Patch mid-file frames first with
 * highly-compressible data (the replacement frames shrink; the tail slides
 * down) and then with poorly-compressible data (the replacement frames
 * grow; the tail slides up), verifying the relocated tail both times and
 * again after a reopen. */
#include "test.h"

#define NS 32768          /* 8 frames of 4096 */
#define PATCH_LO 4096     /* patch covers frames 1 and 2 */
#define PATCH_HI 12288

static uint8_t model[NS];
static uint8_t check[NS];

static int verify(DIRFILE *D, int idx)
{
  int i, n, e, r = 0;

  memset(check, 0, NS);
  n = gd_getdata(D, "data", 0, 0, 0, NS, GD_UINT8, check);
  e = gd_error(D);
  CHECKIi(idx, e, GD_E_OK);
  CHECKIi(idx, n, NS);
  for (i = 0; i < NS; ++i)
    if (check[i] != model[i]) {
      /* report the first mismatch only */
      CHECKIi(idx * NS + i, check[i], model[i]);
      break;
    }

  return r;
}

int main(void)
{
#if ! (defined TEST_ZSTD) || ! (defined USE_ZSTD)
  return 77;
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data.zst";
  uint32_t lcg = 1;
  int i, n, e, unlink_data, r = 0;
  DIRFILE *D;

  /* a poorly-compressible pattern */
  for (i = 0; i < NS; ++i) {
    lcg = lcg * 1664525 + 1013904223;
    model[i] = (uint8_t)(lcg >> 24);
  }

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "data RAW UINT8 1\n/ENCODING zstd size=4096\n");

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  n = gd_putdata(D, "data", 0, 0, 0, NS, GD_UINT8, model);
  e = gd_error(D);
  CHECKI(e, GD_E_OK);
  CHECKI(n, NS);

  /* shrink the patched frames: zeros compress far better than the LCG
   * pattern, so the tail slides down */
  memset(model + PATCH_LO, 0, PATCH_HI - PATCH_LO);
  n = gd_putdata(D, "data", 0, PATCH_LO, 0, PATCH_HI - PATCH_LO, GD_UINT8,
      model + PATCH_LO);
  e = gd_error(D);
  CHECKI(e, GD_E_OK);
  CHECKI(n, PATCH_HI - PATCH_LO);
  r |= verify(D, 1);

  /* grow them again: a fresh poorly-compressible pattern, so the tail
   * slides back up */
  for (i = PATCH_LO; i < PATCH_HI; ++i) {
    lcg = lcg * 1664525 + 1013904223;
    model[i] = (uint8_t)(lcg >> 24);
  }
  n = gd_putdata(D, "data", 0, PATCH_LO, 0, PATCH_HI - PATCH_LO, GD_UINT8,
      model + PATCH_LO);
  e = gd_error(D);
  CHECKI(e, GD_E_OK);
  CHECKI(n, PATCH_HI - PATCH_LO);
  r |= verify(D, 2);

  e = gd_close(D);
  CHECKI(e, 0);

  /* reopen: the scan must parse the relocated frames from scratch */
  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  r |= verify(D, 3);
  gd_discard(D);

  unlink_data = unlink(data);
  unlink(format);
  rmdir(filedir);
  CHECKI(unlink_data, 0);

  return r;
#endif
}
