/* Copyright (C) 2015-2026 D. V. Wiebe
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

uint32_t d[100];

int main(void)
{
#ifndef USE_ENC
  return 77;
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *encdata = "dirfile/data" ENC_SUFFIX;
  int i, e1, e2, r = 0;
  size_t n1, n2;
  off_t nf1, nf2, nf3;
  DIRFILE *D;

  for (i = 0; i < 100; ++i)
    d[i] = i;

  rmdirfile();

  D = gd_open(filedir,
      GD_RDWR | ENC_ENCODING | GD_CREAT | GD_EXCL | GD_VERBOSE);

  gd_add_raw(D, "data", GD_UINT32, 1, 0);

  n1 = gd_putdata(D, "data", 0, 0, 0, 100, GD_UINT32, d);
  CHECKU(n1, 100);

  e1 = gd_error(D);
  CHECKI(e1, GD_E_OK);

  nf1 = gd_nframes(D);
  CHECKU(nf1, 100);

  /* The first gd_nframes() may have finalised an out-of-place temporary
   * file.  A second call must not try to finalise it again (a stale
   * GD_FILE_WRITE in file[0].mode used to send a NULL temporary name to
   * gd_RenameAt) */
  nf2 = gd_nframes(D);
  CHECKU(nf2, 100);

  e2 = gd_error(D);
  CHECKI(e2, GD_E_OK);

  gd_close(D);

  D = gd_open(filedir, GD_RDWR | ENC_ENCODING | GD_VERBOSE);

  n2 = gd_putdata(D, "data", 0, 100, 0, 100, GD_UINT32, d);
  CHECKU(n2, 100);

  e2 = gd_error(D);
  CHECKI(e2, GD_E_OK);

  nf3 = gd_nframes(D);
  CHECKU(nf3, 200);

  gd_discard(D);

  unlink(encdata);
  unlink(format);
  rmdir(filedir);

  return r;
#endif
}
