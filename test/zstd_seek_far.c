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

int main(void)
{
#ifndef TEST_ZSTD
  return 77;
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  const char *zstddata = "dirfile/data.zst";
  char command[4096];
  int error, r = 0;
  off_t n;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "data RAW UINT16 8\n");
  MAKEDATAFILE(data, uint16_t, i, 256);

  /* compress */
  snprintf(command, 4096, "\"%s\" -f --rm %s > %s", ZSTD, data, NULL_DEVICE);
  if (gd_system(command))
    return 1;

#ifdef USE_ZSTD
  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
#else
  D = gd_open(filedir, GD_RDONLY);
#endif
  n = gd_seek(D, "data", 500, 0, GD_SEEK_SET);
  error = gd_error(D);

  gd_discard(D);

  unlink(zstddata);
  unlink(format);
  rmdir(filedir);

#ifdef USE_ZSTD
  CHECKI(error, 0);

  /* Either of these values is fine with us */
  if (n != 256)
    CHECKI(n, 4000);
#else
  CHECKI(error, GD_E_UNSUPPORTED);
  CHECKI(n, GD_E_UNSUPPORTED);
#endif

  return r;
#endif
}
