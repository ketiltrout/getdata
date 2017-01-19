/* Copyright (C) 2015, 2017 D. V. Wiebe
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
#ifndef TEST_FLAC
  return 77;
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  const char *flacdata = "dirfile/data.flac";
  uint32_t c[8];
  char command[4096];
  int n, error, r = 0;
#ifdef USE_FLAC
  int i;
#endif
  DIRFILE *D;

  memset(c, 0, 8);
  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "data RAW UINT32 8\n");
#ifdef WORDS_BIGENDIAN
  MAKEDATAFILE(data, uint32_t, 0x01020304LU * i, 256);
#else
  MAKEDATAFILE(data, uint32_t, 0x04030201LU * i, 256);
#endif

  /* encode */
  snprintf(command, 4096,
      "%s --endian=big --silent --sample-rate=1 --channels=2 --bps=16 "
      "--sign=signed --delete-input-file %s >/dev/null 2>/dev/null", FLAC,
      data);
  if (gd_system(command))
    return 1;

#ifdef USE_FLAC
  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE | GD_BIG_ENDIAN);
#else
  D = gd_open(filedir, GD_RDONLY | GD_BIG_ENDIAN);
#endif
  n = gd_getdata(D, "data", 0, 0, 1, 0, GD_UINT32, c);
  error = gd_error(D);

#ifdef USE_FLAC
  CHECKI(error, 0);
  CHECKI(n, 8);

  for (i = 0; i < 8; ++i)
    CHECKXi(i,c[i], 0x01020304LU * i);
#else
  CHECKI(error, GD_E_UNSUPPORTED);
  CHECKI(n, 0);
#endif

  gd_discard(D);

  unlink(flacdata);
  unlink(format);
  rmdir(filedir);

  return r;
#endif
}
