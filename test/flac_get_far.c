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
  uint16_t c[8];
  char command[4096];
  int n, error, r = 0;
  DIRFILE *D;

  memset(c, 0, 8);
  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "data RAW UINT16 8\n");
  MAKEDATAFILE(data, uint16_t, i, 256);

#ifdef WORDS_BIGENDIAN
#define ENDIANNESS "--endian=big"
#else
#define ENDIANNESS "--endian=little"
#endif

  snprintf(command, 4096,
      "%s " ENDIANNESS " --silent --sample-rate=1 --channels=1 --bps=16 "
      "--sign=signed --delete-input-file %s >/dev/null 2>/dev/null", FLAC,
      data);
  if (gd_system(command))
    return 1;

  D = gd_open(filedir, GD_RDONLY);
  n = gd_getdata(D, "data", 1000, 0, 1, 0, GD_UINT16, c);
  error = gd_error(D);

  gd_discard(D);

  unlink(flacdata);
  unlink(format);
  rmdir(filedir);

#ifdef USE_FLAC
  CHECKI(error, 0);
#else
  CHECKI(error, GD_E_UNSUPPORTED);
#endif
  CHECKI(n, 0);

  return r;
#endif
}
