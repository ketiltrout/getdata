/* Copyright (C) 2008-2011, 2017 D.V. Wiebe
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
/* Attempt a write following a read via the legacy API -- this requires
 * closing and then re-opening the legacy dirfile */
#include "test.h"

int main(void)
{
#ifndef GD_LEGACY_API
  return 77; /* skipped */
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  uint8_t c[8];
  int i, get_error, put_error, n, r = 0;

  memset(c, 0, 8);
  rmdirfile();
  mkdir(filedir, 0700);

  for (i = 0; i < 8; ++i)
    c[i] = (uint8_t)(40 + i);

  MAKEFORMATFILE(format, "data RAW UINT8 8\n");
  MAKEDATAFILE(data, unsigned char, i, 256);

  GetData(filedir, "data", 5, 0, 1, 0, 'c', c, &get_error);
  n = PutData(filedir, "data", 5, 0, 1, 0, 'c', c, &put_error);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(get_error,0);
  CHECKI(put_error,0);
  CHECKI(n,8);

  return r;
#endif
}
