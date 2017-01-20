/* Copyright (C) 2016, 2017 D.V. Wiebe
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
#if !defined USE_LZMA || !defined TEST_LZMA || defined NO_LARGE_TESTS
  return 77;
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data.xz";
  uint32_t c = 3;
  int r = 0, e;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  D = gd_open(filedir, GD_RDWR | GD_LZMA_ENCODED | GD_LITTLE_ENDIAN
      | GD_VERBOSE | GD_CREAT | GD_EXCL);
  gd_add_spec(D, "data RAW UINT32 1", 0);
  gd_putdata(D, "data", 2000000, 0, 0, 1, GD_UINT32, &c);
  e = gd_error(D);
  CHECKIi(0, e, GD_E_OK);
  gd_close(D);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  gd_putdata(D, "data", 800000, 0, 0, 1, GD_UINT32, &c);
  e = gd_error(D);
  CHECKIi(1, e, GD_E_OK);
  gd_putdata(D, "data", 100000, 0, 0, 1, GD_UINT32, &c);
  e = gd_error(D);
  CHECKIi(1, e, GD_E_OK);
  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  return r;
#endif
}
