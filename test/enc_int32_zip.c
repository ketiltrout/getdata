/* Copyright (C) 2016, 2017 D. V. Wiebe
 * Copyright (C) 2019 Matthew Petroff
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
#ifdef HAVE_ZZIP_LIB_H
#ifdef ENC_SKIP_TEST
  return 77;
#else
  const char *filedir = "dirfile";
  const char *filedirzip = "dirfile.zip";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data" ENC_SUFFIX;
  const char *command = "zip -jq0 dirfile.zip dirfile/format dirfile/data" ENC_SUFFIX;
  int e1, e2, e3, r = 0;
  DIRFILE *D1, *D2;
  const int32_t data_in[8] = { -5, 6, -10, 100, 0, 1, -100, 3 };
  int32_t data_out[8];
  int i = 0;

  rmdirfile();
  unlink(filedirzip);

  D1 = gd_open(filedir, GD_RDWR | GD_CREAT | GD_EXCL | GD_ENC_ENCODED
       | GD_VERBOSE);

  e1 = gd_add_spec(D1, "data RAW INT32 1", 0);
  CHECKI(e1, 0);

  e2 = gd_putdata(D1, "data", 0, 0, 0, 8, GD_INT32, data_in);
  CHECKI(e2, 8);

  gd_close(D1);

  if (gd_system(command))
    return 1;

  unlink(format);
  unlink(data);
  rmdir(filedir);

  D2 = gd_open(filedirzip, GD_RDONLY | GD_VERBOSE);

  e3 = gd_getdata(D2, "data", 0, 0, 0, 8, GD_INT32, data_out);
  CHECKI(e3, 8);

  if (e3 > 8)
    e3 = 8;
  for (i = 0; i < e3; ++i)
    CHECKIi(i, data_out[i], data_in[i]);

  gd_discard(D2);
  unlink(filedirzip);

  return r;
#endif
#else
  return 77;
#endif
}
