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

/* Overwriting text-encoded samples must preserve the lines that follow,
 * even when the replacement lines differ in width */
int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data.txt";
  int32_t d, c[2];
  int n, e, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "data RAW INT32 1\n");

  D = gd_open(filedir, GD_RDWR | GD_TEXT_ENCODED | GD_VERBOSE);

  /* write sample 1, leaving a zero-filled hole at sample 0 */
  d = 2;
  n = gd_putdata(D, "data", 1, 0, 0, 1, GD_INT32, &d);
  CHECKI(n, 1);

  /* backfill sample 0 with a wider value */
  d = -1000000;
  n = gd_putdata(D, "data", 0, 0, 0, 1, GD_INT32, &d);
  CHECKI(n, 1);

  e = gd_error(D);
  CHECKI(e, GD_E_OK);

  n = (int)gd_getdata(D, "data", 0, 0, 2, 0, GD_INT32, c);
  CHECKI(n, 2);
  CHECKIi(0, c[0], -1000000);
  CHECKIi(1, c[1], 2);
  CHECKI(gd_eof64(D, "data"), 2);

  /* overwrite sample 0 with a narrower value; the old line's excess
   * bytes must not survive */
  d = 3;
  n = gd_putdata(D, "data", 0, 0, 0, 1, GD_INT32, &d);
  CHECKI(n, 1);

  memset(c, 0, sizeof(c));
  n = (int)gd_getdata(D, "data", 0, 0, 2, 0, GD_INT32, c);
  CHECKI(n, 2);
  CHECKIi(0, c[0], 3);
  CHECKIi(1, c[1], 2);
  CHECKI(gd_eof64(D, "data"), 2);

  e = gd_close(D);
  CHECKI(e, 0);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  return r;
}
