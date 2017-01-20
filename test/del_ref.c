/* Copyright (C) 2013, 2017 D.V. Wiebe
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
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  unsigned char c[8];
  int fd, ret, e1, e2, r = 0;
  const char *ref1, *ref2;
  DIRFILE *D;

  memset(c, 0, 8);
  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "data RAW UINT8 8\n"
    "cata RAW UINT8 8\n"
    "/REFERENCE data\n"
  );
  MAKEDATAFILE(data, unsigned char, i, 256);

  D = gd_open(filedir, GD_RDWR);
  ref1 = gd_reference(D, NULL);
  CHECKS(ref1, "data");

  ret = gd_delete(D, "data", 0);
  e1 = gd_error(D);
  CHECKI(ret, 0);
  CHECKI(e1, GD_E_OK);

  ref2 = gd_reference(D, NULL);
  CHECKS(ref2, "cata");

  e2 = gd_discard(D);
  CHECKI(e2, 0);

  fd = unlink(data);
  unlink(format);
  rmdir(filedir);
  CHECKI(fd, 0);

  return r;
}
