/* Copyright (C) 2008-2011, 2013, 2017 D.V. Wiebe
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
/* Attempt to delete a field */
#include "test.h"

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  int ret, error1, error2, r = 0;
  DIRFILE *D;
  unsigned int spf;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "data CONST UINT8 13\n"
    "raw RAW UINT8 data\n"
  );

  D = gd_open(filedir, GD_RDWR);
  ret = gd_delete(D, "data", GD_DEL_FORCE);
  error1 = gd_error(D);
  spf = gd_spf(D, "raw");
  error2 = gd_error(D);
  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error1, GD_E_OK);
  CHECKI(error2, GD_E_BAD_SCALAR);
  CHECKU(spf, 0);
  CHECKI(ret, 0);

  return r;
}
