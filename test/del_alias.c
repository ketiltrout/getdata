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
  int ret, error1, error2, error3, r = 0;
  gd_entype_t t1, t2;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "data RAW UINT8 8\n"
    "/ALIAS alias data\n"
  );

  D = gd_open(filedir, GD_RDWR);
  t1 = gd_entry_type(D, "alias");
  error1 = gd_error(D);
  CHECKI(error1, GD_E_OK);
  CHECKU(t1, GD_RAW_ENTRY);

  ret = gd_delete(D, "alias", 0);
  error2 = gd_error(D);
  CHECKI(error2, GD_E_OK);
  CHECKI(ret, 0);

  t2 = gd_entry_type(D, "alias");
  error3 = gd_error(D);
  CHECKU(t2, GD_NO_ENTRY);
  CHECKI(error3, GD_E_BAD_CODE);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
