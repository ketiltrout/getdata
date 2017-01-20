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
/* Test field modifying */
#include "test.h"

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *table = "dirfile/table";
  const char *table1 = "dirfile/table1";
  const char *data = "dirfile/data";
  int32_t c[8];
  gd_entry_t e;
  int i, ret, e1, e2, e3, n, unlink_table, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "data RAW INT32 8\nlut LINTERP data table\n");
  MAKEFORMATFILE(table, "0 0\n1000 5000\n");
  MAKEFORMATFILE(table1, "0 0\n1000 10000\n");
  MAKEDATAFILE(data, int32_t, i, 256);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  gd_getdata(D, "lut", 5, 0, 1, 0, GD_NULL, NULL);
  ret = gd_alter_linterp(D, "lut", NULL, "table1", 0);
  e1 = gd_error(D);
  CHECKI(ret, 0);
  CHECKI(e1, 0);

  gd_entry(D, "lut", &e);
  e2 = gd_error(D);
  CHECKI(e2, 0);
  CHECKS(e.in_fields[0], "data");
  CHECKS(e.EN(linterp,table), "table1");
  gd_free_entry_strings(&e);

  n = gd_getdata(D, "lut", 5, 0, 1, 0, GD_INT32, c);
  CHECKI(n, 8);

  for (i = 0; i < 8; ++i)
    CHECKIi(i,c[i], (i + 40) * 10);

  e3 = gd_close(D);
  CHECKI(e3, 0);

  unlink(data);
  unlink_table = unlink(table);
  unlink(table1);
  unlink(format);
  rmdir(filedir);

  CHECKI(unlink_table, 0);

  return r;
}
