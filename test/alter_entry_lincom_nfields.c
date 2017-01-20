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
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  int ret, error, r = 0;
  gd_entry_t E;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "lincom LINCOM a 1 2\n");

  D = gd_open(filedir, GD_RDWR);

  gd_entry(D, "lincom", &E);
  E.EN(lincom,n_fields) = 4000;
  ret = gd_alter_entry(D, "lincom", &E, 0);
  CHECKI(ret, GD_E_BAD_ENTRY);
  error = gd_error(D);
  CHECKI(error, GD_E_BAD_ENTRY);

  gd_free_entry_strings(&E);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
