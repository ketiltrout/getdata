/* Copyright (C) 2016 D. V. Wiebe
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
  int error, r = 0;
  DIRFILE *D;

  gd_entry_t E;
  memset(&E, 0, sizeof(E));
  E.field = "data";
  E.field_type = GD_RAW_ENTRY;
  E.fragment_index = 0;
  E.EN(raw,spf) = 2;
  E.EN(raw,data_type) = GD_UINT8;

  rmdirfile();
  D = gd_open(filedir, GD_RDWR | GD_CREAT);
  gd_add(D, &E);
  E.field_type = GD_CONST_ENTRY;
  E.EN(scalar,const_type) = GD_UINT8;
  gd_madd(D, &E, "data");
  gd_madd(D, &E, "data/data");
  error = gd_error(D);

  CHECKI(error, GD_E_BAD_CODE);

  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  return r;
}
