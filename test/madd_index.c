/* Copyright (C) 2011, 2013 D. V. Wiebe
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
  int error, ge_error, n, r = 0;
  gd_entry_t e;
  DIRFILE *D;

  gd_entry_t E;
  memset(&E, 0, sizeof(E));
  E.field = "data";
  E.field_type = GD_RAW_ENTRY;
  E.fragment_index = 0;
  E.EN(raw,spf) = 2;
  E.EN(raw,data_type) = GD_UINT8;

  rmdirfile();
  D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  gd_add(D, &E);
  E.field_type = GD_CONST_ENTRY;
  E.EN(scalar,const_type) = GD_UINT8;
  E.fragment_index = 99;
  gd_madd(D, &E, "data");
  error = gd_error(D);

  /* check */
  n = gd_nfields(D);
  gd_entry(D, "data/data", &e);
  ge_error = gd_error(D);

  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, GD_E_OK);
  CHECKI(ge_error, 0);
  CHECKI(n, 2);

  if (!r) {
    CHECKI(e.field_type, GD_CONST_ENTRY);
    CHECKI(e.fragment_index, 0);
    CHECKI(e.EN(scalar,const_type), GD_UINT8);
    gd_free_entry_strings(&e);
  }

  return r;
}
