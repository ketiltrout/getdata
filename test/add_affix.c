/* Copyright (C) 2011-2012, 2017 D.V. Wiebe
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
  const char *format1 = "dirfile/format1";
  const char *data = "dirfile/data";
  int n, e1, e2, e3, e4, e5, r = 0;
  DIRFILE *D;
  gd_entry_t E, e;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "AconstZ CONST UINT8 3\n/INCLUDE format1 A Z\n");
  MAKEEMPTYFILE(format1, 0600);

  D = gd_open(filedir, GD_RDWR | GD_UNENCODED);

  memset(&E, 0, sizeof(E));
  E.field = "Adata1Z";
  E.field_type = GD_RAW_ENTRY;
  E.fragment_index = 1;
  E.EN(raw,spf) = 2;
  E.EN(raw,data_type) = GD_UINT8;
  E.scalar[0] = NULL;

  gd_add(D, &E);
  e1 = gd_error(D);

  memset(&E, 0, sizeof(E));
  E.field = "Adata2Z";
  E.field_type = GD_RAW_ENTRY;
  E.fragment_index = 1;
  E.EN(raw,spf) = 2;
  E.EN(raw,data_type) = GD_UINT8;
  E.scalar[0] = "const";

  gd_add(D, &E);
  e2 = gd_error(D);

  memset(&E, 0, sizeof(E));
  E.field = "Adata3Z";
  E.field_type = GD_RAW_ENTRY;
  E.fragment_index = 1;
  E.EN(raw,spf) = 2;
  E.EN(raw,data_type) = GD_UINT8;
  E.scalar[0] = "AconstZ";

  gd_add(D, &E);
  e3 = gd_error(D);

  /* check */
  gd_entry(D, "Adata1Z", &e);
  e4 = gd_error(D);
  n = gd_spf(D, "Adata3Z");
  e5 = gd_error(D);

  gd_discard(D);

  unlink(data);
  unlink(format);
  unlink(format1);
  rmdir(filedir);

  CHECKI(e1, GD_E_OK);
  CHECKI(e2, GD_E_BAD_CODE);
  CHECKI(e3, GD_E_OK);
  CHECKI(e4, GD_E_OK);
  CHECKI(e5, GD_E_OK);
  CHECKI(n, 3);
  CHECKI(e.field_type, GD_RAW_ENTRY);
  CHECKI(e.fragment_index, 1);
  CHECKI(e.EN(raw,spf), 2);
  CHECKI(e.EN(raw,data_type), GD_UINT8);
  CHECKP(e.scalar[0]);
  gd_free_entry_strings(&e);

  return r;
}
