/* Copyright (C) 2013, 2016 D. V. Wiebe
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
  int e1, e2, r = 0;
  DIRFILE *D;

  gd_entry_t E, e;
  memset(&E, 0, sizeof(E));
  E.field = "data";
  E.field_type = GD_LINCOM_ENTRY;
  E.fragment_index = 0;
  E.EN(lincom,n_fields) = 1;
  E.in_fields[0] = "INDEX";
  E.EN(lincom,m)[0] = 1.;
  E.scalar[0] = NULL;
  E.scalar[0 + GD_MAX_LINCOM] = "c";
  E.scalar_ind[0 + GD_MAX_LINCOM] = -1;

  rmdirfile();
  D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  gd_add_spec(D, "c CARRAY INT64 1 2 3 4", 0);
  gd_add(D, &E);
  e1 = gd_error(D);
  CHECKI(e1, GD_E_OK);

  /* check */
  gd_entry(D, "data", &e);
  e2 = gd_error(D);
  CHECKI(e2, GD_E_OK);
  CHECKI(e.field_type, GD_LINCOM_ENTRY);
  CHECKI(e.fragment_index, 0);
  CHECKI(e.EN(lincom,n_fields), 1);
  CHECKF(e.EN(lincom,m)[0], 1);
  CHECKF(e.EN(lincom,b)[0], 1);
  CHECKP(e.scalar[0]);
  CHECKS(e.scalar[0 + GD_MAX_LINCOM], "c");
  CHECKI(e.scalar_ind[0 + GD_MAX_LINCOM], 0);
  gd_free_entry_strings(&e);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
