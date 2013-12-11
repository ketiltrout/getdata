/* Copyright (C) 2009-2011, 2013 D. V. Wiebe
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
  int error, r = 0;
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

  rmdirfile();
  D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  gd_add_spec(D, "c CONST INT64 4", 0);
  gd_add(D, &E);
  error = gd_error(D);

  /* check */
  gd_entry(D, "data", &e);
  if (gd_error(D))
    r = 1;
  else {
    CHECKI(e.field_type, GD_LINCOM_ENTRY);
    CHECKI(e.fragment_index, 0);
    CHECKI(e.EN(lincom,n_fields), 1);
    CHECKF(e.EN(lincom,m)[0], 1);
    CHECKF(e.EN(lincom,b)[0], 4);
    CHECKP(e.scalar[0]);
    CHECKS(e.scalar[0 + GD_MAX_LINCOM], "c");
    gd_free_entry_strings(&e);
  }

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error, GD_E_OK);

  return r;
}
