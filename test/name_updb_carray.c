/* Copyright (C) 2014, 2016 D. V. Wiebe
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
  int e1, e2, e3, r = 0;
  DIRFILE *D;
  gd_entry_t E;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "phase PHASE INDEX data<3>\n"
    "data CARRAY UINT8 8 9 10 11 12\n"
    "indir INDIR INDEX data\n"
    "lincom LINCOM INDEX data data<1>\n"
    );

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);

  gd_rename(D, "data", "zata", GD_REN_UPDB);
  e1 = gd_error(D);
  CHECKI(e1, 0);

  gd_entry(D, "phase", &E);
  e2 = gd_error(D);
  CHECKI(e2, 0);
  CHECKS(E.scalar[0], "zata");
  CHECKI(E.scalar_ind[0], 3);
  gd_free_entry_strings(&E);

  gd_entry(D, "lincom", &E);
  e3 = gd_error(D);
  CHECKI(e3, 0);
  CHECKS(E.scalar[0], "zata");
  CHECKS(E.scalar[GD_MAX_LINCOM], "zata");
  CHECKI(E.scalar_ind[0], 0);
  CHECKI(E.scalar_ind[GD_MAX_LINCOM], 1);
  gd_free_entry_strings(&E);

  gd_entry(D, "indir", &E);
  CHECKS(E.in_fields[1], "zata");
  gd_free_entry_strings(&E);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
