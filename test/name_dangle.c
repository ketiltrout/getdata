/* Copyright (C) 2008-2013, 2016, 2017 D.V. Wiebe
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
  const char *zata = "dirfile/zata";
  int ret, e1, e2, e3, e4, e5, e6, unlink_data, unlink_zata, r = 0;
  const char **fl;
#define NFIELDS 6
  const char *field_list[NFIELDS] = {
    "cata", "eata", "late", "zata", "INDEX", "early"
  };
  char *s1, *s4;
  const char *s3, *s6;
  unsigned nf, i;
  DIRFILE *D;
  gd_entry_t E;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "early MULTIPLY data data\n"
    "/ALIAS earlya data\n"
    "late PHASE data 0\n"
    "/ALIAS latea data\n"
    "cata RAW UINT8 8\n"
    "data RAW UINT8 8\n"
    "eata RAW UINT8 8\n"
    );
  MAKEDATAFILE(data, uint8_t, i, 256);

  D = gd_open(filedir, GD_RDWR);

  /* force-resolve the early entries */
  gd_validate(D, "early");
  gd_validate(D, "earlya");

  ret = gd_rename(D, "data", "zata", GD_REN_DANGLE);
  e1 = gd_error(D);
  CHECKI(ret, 0);
  CHECKI(e1, 0);

  gd_spf(D, "early");
  e2 = gd_error(D);
  CHECKI(e2, GD_E_BAD_CODE);

  gd_spf(D, "late");
  e3 = gd_error(D);
  CHECKI(e3, GD_E_BAD_CODE);

  nf = gd_nfields(D);
  CHECKI(nf, NFIELDS);
  if (nf > NFIELDS)
    nf = NFIELDS;

  fl = gd_field_list(D);
  for (i = 0; i < nf; ++i)
    CHECKSi(i, fl[i], field_list[i]);

  gd_entry(D, "early", &E);
  s1 = E.in_fields[0];
  CHECKS(s1, "data");
  gd_free_entry_strings(&E);

  gd_entry(D, "earlya", &E);
  e4 = gd_error(D);
  s3 = gd_alias_target(D, "earlya");
  CHECKI(e4, GD_E_BAD_CODE);
  CHECKS(s3, "data");
  gd_free_entry_strings(&E);

  gd_entry(D, "late", &E);
  s4 = E.in_fields[0];
  CHECKS(s4, "data");
  gd_free_entry_strings(&E);

  gd_entry(D, "latea", &E);
  e5 = gd_error(D);
  s6 = gd_alias_target(D, "earlya");
  CHECKI(e5, GD_E_BAD_CODE);
  CHECKS(s6, "data");
  gd_free_entry_strings(&E);

  e6 = gd_close(D);
  CHECKI(e6, 0);

  unlink_data = unlink(data);
  unlink_zata = unlink(zata);
  unlink(format);
  rmdir(filedir);
  CHECKI(unlink_data, 0);
  CHECKI(unlink_zata, -1);

  return r;
}
