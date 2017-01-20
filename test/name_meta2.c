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
  int ret, e1, e2, e3, e4, e5, e6, r = 0;
  const char **fl;
#define NFIELDS 7
  const char *field_list[NFIELDS] = {
    "early", "earlya", "late", "latea", "cata", "zata", "eata"
  };
  char *s1, *s2, *s5;
  const char *s3, *s4, *s6;
  unsigned nf, i;
  DIRFILE *D;
  gd_entry_t E;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "parent RAW UINT8 1\n"
    "parent/early MULTIPLY parent/data parent/data\n"
    "/ALIAS parent/earlya parent/data\n"
    "parent/late PHASE parent/data 0\n"
    "/ALIAS parent/latea parent/data\n"
    "parent/cata PHASE parent 0\n"
    "parent/data PHASE parent 0\n"
    "parent/eata PHASE parent 0\n"
  );

  D = gd_open(filedir, GD_RDWR);

  /* force-resolve the early entries */
  gd_validate(D, "parent/early");
  gd_validate(D, "parent/earlya");

  ret = gd_rename(D, "parent/data", "zata", 0);
  e1 = gd_error(D);
  CHECKI(ret,0);
  CHECKI(e1,0);

  gd_spf(D, "parent/early");
  e2 = gd_error(D);
  CHECKI(e2,GD_E_BAD_CODE);

  gd_spf(D, "parent/late");
  e3 = gd_error(D);
  CHECKI(e3,GD_E_BAD_CODE);

  nf = gd_nmfields(D, "parent");
  CHECKI(nf, NFIELDS);
  if (nf > NFIELDS)
    nf = NFIELDS;

  fl = gd_mfield_list(D, "parent");
  for (i = 0; i < nf; ++i)
    CHECKSi(i, fl[i], field_list[i]);

  memset(&E, 0, sizeof E);
  gd_entry(D, "parent/early", &E);
  s1 = E.in_fields[0];
  CHECKS(s1, "parent/data");
  gd_free_entry_strings(&E);

  memset(&E, 0, sizeof E);
  gd_entry(D, "parent/earlya", &E);
  e4 = gd_error(D);
  s2 = E.field;
  s3 = gd_alias_target(D, "parent/earlya");
  CHECKI(e4,0);
  CHECKS(s2, "parent/zata");
  CHECKS(s3, "parent/zata");
  gd_free_entry_strings(&E);

  memset(&E, 0, sizeof E);
  gd_entry(D, "parent/late", &E);
  s4 = E.in_fields[0];
  CHECKS(s4, "parent/data");
  gd_free_entry_strings(&E);

  memset(&E, 0, sizeof E);
  gd_entry(D, "parent/latea", &E);
  e5 = gd_error(D);
  s5 = E.field;
  s6 = gd_alias_target(D, "parent/earlya");
  CHECKI(e5,0);
  CHECKS(s5, "parent/zata");
  CHECKS(s6, "parent/zata");
  gd_free_entry_strings(&E);

  e6 = gd_close(D);
  CHECKI(e6, 0);

  unlink(format);
  rmdir(filedir);

  return r;
}
