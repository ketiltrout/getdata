/* Copyright (C) 2012-2013 D. V. Wiebe
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

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  const char *zata = "dirfile/zata";
  const char *format_data =
    "early PHASE cata 0\n"
    "earlya PHASE data 0\n"
    "/ALIAS aata cata\n"
    "/ALIAS bata data\n"
    "late PHASE cata 0\n"
    "latea PHASE data 0\n"
    "cata RAW UINT8 8\n"
    "/ALIAS data cata\n"
    "eata RAW UINT8 8\n";
  unsigned char data_data[256];
  int fd, ret, e0, e1, e2, e3, e4, e5, e6, e7, unlink_data, unlink_zata, r = 0;
  const char **fl;
#define NFIELDS 10
  const char *field_list[NFIELDS] = {
    "INDEX", "aata", "bata", "cata", "early", "earlya", "eata", "late", "latea",
    "zata"
  };
  char *s1, *s2, *s3, *s4;
  const char *s5, *s6;
  unsigned nf, i;
  DIRFILE *D;
  gd_entry_t E;

  rmdirfile();
  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (unsigned char)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 256);
  close(fd);

  D = gd_open(filedir, GD_RDWR);
  gd_validate(D, "early");
  gd_validate(D, "earlya");

  ret = gd_rename(D, "data", "zata", 0);
  e0 = gd_error(D);
  CHECKI(ret, 0);
  CHECKI(e0, 0);

  gd_spf(D, "early");
  e1 = gd_error(D);
  CHECKI(e1, 0);

  gd_spf(D, "late");
  e2 = gd_error(D);
  CHECKI(e2, 0);

  gd_spf(D, "earlya");
  e3 = gd_error(D);
  CHECKI(e3, GD_E_BAD_CODE);

  gd_spf(D, "latea");
  e4 = gd_error(D);
  CHECKI(e4, GD_E_BAD_CODE);

  nf = gd_nfields(D);
  CHECKU(nf, NFIELDS);

  if (nf > NFIELDS)
    nf = NFIELDS;

  fl = gd_field_list(D);
  for (i = 0; i < nf; ++i)
    CHECKSi(i, fl[i], field_list[i]);

  gd_entry(D, "early", &E);
  s1 = E.in_fields[0];
  CHECKS(s1, "cata");
  gd_free_entry_strings(&E);

  gd_entry(D, "earlya", &E);
  s2 = E.in_fields[0];
  CHECKS(s2, "data");
  gd_free_entry_strings(&E);

  gd_entry(D, "late", &E);
  s3 = E.in_fields[0];
  CHECKS(s3, "cata");
  gd_free_entry_strings(&E);

  gd_entry(D, "latea", &E);
  s4 = E.in_fields[0];
  CHECKS(s4, "data");
  gd_free_entry_strings(&E);

  gd_entry(D, "aata", &E);
  e5 = gd_error(D);
  s5 = gd_alias_target(D, "aata");
  CHECKI(e5, 0);
  CHECKS(s5, "cata");
  gd_free_entry_strings(&E);

  gd_entry(D, "bata", &E);
  e6 = gd_error(D);
  s6 = gd_alias_target(D, "bata");
  CHECKS(s6, "zata");
  CHECKI(e6, 0);
  gd_free_entry_strings(&E);

  e7 = gd_close(D);
  CHECKI(e7, 0);

  unlink_data = unlink(data);
  unlink_zata = unlink(zata);
  unlink(format);
  rmdir(filedir);
  CHECKI(unlink_data, 0);
  CHECKI(unlink_zata, -1);

  return r;
}
