/* Copyright (C) 2008-2013 D. V. Wiebe
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
    "early MULTIPLY data data\n"
    "/ALIAS earlya data\n"
    "late PHASE data 0\n"
    "/ALIAS latea data\n"
    "cata RAW UINT8 8\n"
    "data RAW UINT8 8\n"
    "eata RAW UINT8 8\n";
  unsigned char data_data[256];
  int fd, ret, e1, e2, e3, e4, e5, e6, unlink_data, unlink_zata, r = 0;
  const char **fl;
#define NFIELDS 8
  const char *field_list[NFIELDS] = {
    "INDEX", "cata", "early", "earlya", "eata", "late", "latea", "zata"
  };
  char *s1, *s2, *s5;
  const char *s3, *s4, *s6;
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

  /* force-resolve the early entries */
  gd_validate(D, "early");
  gd_validate(D, "earlya");

  ret = gd_rename(D, "data", "zata", 0);
  e1 = gd_error(D);
  CHECKI(ret,0);
  CHECKI(e1,0);

  gd_spf(D, "early");
  e2 = gd_error(D);
  CHECKI(e2,GD_E_BAD_CODE);

  gd_spf(D, "late");
  e3 = gd_error(D);
  CHECKI(e3,GD_E_BAD_CODE);

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
  s2 = E.field;
  s3 = gd_alias_target(D, "earlya");
  CHECKI(e4,0);
  CHECKS(s2, "zata");
  CHECKS(s3, "zata");
  gd_free_entry_strings(&E);

  gd_entry(D, "late", &E);
  s4 = E.in_fields[0];
  CHECKS(s4, "data");
  gd_free_entry_strings(&E);

  gd_entry(D, "latea", &E);
  e5 = gd_error(D);
  s5 = E.field;
  s6 = gd_alias_target(D, "earlya");
  CHECKI(e5,0);
  CHECKS(s5, "zata");
  CHECKS(s6, "zata");
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
