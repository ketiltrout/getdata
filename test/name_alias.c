/* Copyright (C) 2012 D. V. Wiebe
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
  int fd, ret, e0, e1, e2, e3, e4, e5, e6, unlink_data, unlink_zata, r = 0;
  const char **fl;
  char *field_list[9];
  char *s1, *s2, *s3, *s4, *s5, *s6;
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
  gd_spf(D, "early");
  e1 = gd_error(D);
  gd_spf(D, "late");
  e2 = gd_error(D);
  gd_spf(D, "earlya");
  e3 = gd_error(D);
  gd_spf(D, "latea");
  e4 = gd_error(D);
  fl = gd_field_list(D);

  field_list[0] = strdup(fl[0]);
  field_list[1] = strdup(fl[1]);
  field_list[2] = strdup(fl[2]);
  field_list[3] = strdup(fl[3]);
  field_list[4] = strdup(fl[4]);
  field_list[5] = strdup(fl[5]);
  field_list[6] = strdup(fl[6]);
  field_list[7] = strdup(fl[7]);
  field_list[8] = strdup(fl[8]);

  gd_entry(D, "early", &E);
  s1 = E.in_fields[0];
  E.in_fields[0] = NULL;
  gd_free_entry_strings(&E);

  gd_entry(D, "earlya", &E);
  s2 = E.in_fields[0];
  E.in_fields[0] = NULL;
  gd_free_entry_strings(&E);

  gd_entry(D, "late", &E);
  s3 = E.in_fields[0];
  E.in_fields[0] = NULL;
  gd_free_entry_strings(&E);

  gd_entry(D, "latea", &E);
  s4 = E.in_fields[0];
  E.in_fields[0] = NULL;
  gd_free_entry_strings(&E);

  gd_entry(D, "aata", &E);
  e5 = gd_error(D);
  s5 = strdup(gd_alias_target(D, "aata"));

  gd_entry(D, "bata", &E);
  e6 = gd_error(D);
  s6 = strdup(gd_alias_target(D, "bata"));

  gd_close(D);

  unlink_data = unlink(data);
  unlink_zata = unlink(zata);
  unlink(format);
  rmdir(filedir);

  CHECKI(e0,0);
  CHECKI(e1,0);
  CHECKI(e2,0);
  CHECKI(e3,GD_E_BAD_CODE);
  CHECKI(e4,GD_E_BAD_CODE);
  CHECKI(e5,0);
  CHECKI(e6,GD_E_BAD_CODE);
  CHECKI(ret,0);
  CHECKS(field_list[0], "INDEX");
  CHECKS(field_list[1], "aata");
  CHECKS(field_list[2], "cata");
  CHECKS(field_list[3], "early");
  CHECKS(field_list[4], "earlya");
  CHECKS(field_list[5], "eata");
  CHECKS(field_list[6], "late");
  CHECKS(field_list[7], "latea");
  CHECKS(field_list[8], "zata");
  CHECKI(unlink_data, 0);
  CHECKI(unlink_zata, -1);
  CHECKS(s1, "cata");
  CHECKS(s2, "data");
  CHECKS(s3, "cata");
  CHECKS(s4, "data");
  CHECKS(s5, "cata");
  CHECKS(s6, "data");

  free(field_list[0]);
  free(field_list[1]);
  free(field_list[2]);
  free(field_list[3]);

  return r;
}
