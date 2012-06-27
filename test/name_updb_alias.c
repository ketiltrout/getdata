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

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *format_data = 
    "early  PHASE cata 0\n"
    "earlya PHASE data 0\n"
    "/ALIAS aata cata\n"
    "/ALIAS bata data\n"
    "late   PHASE cata 0\n"
    "latea  PHASE data 0\n"
    "cata RAW UINT8 8\n"
    "/ALIAS data cata\n";
  int fd, e0, e1, e2, e3, e4, e5, e6, r = 0;
  char *s1, *s2, *s3, *s4, *s5, *s6;
  gd_entry_t E;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDWR);
  gd_validate(D, "early");
  gd_validate(D, "earlya");
  gd_rename(D, "data", "zata", GD_REN_UPDB);
  e0 = gd_error(D);
  gd_spf(D, "early");
  e1 = gd_error(D);
  gd_spf(D, "earlya");
  e2 = gd_error(D);
  gd_spf(D, "late");
  e3 = gd_error(D);
  gd_spf(D, "latea");
  e4 = gd_error(D);

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

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(e0,0);
  CHECKI(e1,0);
  CHECKI(e2,0);
  CHECKI(e3,0);
  CHECKI(e4,0);
  CHECKI(e5,0);
  CHECKI(e6,0);
  CHECKS(s1, "cata");
  CHECKS(s2, "zata");
  CHECKS(s3, "cata");
  CHECKS(s4, "zata");
  CHECKS(s5, "cata");
  CHECKS(s6, "zata");

  return r;
}
