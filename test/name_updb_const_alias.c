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
    "early RAW UINT8 c\n"
    "late RAW UINT8 c\n"
    "/ALIAS b c\n"
    "e CONST UINT8 2\n"
    "/ALIAS c e\n";
  int fd, e1, e2, e3, e4, r = 0;
  char *s1, *s2, *s3;
  DIRFILE *D;
  gd_entry_t E;

  rmdirfile();
  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDWR);
  gd_validate(D, "early");
  gd_rename(D, "c", "d", GD_REN_UPDB);
  e1 = gd_error(D);
  gd_spf(D, "early");
  e2 = gd_error(D);
  gd_spf(D, "late");
  e3 = gd_error(D);

  gd_entry(D, "early", &E);
  s1 = E.scalar[0];
  E.scalar[0] = NULL;
  gd_free_entry_strings(&E);

  gd_entry(D, "late", &E);
  s2 = E.scalar[0];
  E.scalar[0] = NULL;
  gd_free_entry_strings(&E);

  gd_entry(D, "b", &E);
  e4 = gd_error(D);
  s3 = strdup(gd_alias_target(D, "b"));

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(e1,0);
  CHECKI(e2,0);
  CHECKI(e3,0);
  CHECKI(e4,0);
  CHECKS(s1, "d");
  CHECKS(s2, "d");
  CHECKS(s3, "d");

  return r;
}
