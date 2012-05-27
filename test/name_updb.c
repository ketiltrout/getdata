/* Copyright (C) 2011 D. V. Wiebe
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
    "early PHASE data 0\n"
    "late PHASE data 0\n"
    "data RAW UINT8 8\n";
  int fd, e1, e2, e3, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  gd_validate(D, "early");
  gd_rename(D, "data", "zata", GD_REN_UPDB);
  e1 = gd_error(D);
  gd_spf(D, "early");
  e2 = gd_error(D);
  gd_spf(D, "late");
  e3 = gd_error(D);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(e1,0);
  CHECKI(e2,0);
  CHECKI(e3,0);

  return r;
}
