/* Copyright (C) 2010-2011 D. V. Wiebe
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

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *format1 = "dirfile/format1";
  const char *format_data =
    "data RAW UINT16 5\n"
    "phase PHASE data 3\n"
    "/FRAMEOFFSET 35\n"
    "lincom LINCOM 2 phase2 1. 0. phase 1. 0.\n"
    "lincom2 LINCOM 2 phase 1. 0. phase2 1. 0.\n"
    "INCLUDE format1\n";
  const char *format1_data = "data2 RAW UINT8 3\n"
    "FRAMEOFFSET 43\n"
    "phase2 PHASE data2 -7\n";
  int fd, error1, error2, error3, error4, r = 0;
  off_t bof_phase, bof_phase2, bof_lincom, bof_lincom2;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(format1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format1_data, strlen(format1_data));
  close(fd);

  D = gd_open(filedir, GD_RDONLY);
  bof_phase = gd_bof(D, "phase");
  error1 = gd_error(D);
  bof_phase2 = gd_bof(D, "phase2");
  error2 = gd_error(D);
  bof_lincom = gd_bof(D, "lincom");
  error3 = gd_error(D);
  bof_lincom2 = gd_bof(D, "lincom2");
  error4 = gd_error(D);
  gd_close(D);

  unlink(format1);
  unlink(format);
  rmdir(filedir);

  CHECKI(error1, 0);
  CHECKI(bof_phase, 172);
  CHECKI(error2, 0);
  CHECKI(bof_phase2, 136);
  CHECKI(error3, 0);
  CHECKI(bof_lincom, 136);
  CHECKI(error4, 0);
  CHECKI(bof_lincom2, 226);

  return r;
}
