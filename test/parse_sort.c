/* Copyright (C) 2008-2011 D. V. Wiebe
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
/* Field sort check */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *format_data =
    "c RAW UINT8 1\n"
    "d RAW UINT8 1\n"
    "g RAW UINT8 1\n"
    "h RAW UINT8 1\n"
    "i RAW UINT8 1\n"
    "k RAW UINT8 1\n"
    "f RAW UINT8 1\n"
    "b RAW UINT8 1\n"
    "a RAW UINT8 1\n"
    "j RAW UINT8 1\n"
    "e RAW UINT8 1\n";
  int fd, r = 0;
  const char **field_list;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  field_list = gd_field_list(D);

  if (gd_error(D))
    r = 1;
  else if (field_list == NULL)
    r = 1;
  else if (field_list[0][0] != 'I')
    r = 1;
  else if (field_list[1][0] != 'a')
    r = 1;
  else if (field_list[2][0] != 'b')
    r = 1;
  else if (field_list[3][0] != 'c')
    r = 1;
  else if (field_list[4][0] != 'd')
    r = 1;
  else if (field_list[5][0] != 'e')
    r = 1;
  else if (field_list[6][0] != 'f')
    r = 1;
  else if (field_list[7][0] != 'g')
    r = 1;
  else if (field_list[8][0] != 'h')
    r = 1;
  else if (field_list[9][0] != 'i')
    r = 1;
  else if (field_list[10][0] != 'j')
    r = 1;
  else if (field_list[11][0] != 'k')
    r = 1;

  gd_close(D);
  unlink(format);
  rmdir(filedir);

  return r;
}
