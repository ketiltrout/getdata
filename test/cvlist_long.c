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
  const char *format_data =
    "data00 CONST UINT8 0\n"
    "data01 CONST UINT8 1\n"
    "data02 CONST UINT8 2\n"
    "data03 CONST UINT8 3\n"
    "data04 CONST UINT8 4\n"
    "data05 CONST UINT8 5\n"
    "data06 CONST UINT8 6\n"
    "data07 CONST UINT8 7\n"
    "data08 CONST UINT8 8\n"
    "data09 CONST UINT8 9\n"
    "data10 CONST UINT8 10\n"
    "data11 CONST UINT8 11\n"
    "data12 CONST UINT8 12\n"
    "data13 CONST UINT8 13\n"
    "data14 CONST UINT8 14\n"
    "data15 CONST UINT8 15\n"
    "data16 CONST UINT8 16\n"
    "data17 CONST UINT8 17\n"
    "data18 CONST UINT8 18\n"
    "data19 CONST UINT8 19\n"
    ;
  int fd, error, r = 0;
  const uint8_t *field_list;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  field_list = (const uint8_t *)gd_constants(D, GD_UINT8);

  error = gd_error(D);

  CHECKI(error, 0);

  if (!r)
    for (fd = 0; fd < 20; ++fd)
      CHECKUi(fd,field_list[fd], fd);

  gd_discard(D);
  unlink(format);
  rmdir(filedir);

  return r;
}
