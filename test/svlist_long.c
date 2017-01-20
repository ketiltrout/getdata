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
    "data STRING a\n"
    "datb STRING b\n"
    "datc STRING c\n"
    "datd STRING d\n"
    "date STRING e\n"
    "datf STRING f\n"
    "datg STRING g\n"
    "dath STRING h\n"
    "dati STRING i\n"
    "datj STRING j\n"
    "datk STRING k\n"
    "datl STRING l\n"
    "datm STRING m\n"
    "datn STRING n\n"
    "dato STRING o\n"
    "datp STRING p\n"
    "datq STRING q\n"
    "datr STRING r\n"
    "dats STRING s\n"
    "datt STRING t\n"
    "datu STRING u\n"
    "datv STRING v\n"
    "datw STRING w\n"
    "datx STRING x\n"
    "daty STRING y\n"
    "datz STRING z\n"
    ;
  int fd, i, error, r = 0;
  const char **field_list;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  field_list = gd_strings(D);

  error = gd_error(D);
  CHECKI(error, 0);
  CHECKPN(field_list);

  for (i = 0; field_list[i]; ++i) {
    CHECKIi(i, field_list[i][0], 'a' + i);
    CHECKIi(i, field_list[i][1], 0);
  }

  CHECKI(i,26);

  gd_discard(D);
  unlink(format);
  rmdir(filedir);

  return r;
}
