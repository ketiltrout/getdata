/* Copyright (C) 2013, 2017 D.V. Wiebe
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
  const char *table = "dirfile/table";
  const char *table_data = "0 0\n1 1\n";
  int fd, error, r = 0;
  DIRFILE *D;
  gd_type_t type;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "linterp LINTERP data table\n"
    "data RAW UINT8 11\n"
  );

  fd = open(table, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, table_data, strlen(table_data));
  close(fd);
  
  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);

  type = gd_native_type(D, "linterp");
  error = gd_error(D);
  CHECKU(type, GD_FLOAT64);
  CHECKI(error, 0);

  gd_discard(D);

  unlink(table);
  unlink(format);
  rmdir(filedir);

  return r;
}
