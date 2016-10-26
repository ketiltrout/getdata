/* Copyright (C) 2014 D. V. Wiebe
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
  const char *format1 = "dirfile/format1";
  const char *format_data = "/INCLUDE format1 ns.\n";
  const char *format1_data = "#\n";
  int error, n, r = 0;
  gd_entry_t E;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0777);

  n = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(n, format_data, strlen(format_data));
  close(n);

  n = open(format1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(n, format1_data, strlen(format1_data));
  close(n);

  E.field =  "ns.w";
  E.field_type = GD_CONST_ENTRY;
  E.fragment_index = 1;
  E.EN(scalar,const_type) = GD_UINT8;

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  gd_add(D, &E);
  error = gd_error(D);
  CHECKI(error, GD_E_OK);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
