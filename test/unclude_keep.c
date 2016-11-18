/* Copyright (C) 2016 D. V. Wiebe
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
  const char *format2 = "dirfile/format2";
  const char *format_data =
    "/INCLUDE format1\n"
    "/INCLUDE format2\n"
    "a CONST UINT8 1\n";
  const char *format1_data = "b CONST UINT8 11\n";
  const char *format2_data = "c CONST UINT8 11\n";
  int f, ret, e1, e2, unlink_format1, unlink_format2, r = 0;
  unsigned int nfields, nfragments;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0777);

  f = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(f, format_data, strlen(format_data));
  close(f);

  f = open(format1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(f, format1_data, strlen(format1_data));
  close(f);

  f = open(format2, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(f, format2_data, strlen(format2_data));
  close(f);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  ret = gd_uninclude(D, 1, 0);
  e1 = gd_error(D);
  CHECKI(ret,0);
  CHECKI(e1,0);

  nfields = gd_nfields(D);
  CHECKI(nfields,3);

  nfragments = gd_nfragments(D);
  CHECKI(nfragments,2);

  f = gd_fragment_index(D, "c");
  CHECKI(f, 1);

  e2 = gd_close(D);
  CHECKI(e2, 0);

  unlink_format2 = unlink(format2);
  unlink_format1 = unlink(format1);
  unlink(format);
  rmdir(filedir);

  CHECKI(unlink_format2,0);
  CHECKI(unlink_format1,0);

  return r;
}
