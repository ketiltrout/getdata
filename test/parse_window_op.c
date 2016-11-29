/* Copyright (C) 2011, 2013, 2016 D. V. Wiebe
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

int nerror = 0;

int callback(gd_parser_data_t *pdata, void *extra)
{
  pdata = extra; /* avoid compiler warnings */
  extra = pdata;

  nerror++;
  return GD_SYNTAX_CONTINUE;
}

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  int e1, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
      "w1 WINDOW a b = 1\n"
      "w2 WINDOW a b EQUALS 2\n"
      "w3 WINDOW a b LTDL 3\n"
      "w4 WINDOW a b LET 4\n"
      "w5 WINDOW a b GTO 5\n"
      "w6 WINDOW a b GET 6\n"
      "w7 WINDOW a b NET 7\n"
      "w8 WINDOW a b SETTER 8\n"
      "w9 WINDOW a b CLRV 9\n"
      );

  D = gd_cbopen(filedir, GD_RDONLY, callback, NULL);
  e1 = gd_error(D);
  CHECKI(e1, GD_E_FORMAT);
  CHECKI(nerror, 9);
  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
