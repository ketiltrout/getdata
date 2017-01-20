/* Copyright (C) 2008-2011, 2013, 2017 D.V. Wiebe
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
/* Test include */
#include "test.h"

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *format1 = "dirfile/format1";
  const char *format2 = "dirfile/format2";
  int ret1, ret2, error1, error2, r = 0;
  unsigned int nfields, nfragments;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "/INCLUDE format1\na CONST UINT8 1\n");
  MAKEFORMATFILE(format1, "b CONST UINT8 11\n/INCLUDE format2\n");
  MAKEFORMATFILE(format2, "c CONST UINT8 11\n");

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  ret1 = gd_uninclude(D, 2, 0);
  error1 = gd_error(D);
  ret2 = gd_include(D, "format2", 0, 0);
  error2 = gd_error(D);
  nfields = gd_nfields(D);
  nfragments = gd_nfragments(D);
  gd_discard(D);

  unlink(format2);
  unlink(format1);
  unlink(format);
  rmdir(filedir);

  CHECKI(error1,0);
  CHECKI(error2,0);
  CHECKI(ret1,0);
  CHECKI(ret2,2);
  CHECKI(nfields,4);
  CHECKI(nfragments,3);

  return r;
}
