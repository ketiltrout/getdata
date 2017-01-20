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
/* Test gd_fragmentname out-of-range handling */
#include "test.h"

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *format1 = "dirfile/format1";
  const char *form0 = NULL;
  const char *form1 = NULL;
  int error, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "INCLUDE format1\n");
  MAKEFORMATFILE(format1, "data RAW UINT8 11\n");

  D = gd_open(filedir, GD_RDONLY);
  form0 = gd_fragmentname(D, -3000);
  form1 = gd_fragmentname(D, 1000);
  error = gd_error(D);
  gd_discard(D);

  unlink(format1);
  unlink(format);
  rmdir(filedir);

  CHECKP(form0);
  CHECKP(form1);
  CHECKI(error, GD_E_BAD_INDEX);

  return r;
}
