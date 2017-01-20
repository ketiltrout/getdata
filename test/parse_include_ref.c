/* Copyright (C) 2012-2013, 2017 D.V. Wiebe
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
  int r = 0;
  DIRFILE *D;
  const char *ref;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "data RAW UINT8 1\n"
    "/REFERENCE data\n"
    "INCLUDE format1\n"
  );
  MAKEFORMATFILE(format1, "zata RAW UINT8 11\n/REFERENCE zata\n");

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  ref = gd_reference(D, NULL);
  CHECKS(ref, "zata");
  gd_discard(D);

  unlink(format1);
  unlink(format);
  rmdir(filedir);

  return r;
}
