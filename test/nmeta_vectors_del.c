/* Copyright (C) 2010-2011, 2017 D.V. Wiebe
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
  int error, error2, error3, r = 0;
  unsigned int nvec, nvec2, nvec3;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "raw1 RAW UINT8 1\n"
    "META raw1 linterp1 LINTERP raw2 table\n"
    "META raw1 linterp2 LINTERP raw3 table\n"
    "META raw1 linterp3 LINTERP raw4 table\n"
    "META raw1 const CONST UINT8 1\n"
    "META raw1 string STRING value\n"
    "META raw1 string2 STRING value\n"
    "raw2 RAW UINT8 1\n"
    "raw3 RAW UINT8 1\n"
    "raw4 RAW UINT8 1\n"
    "const CONST UINT8 1\n"
    "string STRING value\n"
    "string2 STRING value\n"
  );

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  nvec = gd_nmvectors(D, "raw1");
  error = gd_error(D);
  gd_delete(D, "raw1/linterp3", 0);
  nvec2 = gd_nmvectors(D, "raw1");
  error2 = gd_error(D);
  gd_delete(D, "raw1/string", 0);
  nvec3 = gd_nmvectors(D, "raw1");
  error3 = gd_error(D);
  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(error,0);
  CHECKI(nvec,3);
  CHECKI(error2,0);
  CHECKI(nvec2,2);
  CHECKI(error3,0);
  CHECKI(nvec3,2);

  return r;
}
