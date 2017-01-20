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
#include "test.h"

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data1 = "dirfile/data1";
  const char *data2 = "dirfile/data2";

  int error, error2, r = 0;
  DIRFILE *D;
  off_t nf;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "data1 RAW UINT8 1\n"
    "data2 RAW UINT8 1\n"
  );
  MAKEDATAFILE(data1, uint8_t, i, 4);
  MAKEDATAFILE(data2, uint8_t, i, 3);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);

  error = gd_error(D);
  CHECKI(error, 0);

  nf = gd_nframes(D);

  error2 = gd_error(D);
  CHECKI(error2, 0);
  CHECKI(nf,4);

  gd_discard(D);

  unlink(format);
  unlink(data1);
  unlink(data2);
  rmdir(filedir);

  return r;
}
