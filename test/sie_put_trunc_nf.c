/* Copyright (C) 2015, 2017 D.V. Wiebe
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
  const char *data = "dirfile/data.sie";
  unsigned char c[] = {1, 1, 2, 1};
  DIRFILE *D;
  int n1, n2, r = 0;

  rmdirfile();
  mkdir(filedir, 0700); 

  MAKEFORMATFILE(format, "/ENCODING sie\n/ENDIAN little\n");

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  gd_add_raw(D, "data", GD_UINT8, 1, 0);
  gd_putdata(D, "data", 0, 0, 0, 4, GD_UINT8, c);
  gd_raw_close(D, "data");
  n1 = gd_nframes(D);
  CHECKI(n1, 4);

  gd_putdata(D, "data", 0, 2, 0, 2, GD_UINT8, c);
  n2 = gd_nframes(D);
  CHECKI(n2, 4);

  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  return r;
}
