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
  const char *data = "dirfile/data";
  unsigned char data_data[256];
  int i, fd, e, r = 0;
  off_t t, s;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  memset(data_data, 0, 256);

  MAKEFORMATFILE(format, "FRAMEOFFSET 2\ndata RAW UINT8 1\n");

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 256);
  close(fd);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);

  for (i = 0; i < 10; ++i) {
    s = gd_seek(D, "data", i, 0, GD_SEEK_SET);
    e = gd_error(D);
    t = gd_tell(D, "data"); 
    CHECKIi(i, s, i);
    CHECKIi(i, e, GD_E_OK);
    CHECKIi(i, t, i);
  }

  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  return r;
}
