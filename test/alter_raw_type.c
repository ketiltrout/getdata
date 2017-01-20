/* Copyright (C) 2008-2011, 2013, 2016, 2017 D.V. Wiebe
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
  uint16_t d;
  int fd, i, ret, e1, e2, e3, r = 0;
  off_t n;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "data RAW UINT8 8\n");
  MAKEDATAFILE(data, unsigned char, i, 256);

  D = gd_open(filedir, GD_RDWR);
  ret = gd_alter_raw(D, "data", GD_STRING, 0, 1);
  CHECKI(ret, GD_E_BAD_TYPE);
  e1 = gd_error(D);
  CHECKI(e1, GD_E_BAD_TYPE);

  gd_alter_raw(D, "data", GD_UINT16 + 1, 0, 1);
  e2 = gd_error(D);
  CHECKI(e2, GD_E_BAD_TYPE);

  gd_alter_raw(D, "data", GD_UINT16, 0, 1);
  e3 = gd_error(D);
  CHECKI(e3, 0);

  n = gd_nframes(D);
  CHECKI(n, 32);

  gd_discard(D);

  fd = open(data, O_RDONLY | O_BINARY);
  i = 0;

  if (fd >= 0) {
    while (read(fd, &d, sizeof(uint16_t))) {
      CHECKX(d, i);
      i++;
    }
    close(fd);
  } else {
    perror("open");
    r = 1;
  }

  unlink(data);
  unlink(format);
  rmdir(filedir);

  return r;
}
