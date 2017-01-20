/* Copyright (C) 2013, 2017 D.V. Wiebe
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
  int fd, i, ret, error, r = 0;
  off_t n;
  DIRFILE *D;
  gd_entry_t E;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "data RAW UINT8 a\n"
    "a CONST UINT8 8\n"
    "b CONST UINT8 11\n"
  );
  MAKEDATAFILE(data, unsigned char, i, 256);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);

  memset(&E, 0, sizeof(E));
  E.field_type = GD_RAW_ENTRY;
  E.EN(raw,data_type) = GD_UINT16;
  E.EN(raw,spf) = 0;
  E.scalar[0] = "b";

  ret = gd_alter_entry(D, "data", &E, 1);
  error = gd_error(D);
  n = gd_nframes(D);
  CHECKI(error, 0);
  CHECKI(n, 32);
  CHECKI(ret, 0);

  gd_discard(D);

  fd = open(data, O_RDONLY | O_BINARY);
  i = 0;

  if (fd >= 0) {
    while (read(fd, &d, sizeof(uint16_t))) {
      CHECKXi(i, d, i * 8 / 11);
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
