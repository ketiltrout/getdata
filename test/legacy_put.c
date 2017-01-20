/* Copyright (C) 2008-2011, 2017 D.V. Wiebe
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
/* Attempt to write UINT8 via the legacy interface*/
#include "test.h"

int main(void)
{
#ifndef GD_LEGACY_API
  return 77; /* skipped */
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  uint8_t c[8], d;
  int fd, i, n, error, r = 0;

  memset(c, 0, 8);
  rmdirfile();
  mkdir(filedir, 0700);

  for (i = 0; i < 8; ++i)
    c[i] = (uint8_t)(40 + i);

  MAKEFORMATFILE(format, "data RAW UINT8 8\n");

  n = PutData(filedir, "data", 5, 0, 1, 0, 'c', c, &error);

  fd = open(data, O_RDONLY | O_BINARY);
  i = 0;
  if (fd >= 0) {
    while (read(fd, &d, sizeof(uint8_t))) {
      if (i < 40 || i > 48) {
        CHECKUi(i,d,0);
      } else
        CHECKUi(i,d,i);
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

  CHECKI(error,0);
  CHECKI(n,8);

  return r;
#endif
}
