/* Copyright (C) 2014 D. V. Wiebe
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
#ifndef TEST_LZMA
  return 77;
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data_lzma = "dirfile/data.lzma";
  const char *data = "dirfile/data";
  const char *format_data = "data RAW UINT8 8\n";
  uint8_t c[8] = {40, 41, 42, 43, 44, 45, 46, 47};
  uint8_t data_data[256];
  char command[4096];
  int fd, n, e1, e2, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (uint8_t)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 256 * sizeof(uint8_t));
  close(fd);

  /* compress */
  snprintf(command, 4096, "%s -F lzma -f %s > /dev/null", XZ, data);
  if (gd_system(command))
    return 1;

  D = gd_open(filedir, GD_RDWR);
  n = gd_putdata(D, "data", 5, 0, 1, 0, GD_UINT8, c);
  e1 = gd_error(D);
  CHECKI(e1, GD_E_UNSUPPORTED);
  CHECKI(n, 0);

  e2 = gd_discard(D);
  CHECKI(e2, 0);

  unlink(data_lzma);
  unlink(format);
  rmdir(filedir);

  return r;
#endif
}
