/* Copyright (C) 2015 D. V. Wiebe
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
#if !defined TEST_ZZIP || !defined USE_ZZIP
  return 77;
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  const char *rawzip = "dirfile/raw.zip";
  const char *format_data = "data RAW UINT16 8\n";
  char command[4096];
  uint16_t data_data[256];
  int fd, n, error, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (unsigned char)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 256 * sizeof(uint16_t));
  close(fd);

  chdir(filedir);
  snprintf(command, 4096, "%s raw data > /dev/null", ZIP);
  if (gd_system(command))
    return 1;
  chdir("..");
  unlink(data);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);

  n = gd_seek(D, "data", 500, 0, GD_SEEK_SET);
  CHECKI(n, 256);

  error = gd_error(D);
  CHECKI(error, 0);

  gd_discard(D);

  unlink(rawzip);
  unlink(format);
  rmdir(filedir);

  return r;
#endif
}
