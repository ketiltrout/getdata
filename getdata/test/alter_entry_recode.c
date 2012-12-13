/* Copyright (C) 2008-2011 D. V. Wiebe
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
/* Test field modifying */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <inttypes.h>
#include <errno.h>
#include <stdio.h>

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  const char *format_data = "data RAW UINT8 8\n";
  unsigned char data_data[256];
  uint16_t d;
  int fd, i, ret, error, r = 0;
  off_t n;
  DIRFILE *D;
  gd_entry_t E;

  rmdirfile();
  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (unsigned char)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 256);
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  gd_entry(D, "data", &E);
  E.EN(raw,data_type) = GD_UINT16;
  E.EN(raw,spf) = 11;
  ret = gd_alter_entry(D, "data", &E, 1);
  error = gd_error(D);
  n = gd_nframes(D);

  gd_close(D);

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

  CHECKI(error, 0);
  CHECKI(n, 32);
  CHECKI(ret, 0);
  gd_free_entry_strings(&E);

  return r;
}
