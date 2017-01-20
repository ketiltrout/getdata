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
/* The number of frames should track the samples per frame */
#include "test.h"

static void write_format(const char *format, int spf)
{
  char format_data[100];
  int fd;
  sprintf(format_data, "data RAW UINT8 %i\n", spf);

  fd = open(format, O_CREAT | O_TRUNC | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);
}

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  int fd, i, r = 0;
  const size_t len = strlen(data);

  rmdirfile();
  mkdir(filedir, 0700);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data, len);
  close(fd);

  for (i = 1; i < (int)len + 1; ++i) {
    DIRFILE *D;
    unsigned int spf;
    size_t n;

    write_format(format, i);
    D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
    spf = gd_spf(D, "data");
    n = gd_nframes(D);
    CHECKUi(i, n, len / spf);
    gd_discard(D);
  }

  unlink(data);
  unlink(format);
  rmdir(filedir);

  return r;
}
