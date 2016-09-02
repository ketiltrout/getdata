/* Copyright (C) 2016 D. V. Wiebe
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
#ifndef TEST_FLAC
  return 77;
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  const char *flacdata = "dirfile/data.flac";
  const char *format_data = "data RAW UINT8 8\n";
  int8_t c[8];
  char command[4096];
  int8_t data_data[256];
  int fd, n, error, r = 0;
#ifdef USE_FLAC
  int i;
#endif
  DIRFILE *D;

  memset(c, 0, 8);
  rmdirfile();
  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (unsigned char)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 256 * sizeof(uint8_t));
  close(fd);

  /* encode */
  snprintf(command, 4096,
      "%s --endian=little --silent --sample-rate=1 --channels=1 --bps=8 "
      "--sign=signed --delete-input-file %s > /dev/null", FLAC, data);
  if (gd_system(command))
    return 1;

#ifdef USE_FLAC
  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE | GD_LITTLE_ENDIAN);
#else
  D = gd_open(filedir, GD_RDONLY | GD_LITTLE_ENDIAN);
#endif
  n = gd_getdata(D, "data", 5, 0, 1, 0, GD_UINT8, c);
  error = gd_error(D);

#ifdef USE_FLAC
  CHECKI(error, 0);
  CHECKI(n, 8);

  for (i = 0; i < 8; ++i)
    CHECKXi(i,c[i],40+i);
#else
  CHECKI(error, GD_E_UNSUPPORTED);
  CHECKI(n, 0);
#endif

  gd_discard(D);

  unlink(flacdata);
  unlink(format);
  rmdir(filedir);

  return r;
#endif
}
