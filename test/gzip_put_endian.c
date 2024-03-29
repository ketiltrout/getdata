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

#ifdef WORDS_BIGENDIAN
#define ENDIAN "ENDIAN little"
#else
#define ENDIAN "ENDIAN big"
#endif

int main(void)
{
#ifndef TEST_GZIP
  return 77;
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data_gz = "dirfile/data.gz";
  const char *data = "dirfile/data";
  uint16_t c[8];
#ifdef USE_GZIP
  char command[4096];
  uint16_t d;
#endif
  struct stat buf;
  int fd, i, n, e1, e2, stat_data, unlink_data, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  for (i = 0; i < 8; ++i)
    c[i] = (uint16_t)(0x102 * i);

  MAKEFORMATFILE(format, "data RAW UINT16 8\n" ENDIAN "\n");

#ifdef USE_GZIP
  D = gd_open(filedir, GD_RDWR | GD_GZIP_ENCODED | GD_VERBOSE);
#else
  D = gd_open(filedir, GD_RDWR | GD_GZIP_ENCODED);
#endif
  n = gd_putdata(D, "data", 5, 0, 1, 0, GD_UINT16, c);
  e1 = gd_error(D);

  e2 = gd_close(D);
  CHECKI(e2, 0);

  stat_data = stat(data_gz, &buf);
#ifdef USE_GZIP
  if (stat_data) {
    perror("stat");
  }
  CHECKI(stat_data, 0);
#else
  CHECKI(stat_data, -1);
#endif

#ifdef USE_GZIP
  /* uncompress */
  snprintf(command, 4096, "%s -f %s > /dev/null", GUNZIP, data);
  if (gd_system(command)) {
    r = 1;
  } else {
    fd = open(data, O_RDONLY | O_BINARY);
    if (fd >= 0) {
      i = 0;
      while (read(fd, &d, sizeof(uint16_t))) {
        if (i < 40 || i > 48) {
          CHECKIi(i, d, 0);
        } else
          CHECKIi(i, d, 0x201 * (i - 40));
        i++;
      }
      CHECKI(i, 48);
      close(fd);
    }
  }
#endif

  unlink_data = unlink(data);
  unlink(format);
  rmdir(filedir);

#ifdef USE_GZIP
  CHECKI(unlink_data, 0);
  CHECKI(e1, GD_E_OK);
  CHECKI(n, 8);
#else
  CHECKI(unlink_data, -1);
  CHECKI(e1, GD_E_UNSUPPORTED);
  CHECKI(n, 0);
#endif

  return r;
#endif
}
