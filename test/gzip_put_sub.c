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
#if ! (defined TEST_GZIP) || ! (defined USE_GZIP)
  return 77;
#else
  const char *filedir = "dirfile";
  const char *subdir = "dirfile/sub";
  const char *format = "dirfile/format";
  const char *format1 = "dirfile/sub/format1";
  const char *data_gz = "dirfile/sub/data.gz";
  const char *data = "dirfile/sub/data";
  uint8_t c[8];
  char command[4096];
  uint8_t d;
  struct stat buf;
  int fd, i, n1, n2, e1, e2, e3, stat_data, unlink_data, r = 0;
  DIRFILE *D;

  memset(c, 0, 8);
  rmdirfile();
  mkdir(filedir, 0700);
  mkdir(subdir, 0700);

  for (i = 0; i < 8; ++i)
    c[i] = (uint8_t)(40 + i);

  MAKEFORMATFILE(format, "/INCLUDE sub/format1\n");
  MAKEFORMATFILE(format1, "data RAW UINT8 8\n");

  D = gd_open(filedir, GD_RDWR | GD_GZIP_ENCODED | GD_VERBOSE);
  n1 = gd_putdata(D, "data", 5, 0, 1, 0, GD_UINT8, c);
  e1 = gd_error(D);
  CHECKI(e1, GD_E_OK);
  CHECKI(n1, 8);

  n2 = gd_putdata(D, "data", 0, 0, 1, 0, GD_UINT8, c);
  e2 = gd_error(D);
  CHECKI(e2, GD_E_OK);
  CHECKI(n2, 8);

  e3 = gd_close(D);
  CHECKI(e3, 0);

  stat_data = stat(data_gz, &buf);
  if (stat_data) {
    perror("stat");
  }
  CHECKI(stat_data, 0);

  /* uncompress */
  snprintf(command, 4096, "%s -f %s > /dev/null", GUNZIP, data);
  if (gd_system(command)) {
    r = 1;
  } else {
    fd = open(data, O_RDONLY | O_BINARY);
    if (fd >= 0) {
      i = 0;
      while (read(fd, &d, sizeof(uint8_t))) {
        if (i < 8) {
          CHECKIi(i, d, i + 40);
        } else if (i < 40 || i > 48) {
          CHECKIi(i, d, 0);
        } else
          CHECKIi(i, d, i);
        i++;
      }
      CHECKI(i, 48);
      close(fd);
    }
  }

  unlink_data = unlink(data);
  unlink(format1);
  unlink(format);
  rmdir(subdir);
  rmdir(filedir);

  CHECKI(unlink_data, 0);

  return r;
#endif
}
