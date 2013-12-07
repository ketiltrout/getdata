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
/* Attempt to write LINCOM 1 */
#include "test.h"

#include <inttypes.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  const char *format_data = "lincom LINCOM 1 data 1;1 0;-1\ndata RAW INT8 8\n";
  int8_t c[8], d;
  struct stat buf;
  int fd, q, n, error, r = 0;
  DIRFILE *D;

  memset(c, 0, 8);
  rmdirfile();
  mkdir(filedir, 0777);

  for (q = 0; q < 8; ++q)
    c[q] = (int8_t)(39 + q * 2);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);
  n = gd_putdata(D, "lincom", 5, 0, 1, 0, GD_INT8, c);
  error = gd_error(D);

  gd_close(D);

  if (stat(data, &buf)) {
    perror("stat");
    r = 1;
  } else {
    CHECKI(buf.st_size, 48 * sizeof(int8_t));

    fd = open(data, O_RDONLY | O_BINARY);
    q = 0;
    while (read(fd, &d, sizeof(int8_t))) {
      if (q < 40 || q > 48) {
        CHECKIi(q,d,0);
      } else {
        /* if:
         *   L = (1+i) D - i
         * then:
         *   D = (L + i) * (1 / (1+i))
         *     = (L + i) * ((1-i) / 2)
         *     = [ (L + 1) + (1 - L) i ] / 2
         * so:
         *   Re(D) = (L + 1) / 2
         * and, if:
         *   L = 39 + 2 (q - 40),
         * then:
         *   Re(D) = (40 + 2(q-40)) / 2 = 20 + q - 40 = q - 20
         */
        CHECKIi(q,d,q - 20);
      }
      q++;
    }
    close(fd);
  }

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(n,8);
  CHECKI(error, 0);

  return r;
}
