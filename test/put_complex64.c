/* Copyright (C) 2009-2011, 2013, 2017 D.V. Wiebe
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
#ifdef GD_NO_C99_API
  float c[8][2], d[2];
  const float zero[] = {0, 0};
#else
  float _Complex c[8], d;
  const float _Complex zero = 0;
#endif
  struct stat buf;
  int fd, i, n, e1, e2, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  for (i = 0; i < 8; ++i) {
#ifdef GD_NO_C99_API
    c[i][0] = 40 + i;
    c[i][1] = i;
#else
    c[i] = CMPLX(40 + i, i);
#endif
  }

  MAKEFORMATFILE(format, "data RAW COMPLEX64 8\n");

  D = gd_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);
  n = gd_putdata(D, "data", 5, 0, 1, 0, GD_COMPLEX64, c);
  e1 = gd_error(D);
  CHECKI(e1,0);
  CHECKI(n,8);

  e2 = gd_close(D);
  CHECKI(e2, 0);

  if (stat(data, &buf)) {
    perror("stat");
    r = 1;
  } else {
    CHECKI(buf.st_size, 48 * 2 * sizeof(float));

    fd = open(data, O_RDONLY | O_BINARY);
    i = 0;
#ifdef GD_NO_C99_API
    while (read(fd, d, 2 * sizeof(float)))
#else
      while (read(fd, &d, sizeof(float _Complex)))
#endif
      {
        if (i < 40 || i > 48) {
          CHECKCi(i,d,zero);
        } else {
#ifdef GD_NO_C99_API
          float v[] = {i, i - 40};
#else
          float _Complex v = CMPLX(i, i - 40);
#endif
          CHECKCi(i,d,v);
        }
        i++;
      }
    close(fd);
  }

  unlink(data);
  unlink(format);
  rmdir(filedir);

  return r;
}
