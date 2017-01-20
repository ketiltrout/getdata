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
/* Attempt to write arm-endian COMPLEX64 */
#include "test.h"

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  unsigned int i;
#ifdef GD_NO_C99_API
  const float c[] = {1.5, 2.25};
#else
  const float complex c = 1.5 + _Complex_I * 2.25;
#endif
  unsigned char x[2 * sizeof(float)] = {
    0x00, 0x00, 0xC0, 0x3F, 0x00, 0x00, 0x10, 0x40
  };
  unsigned char u[2 * sizeof(float)];
  int fd, n, error, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700); 

  MAKEFORMATFILE(format, "data RAW COMPLEX64 1\nENDIAN little arm\n");

  D = gd_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);
#ifdef GD_NO_C99_API
  n = gd_putdata(D, "data", 5, 0, 1, 0, GD_COMPLEX64, c);
#else
  n = gd_putdata(D, "data", 5, 0, 1, 0, GD_COMPLEX64, &c);
#endif
  error = gd_error(D);

  gd_discard(D);

  fd = open(data, O_RDONLY);
  lseek(fd, 5 * 2 * sizeof(float), SEEK_SET);
  read(fd, u, 2 * sizeof(float));
  close(fd);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(n, 1);
  
  for (i = 0; i < 2 * sizeof(float); ++i)
    CHECKXi(i, u[i], x[i]);

  return r;
}
