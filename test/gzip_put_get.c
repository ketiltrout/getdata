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
/* Attempt to write UINT8 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "test.h"

#include <inttypes.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>

int main(void)
{
#ifndef TEST_GZIP
  return 77;
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data.gz";
  const char *format_data = "data RAW UINT8 8\n";
  uint8_t c[8], d[8];
  int fd, i, m, n, e1, e2, unlink_data, r = 0;
  DIRFILE *D;

  memset(c, 0, 8);
  rmdirfile();
  mkdir(filedir, 0777);

  for (i = 0; i < 8; ++i)
    c[i] = (uint8_t)(40 + i);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

#ifdef USE_GZIP
  D = gd_open(filedir, GD_RDWR | GD_GZIP_ENCODED | GD_VERBOSE);
#else
  D = gd_open(filedir, GD_RDWR | GD_GZIP_ENCODED);
#endif
  n = gd_putdata(D, "data", 5, 0, 1, 0, GD_UINT8, c);
  e1 = gd_error(D);
  m = gd_getdata(D, "data", 5, 0, 1, 0, GD_UINT8, d);
  e2 = gd_error(D);

  gd_close(D);

  unlink_data = unlink(data);
  unlink(format);
  rmdir(filedir);

#ifdef USE_GZIP
  CHECKI(unlink_data, 0);
  CHECKI(e1, GD_E_OK);
  CHECKI(e2, GD_E_OK);
  CHECKI(n, 8);
  CHECKI(m, 8);
#else
  CHECKI(unlink_data, -1);
  CHECKI(e1, GD_E_UNSUPPORTED);
  CHECKI(e2, GD_E_UNSUPPORTED);
  CHECKI(n, 0);
  CHECKI(m, 0);
#endif
  
  for (i = 0; i < m; ++i)
    CHECKIi(i, d[i], c[i]);

  return r;
#endif
}