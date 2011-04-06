/* Copyright (C) 2010-2011 D. V. Wiebe
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
/* Add a RAW field */
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
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  uint8_t val[] = {0, 0, 0, 0, 0, 0, 0, 0};
  int error, r = 0, i;
  DIRFILE *D;

  D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_VERBOSE);
  gd_add_carray(D, "data", GD_UINT8, 8, GD_UINT8, &val, 0);
  for (i = 0; i < 8; ++i)
    val[i] = i * (i + 1);
  gd_put_carray(D, "data", GD_UINT8, &val);
  error = gd_error(D);
  gd_close(D);

  /* check */
  memset(val, 0, 8);
  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  gd_get_carray(D, "data", GD_UINT8, &val);
  gd_close(D);

  unlink(format);
  rmdir(filedir);

  for (i = 0; i < 8; ++i)
    CHECKIi(i, val[i], i * (i + 1));
  CHECKI(error,GD_E_OK);
  return r;
}
