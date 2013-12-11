/* Copyright (C) 2008-2011, 2013 D. V. Wiebe
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
/* Getting data from an invalid dirfile should fail cleanly */
#include "test.h"

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>

int main(void)
{
  const char *filedir = "dirfile";
  char c[8];
  int error, r = 0;
  size_t n;
  DIRFILE *D;

  rmdirfile();
  D = gd_open(filedir, GD_RDONLY);
  n = gd_getdata(D, "data", 5, 0, 1, 0, GD_UINT8, c);
  error = gd_error(D);
  gd_discard(D);

  CHECKI(n, 0);
  CHECKI(error, GD_E_BAD_DIRFILE);

  return r;
}
