/* Copyright (C) 2013 D. V. Wiebe
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

#include <inttypes.h>
#include <stdlib.h>
#include <sys/types.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>

#define S "a\"b\\c#d e\tf\ng"
int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  int e1, e0, r = 0;
  char s[100] = {0};
  DIRFILE *D;

  rmdirfile();
  D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_TRUNC |
      GD_VERBOSE);
  gd_add_string(D, "s", S, 0);

  e0 = gd_close(D);
  CHECKI(e0, 0);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  gd_get_string(D, "s", 100, s);
  e1 = gd_error(D);
  CHECKI(e1, GD_E_OK);

  gd_discard(D);

  CHECKS(s, S);

  unlink(format);
  rmdir(filedir);


  return r;
}
