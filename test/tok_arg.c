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
/* Parser check */
#include "test.h"

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
  int e1, e2, r = 0;
  char *tok;
  DIRFILE *D;

  rmdirfile();

  D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_EXCL);
  e1 = gd_error(D);

  tok = gd_strtok(D, NULL);
  e2 = gd_error(D);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(e1,GD_E_OK);
  CHECKI(e2,GD_E_ARGUMENT);
  CHECKP(tok);
  return r;
}
