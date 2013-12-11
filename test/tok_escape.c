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
  const char *spec = "string STRING \\ value";
  int e1, e2, e3, e4, e5, r = 0;
  char *tok1, *tok2, *tok3, *tok4;
  DIRFILE *D;

  rmdirfile();

  D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_EXCL | GD_VERBOSE);
  e1 = gd_error(D);
  CHECKI(e1,GD_E_OK);

  tok1 = gd_strtok(D, spec);
  e2 = gd_error(D);
  CHECKI(e2,GD_E_OK);
  CHECKS(tok1,"string");
  free(tok1);

  tok2 = gd_strtok(D, NULL);
  e3 = gd_error(D);
  CHECKI(e3,GD_E_OK);
  CHECKS(tok2,"STRING");
  free(tok2);

  tok3 = gd_strtok(D, NULL);
  e4 = gd_error(D);
  CHECKI(e4,GD_E_OK);
  CHECKS(tok3," value");
  free(tok3);

  tok4 = gd_strtok(D, NULL);
  e5 = gd_error(D);
  CHECKI(e5,GD_E_OK);
  CHECKP(tok4);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
