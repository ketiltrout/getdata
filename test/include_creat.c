/* Copyright (C) 2008-2011, 2013, 2017 D.V. Wiebe
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
  const char *format1 = "dirfile/format1";
  int fd, e1, e2, unlink_format1, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  gd_include(D, "format1", 0, GD_CREAT);
  e1 = gd_error(D);
  CHECKI(e1,0);

  e2 = gd_close(D);
  CHECKI(e2, 0);

  unlink_format1 = unlink(format1);
  unlink(format);
  rmdir(filedir);
  
  CHECKI(unlink_format1, 0);

  return r;
}
