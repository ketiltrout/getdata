/* Copyright (C) 2008-2011, 2013, 2015, 2017 D.V. Wiebe
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
  int fd, e1, e2, unlink_data, stat_format, r = 0;
  struct stat buf;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format, strlen(format));
  close(fd);

  MAKEEMPTYFILE(data, 0600);

  D = gd_open(filedir, GD_RDWR | GD_TRUNC | GD_VERBOSE);
  e1 = gd_error(D);
  CHECKI(e1, GD_E_OK);

  e2 = gd_close(D);
  CHECKI(e2, 0);

  unlink_data = unlink(data);
  CHECKI(unlink_data, -1);

  stat_format = stat(format, &buf);
  CHECKI(stat_format, 0);
  CHECK((buf.st_size > 0),buf.st_size,"%" PRId64,"%s",(int64_t)buf.st_size,
      "> 0");

  unlink(format);
  rmdir(filedir);

  return r;
}
