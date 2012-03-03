/* Copyright (C) 2008-2012 D. V. Wiebe
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
/* Truncating a dirfile should succeed cleanly */
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
  const char *subdir = "dirfile/sub";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  int fd, error, unlink_data, rmdir_sub, stat_format, r = 0;
  struct stat buf;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0777);
  mkdir(subdir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format, strlen(format));
  close(fd);

  close(open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666));

  D = gd_open(filedir, GD_RDWR | GD_TRUNC | GD_TRUNCSUB | GD_VERBOSE);
  error = gd_error(D);
  gd_close(D);

  unlink_data = unlink(data);
  CHECKI(unlink_data, -1);

  stat_format = stat(format, &buf);
  CHECKI(stat_format, 0);
  CHECK((buf.st_size > 0),buf.st_size,"%lli","%s",(long long)buf.st_size,"> 0");

  rmdir_sub = rmdir(subdir);
  CHECKI(rmdir_sub, -1);

  unlink(format);
  rmdir(filedir);

  CHECKI(error,GD_E_OK);
  return r;
}
