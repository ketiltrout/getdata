/* Copyright (C) 2008-2011, 2013, 2017 D.V. Wiebe
 * Copyright (C) 2019 Matthew Petroff
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
/* Retreiving the number of frames should succeed cleanly */
#include "test.h"

int main(void)
{
#ifdef HAVE_ZZIP_LIB_H
  const char *filedir = "dirfile";
  const char *filedirzip = "dirfile.zip";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  const char *command = "zip -jq0 dirfile.zip dirfile/format dirfile/data";
  int fd, error, r = 0;
  const size_t len = strlen(data);
  off_t n;
  DIRFILE *D;

  rmdirfile();
  unlink(filedirzip);
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "/ENCODING none\ndata RAW UINT16 1\n");

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data, len);
  close(fd);

  if (gd_system(command))
    return 1;

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n = gd_nframes(D);
  error = gd_error(D);
  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);
  unlink(filedirzip);

  CHECKI(error, 0);
  CHECKI(n, (off_t)len / 2);

  return r;
#else
  return 77;
#endif
}
