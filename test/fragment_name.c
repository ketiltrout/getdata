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
#include "test.h"

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
  const char *format1 = "dirfile/format1";
  const char *format_data = "INCLUDE format1\n";
  const char *format1_data = "data RAW UINT8 11\n";
  char *form0 = NULL;
  char *form1 = NULL;
  int fd, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(format1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format1_data, strlen(format1_data));
  close(fd);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  form0 = strdup(gd_fragmentname(D, 0));
  form1 = strdup(gd_fragmentname(D, 1));
  gd_close(D);

  unlink(format1);
  unlink(format);
  rmdir(filedir);

  /* This only checks whether the end of the returned path is what we expect.
   * This should work, since we can guarantee that both "dirfile" and "format*"
   * aren't symlinks. */
#if GD_DIRSEP == '/'
  CHECKEOS(form0,"dirfile/format");
  CHECKEOS(form1,"dirfile/format1");
#else
  CHECKEOS(form0,"dirfile\\format");
  CHECKEOS(form1,"dirfile\\format1");
#endif
  free(form0);
  free(form1);

  return r;
}
