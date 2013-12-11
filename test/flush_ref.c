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
/* Test include */
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
  const char *subdir = "dirfile/sub";
  const char *format = "dirfile/format";
  const char *format1 = "dirfile/sub/format";
  const char *format_data = "data RAW UINT8 1\nREFERENCE data\n";
  const char *format1_data = "mata RAW UINT8 11\nREFERENCE mata\n";
  int fd, e1, e2, r = 0;
  const char *r1;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0777);
  mkdir(subdir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(format1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format1_data, strlen(format1_data));
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  gd_include_affix(D, "sub/format", 0, "prefix_", NULL, GD_IGNORE_REFS);
  gd_rewrite_fragment(D, GD_ALL_FRAGMENTS);

  e2 = gd_close(D);
  CHECKI(e2, 0);

  D = gd_open(subdir, GD_RDONLY | GD_VERBOSE);
  r1 = gd_reference(D, NULL);
  CHECKS(r1, "mata");
  e1 = gd_error(D);
  gd_discard(D);

  unlink(format1);
  unlink(format);
  rmdir(filedir);

  CHECKI(e1, 0);

  return r;
}
