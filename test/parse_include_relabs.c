/* Copyright (C) 2011-2013, 2017 D.V. Wiebe
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

int main(void)
{
#if defined GD_NO_GETCWD
  return 77;
#else
  const char *filedir = "dirfile";
  const char *subdir = "dirfile/sub";
  const char *format = "dirfile/format";
  const char *format1 = "dirfile/sub/format1";
  const char *format2 = "dirfile/sub/format2";
  const char *format1_data1 = "INCLUDE ";
  const char *format1_data2 = "/dirfile/sub/format2\n";
  int cwd_size = 2048;
  char *ptr, *cwd = NULL;
  int fd, r = 0;
  DIRFILE *D;
  unsigned int spf;

  rmdirfile();
  mkdir(filedir, 0700);
  mkdir(subdir, 0700);

  gdtest_getcwd(ptr, cwd, cwd_size);

  MAKEFORMATFILE(format, "INCLUDE sub/format1\n");

  fd = open(format1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format1_data1, strlen(format1_data1));
  gd_pathwrite(fd, cwd);
  write(fd, format1_data2, strlen(format1_data2));
  close(fd);

  MAKEFORMATFILE(format2, "data RAW UINT8 11\n");

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  spf = gd_spf(D, "data");
  gd_discard(D);

  unlink(format2);
  unlink(format1);
  unlink(format);
  rmdir(subdir);
  rmdir(filedir);

  CHECKU(spf, 11);
  free(cwd);
  return r;
#endif
}
