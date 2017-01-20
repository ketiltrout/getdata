/* Copyright (C) 2013, 2015, 2017 D.V. Wiebe
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

/* this tests handling absolute directories in _GD_GrabDir and also test
 * garbage collection in _GD_ReleaseDir */
#include "test.h"

int main(void)
{
#if defined GD_NO_GETCWD
  return 77;
#else
  const char *filedir = "dirfile";
  const char *lutdir = "dirfile/lut";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  const char *table = "dirfile/lut/table";
  const char *format_data1 = "linterp LINTERP data ";
  const char *format_data2 = "/dirfile/lut/table\ndata RAW UINT8 1\n";
  unsigned char c = 0;
  int fd, i, n, error, r = 0;
  DIRFILE *D;
  FILE *t;
  int cwd_size = 2048;
  char *ptr, *cwd = NULL;

  rmdirfile();
  mkdir(filedir, 0700);
  mkdir(lutdir, 0700);

  gdtest_getcwd(ptr, cwd, cwd_size);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data1, strlen(format_data1));
  gd_pathwrite(fd, cwd);
  write(fd, format_data2, strlen(format_data2));
  close(fd);

  MAKEDATAFILE(data, unsigned char, i, 64);

  t = fopen(table, "wt");
  for (i = 0; i < 30; ++i)
    fprintf(t, "%i %i\n", i * 6, i * 12);
  fclose(t);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n = gd_getdata(D, "linterp", 5, 0, 1, 0, GD_UINT8, &c);
  error = gd_error(D);

  gd_discard(D);

  unlink(table);
  unlink(data);
  unlink(format);
  rmdir(lutdir);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(n, 1);
  CHECKU(c, 10);

  free(cwd);

  return r;
#endif
}
