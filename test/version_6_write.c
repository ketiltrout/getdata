/* Copyright (C) 2010-2011 D. V. Wiebe
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
  const char *format_data =
    "/VERSION 6\n"
    "b\\ c CONST UINT8 8\n"
    "\"a \\#r\" RAW UINT8 b\\ c\n"
    "/META \"a \\#r\" \\x64\\c\\157 PHASE \"a \\#r\" 0\n";
  int fd, e, q, c, r = 0;
  DIRFILE *D;
  gd_spf_t s;

  rmdirfile();
  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  e = gd_dirfile_standards(D, 6);
  q = gd_rewrite_fragment(D, 0);
  gd_close(D);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  c = gd_dirfile_standards(D, GD_VERSION_CURRENT);
  s = gd_spf(D, "a #r/dco");
  gd_close(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(e,6);
  CHECKI(q,0);
  CHECKI(c,6);
  CHECKU(s,8);

  return r;
}
