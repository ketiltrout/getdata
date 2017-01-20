/* Copyright (C) 2010-2011, 2013, 2017 D.V. Wiebe
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
  const char *format1 = "dirfile/RAW";
  const char *format_data1 = "VERSION PHASE ENDIAN 1\na&b RAW c 8\n";
  int fd, e, e2, q, c, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "ENDIAN RAW c 8\nINCLUDE RAW\n");

  fd = open(format1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data1, strlen(format_data1));
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  e = gd_dirfile_standards(D, 4);
  q = gd_rewrite_fragment(D, GD_ALL_FRAGMENTS);
  CHECKI(e,4);
  CHECKI(q,0);

  e2 = gd_close(D);
  CHECKI(e2, 0);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  c = gd_dirfile_standards(D, GD_VERSION_EARLIEST);
  CHECKI(c,4);

  gd_discard(D);

  unlink(format);
  unlink(format1);
  rmdir(filedir);

  return r;
}
