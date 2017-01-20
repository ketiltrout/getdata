/* Copyright (C) 2013, 2017 D.V. Wiebe
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
  const char *filedir = "dirfile";
  const char *subdir = "dirfile/sub";
  const char *format = "dirfile/format";
  const char *format1 = "dirfile/sub/format";
  int e1, e2, r = 0;
  const char *r1;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);
  mkdir(subdir, 0700);

  MAKEFORMATFILE(format, "data RAW UINT8 1\nREFERENCE data\n");
  MAKEFORMATFILE(format1, "mata RAW UINT8 11\nREFERENCE mata\n");

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
