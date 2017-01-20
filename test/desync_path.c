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
#include "test.h"

int main(void)
{
#if ! defined HAVE_SYMLINK
  return 77;
#else
  const char *filedir = "dirfile";
  const char *dira = "dirfile/a";
  const char *dirb = "dirfile/b";
  const char *link = "dirfile/link";
  const char *formata = "dirfile/a/format";
  const char *formatb = "dirfile/b/format";
  int e1, e2, e3, n1, n2, n3, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);
  mkdir(dira, 0700);
  mkdir(dirb, 0700);
  symlink("a", link);
  MAKEEMPTYFILE(formata, 0600);
  /* ensure mtime ticks over */
  sleep(1);
  MAKEEMPTYFILE(formatb, 0600);

  D = gd_open(link, GD_RDONLY | GD_VERBOSE);

  n1 = gd_desync(D, 0);
  e1 = gd_error(D);
  
  /* change symlink */
  unlink(link);
  symlink("b", link);

  n2 = gd_desync(D, 0);
  e2 = gd_error(D);

  n3 = gd_desync(D, GD_DESYNC_PATHCHECK);
  e3 = gd_error(D);

  gd_discard(D);

  unlink(formata);
  unlink(formatb);
  unlink(link);
  rmdir(dira);
  rmdir(dirb);
  rmdir(filedir);

  CHECKI(e1, 0);
  CHECKI(e2, 0);
  CHECKI(e3, 0);
  CHECKI(n1, 0);
  CHECKI(n2, 0);
  CHECKI(n3, 1);
  return r;
#endif
}
