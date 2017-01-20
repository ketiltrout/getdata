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
/* this tests whether _GD_CanonicalPath can deal with bad intermediate
 * symlinks */
#include "test.h"

int main(void)
{
#if ! defined HAVE_SYMLINK || defined GD_NO_GETCWD
  return 77;
#else
  const char *link = "dirfile/link";
  const char *filedir = "dirfile/link/dirfile";
  int error, r = 0;
  int cwd_size = 2048;
  char *ptr, *cwd = NULL;
  DIRFILE *D;

  gdtest_getcwd(ptr, cwd, cwd_size);

  rmdirfile();
  mkdir("dirfile", 0700);

  /* make a bad symlink */
  symlink("non_existent", link);

  D = gd_open(filedir, GD_RDONLY);
  error = gd_error(D);
  ptr = gd_error_string(D, NULL, 0);
  CHECKI(error, GD_E_IO);
  CHECKSS(ptr, "dirfile/non_existent/dirfile");
  free(ptr);

  gd_discard(D);

  unlink(link);
  rmdir("dirfile");

  free(cwd);
  return r;
#endif
}
