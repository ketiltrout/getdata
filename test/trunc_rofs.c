/* Copyright (C) 2008-2011, 2017 D.V. Wiebe
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
/* Truncating a read-only dirfile should fail cleanly */
#include "test.h"

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  int error, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);
  MAKEEMPTYFILE(format, 0600);
  MAKEEMPTYFILE(data, 0600);
  chmod(filedir, 0500);

  /* ensure filesystem honours read-onlyness */
  if (!unlink(data) || errno != EACCES) {
    unlink(format);
    rmdir(filedir);
    return 77;
  }

  D = gd_open(filedir, GD_RDWR | GD_TRUNC);
  error = gd_error(D);
  gd_discard(D);

  chmod(filedir, 0700);
  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, GD_E_TRUNC);
  return r;
}
