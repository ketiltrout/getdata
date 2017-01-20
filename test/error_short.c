/* Copyright (C) 2008-2011, 2013, 2017 D.V. Wiebe
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
/* a short error string should still be NULL-terminated */
#include "test.h"

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  char string[1000] = "abc";
  int r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);
  MAKEEMPTYFILE(format, 0600);

  D = gd_open(filedir, GD_RDONLY);
  gd_error_string(D, string, 2);
  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(string[1], 0);
  
  return r;
}
