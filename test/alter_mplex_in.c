/* Copyright (C) 2016 D. V. Wiebe
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
  int ret, error, r = 0;
  DIRFILE *D;
  gd_entry_t e;

  rmdirfile();
  mkdir(filedir, 0777);

  MAKEFORMATFILE(format, "mplex MPLEX data count 1 3\n");

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  ret = gd_alter_mplex(D, "mplex", "in1", "in2", 2, -1);
  CHECKI(ret, 0);
  error = gd_error(D);
  CHECKI(error, 0);

  gd_entry(D, "mplex", &e);
  CHECKS(e.in_fields[0], "in1");
  CHECKS(e.in_fields[1], "in2");
  gd_free_entry_strings(&e);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}