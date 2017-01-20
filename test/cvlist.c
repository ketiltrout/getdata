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
/* Retreiving the list of constant values should succeed cleanly */
#include "test.h"

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  int fd, error, r = 0;
  const uint8_t *field_list;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "data1 CONST UINT8 1\n"
    "data2 CONST UINT8 2\n"
    "data3 CONST UINT8 3\n"
    "data4 RAW UINT8 1\n"
  );

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  field_list = (const uint8_t *)gd_constants(D, GD_UINT8);

  error = gd_error(D);

  CHECKI(error, 0);

  if (!r)
    for (fd = 0; fd < 3; ++fd)
      CHECKUi(fd,field_list[fd], fd + 1);

  gd_discard(D);
  unlink(format);
  rmdir(filedir);

  return r;
}
