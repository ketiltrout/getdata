/* Copyright (C) 2008-2011, 2013, 2016, 2017 D.V. Wiebe
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
  int error, i, r = 0;
  const char **fl;
  DIRFILE *D;
#define NFIELDS 12
  const char *field_list[NFIELDS] = {
    "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "INDEX"
  };

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
      "c RAW UINT8 1\n"
      "d RAW UINT8 1\n"
      "g RAW UINT8 1\n"
      "h RAW UINT8 1\n"
      "i RAW UINT8 1\n"
      "k RAW UINT8 1\n"
      "f RAW UINT8 1\n"
      "b RAW UINT8 1\n"
      "a RAW UINT8 1\n"
      "j RAW UINT8 1\n"
      "e RAW UINT8 1\n");

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  fl = gd_field_list(D);

  error = gd_error(D);
  CHECKI(error, 0);

  for (i = 0; fl[i]; ++i)
    CHECKSi(i, fl[i], field_list[i]);
  CHECKI(i, NFIELDS);

  gd_discard(D);
  unlink(format);
  rmdir(filedir);

  return r;
}
