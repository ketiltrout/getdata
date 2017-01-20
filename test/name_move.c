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
  const char *data = "dirfile/data";
  const char *zata = "dirfile/zata";
  int ret, e1, e2, unlink_data, unlink_zata, r = 0;
  const char **fl;
#define NFIELDS 4
  const char *field_list[NFIELDS] = {
    "cata", "eata", "zata", "INDEX"
  };
  unsigned i, nf;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
      "cata RAW UINT8 8\ndata RAW UINT8 8\neata RAW UINT8 8\n");
  MAKEDATAFILE(data, uint8_t, i, 256);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  ret = gd_rename(D, "data", "zata", GD_REN_DATA);
  e1 = gd_error(D);
  CHECKI(e1, 0);
  CHECKI(ret, 0);

  nf = gd_nfields(D);
  CHECKI(nf, NFIELDS);
  if (nf > NFIELDS)
    nf = NFIELDS;

  fl = gd_field_list(D);
  for (i = 0; i < nf; ++i)
    CHECKSi(i, fl[i], field_list[i]);

  e2 = gd_close(D);
  CHECKI(e2, 0);

  unlink_data = unlink(data);
  unlink_zata = unlink(zata);
  unlink(format);
  rmdir(filedir);

  CHECKI(unlink_data, -1);
  CHECKI(unlink_zata, 0);

  return r;
}
