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
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *format1 = "dirfile/format1";
  const char *data = "dirfile/data";
  uint8_t d;
  int fd, i, ret, error, ge_ret, r = 0;
  gd_entry_t E;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "/INCLUDE format1\n"
    "data RAW UINT8 11\n"
    "/FRAMEOFFSET 1\n"
  );
  MAKEFORMATFILE(format1, "FRAMEOFFSET 0\n");
  MAKEDATAFILE(data, uint8_t, i, 256);

  D = gd_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);

  ret = gd_move(D, "data", 1, GD_REN_DATA);
  error = gd_error(D);
  ge_ret =  gd_entry(D, "data", &E);
  gd_discard(D);

  fd = open(data, O_RDONLY | O_BINARY);
  i = 0;
  while (read(fd, &d, sizeof(uint8_t))) {
    CHECKUi(i,d, (i < 11) ? 0 : i - 11);
    i++;
  }
  close(fd);

  unlink(format1);
  unlink(format);
  unlink(data);
  rmdir(filedir);

  CHECKI(ret, 0);
  CHECKI(error, GD_E_OK);
  CHECKI(ge_ret, 0);
  CHECKI(E.fragment_index, 1);
  gd_free_entry_strings(&E);

  return r;
}
