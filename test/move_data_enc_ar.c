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
/* Test move */
#include "test.h"

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *format1 = "dirfile/format1";
  const char *data = "dirfile/data";
  const char *txtdata = "dirfile/data.txt";
  int r = 0;
  uint16_t d;
  int fd, i, ret, e1, e2, ge_ret, unlink_data, unlink_txtdata;
  FILE* stream;
  gd_entry_t E;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "/INCLUDE format1\ndata RAW UINT16 11\nENCODING text\n");
  MAKEFORMATFILE(format1, "ENCODING none\n");

  stream = fopen(txtdata, "w");
  for (i = 0; i < 128; ++i)
    fprintf(stream, "%i\n", i * 0x201);
  fclose(stream);

  D = gd_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);
  ret = gd_move(D, "data", 1, GD_REN_DATA);
  e1 = gd_error(D);
  CHECKI(ret, 0);
  CHECKI(e1, GD_E_OK);

  ge_ret =  gd_entry(D, "data", &E);
  CHECKI(ge_ret, 0);
  CHECKI(E.fragment_index, 1);
  gd_free_entry_strings(&E);

  e2 = gd_close(D);
  CHECKI(e2, 0);

  fd = open(data, O_RDONLY | O_BINARY);
  i = 0;

  if (fd >= 0) {
    while (read(fd, &d, sizeof(uint16_t))) {
      CHECKXi(i,d,i * 0x201);
      i++;
    }
    close(fd);
  } else {
    perror("open");
    r = 1;
  }

  unlink(format1);
  unlink(format);
  unlink_data = unlink(data);
  unlink_txtdata = unlink(txtdata);
  rmdir(filedir);

  CHECKI(unlink_data, 0);
  CHECKI(unlink_txtdata, -1);

  return r;
}
