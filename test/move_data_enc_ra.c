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
  char line[100];
  int i, ret, e1, e2, ge_ret, unlink_data, unlink_txtdata;
  FILE* stream;
  gd_entry_t E;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "/INCLUDE format1\ndata RAW UINT16 11");
  MAKEFORMATFILE(format1, "ENCODING text\n");
  MAKEDATAFILE(data, uint16_t, i * 0x201, 128);

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

  stream = fopen(txtdata, "rt");

  if (stream != NULL) {
    i = 0;

    while (fgets(line, 100, stream)) {
      d = strtoul(line, NULL, 10);
      CHECKXi(i, d, (unsigned)i * 0x201);
      i++;
    }
    fclose(stream);
  } else {
    perror("open");
    r = 1;
  }

  unlink(format1);
  unlink(format);
  unlink_data = unlink(data);
  unlink_txtdata = unlink(txtdata);
  rmdir(filedir);

  CHECKI(unlink_data, -1);
  CHECKI(unlink_txtdata, 0);

  return r;
}
