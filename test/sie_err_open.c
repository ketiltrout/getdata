/* Copyright (C) 2016, 2017 D.V. Wiebe
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
  const char *data = "dirfile/data.sie";
  unsigned char c[16];
  const uint8_t data_data[] = {
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x12,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x20, 0x22,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x30, 0x32
  };
  DIRFILE *D;
  int fd, e1, e2, r = 0;

  rmdirfile();
  mkdir(filedir, 0700); 

  MAKEFORMATFILE(format, "data RAW UINT8 8\n/ENCODING sie\n/ENDIAN big\n");

  D = gd_open(filedir, GD_RDONLY);
  gd_getdata(D, "data", 3, 0, 2, 0, GD_UINT8, c);
  e1 = gd_error(D);
  CHECKI(e1, GD_E_IO);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 3 * 9 * sizeof(unsigned char));
  close(fd);

  gd_getdata(D, "data", 3, 0, 2, 0, GD_UINT8, c);
  e2 = gd_error(D);
  CHECKI(e2, GD_E_OK);

  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  return r;
}
