/* Copyright (C) 2011, 2013, 2017 D.V. Wiebe
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
/* Attempt to decompress an SIE file */
#include "test.h"

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data_sie = "dirfile/data.sie";
  const char *data_raw = "dirfile/data";
  uint8_t check[0x31];
  const uint8_t data_in[] = {
    0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x12,
    0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x22,
    0x30, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x32
  };
  const uint8_t data_out[] = {
    0x12, 0x12, 0x12, 0x12, 0x12, 0x12, 0x12, 0x12,
    0x12, 0x12, 0x12, 0x12, 0x12, 0x12, 0x12, 0x12, 0x12,
    0x22, 0x22, 0x22, 0x22, 0x22, 0x22, 0x22, 0x22,
    0x22, 0x22, 0x22, 0x22, 0x22, 0x22, 0x22, 0x22,
    0x32, 0x32, 0x32, 0x32, 0x32, 0x32, 0x32, 0x32,
    0x32, 0x32, 0x32, 0x32, 0x32, 0x32, 0x32, 0x32
  };
  DIRFILE *D;
  int fd, i, e1, e2, r = 0, unlink_data_sie, unlink_data_raw;

  rmdirfile();
  mkdir(filedir, 0700); 

  MAKEFORMATFILE(format, "data RAW UINT8 8\n/ENCODING sie\n/ENDIAN little\n");

  fd = open(data_sie, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_in, 3 * 9 * sizeof(unsigned char));
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  gd_alter_encoding(D, GD_UNENCODED, 0, 1);
  e1 = gd_error(D);
  CHECKI(e1, 0);

  e2 = gd_close(D);
  CHECKI(e2, 0);

  fd = open(data_raw, O_RDONLY | O_BINARY);
  if (fd < 0) {
    perror("open");
    r = 1;
  } else {
    read(fd, check, 0x31);
    close(fd);

    for (i = 0; i < 0x31; ++i)
      CHECKUi(i, check[i], data_out[i]);
  }

  unlink_data_sie = unlink(data_sie);
  unlink_data_raw = unlink(data_raw);
  unlink(format);
  rmdir(filedir);

  CHECKI(unlink_data_sie, -1);
  CHECKI(unlink_data_raw, 0);

  return r;
}
