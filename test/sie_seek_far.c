/* Copyright (C) 2015, 2017 D.V. Wiebe
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
  const uint8_t data_data[] = {
    0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x12,
    0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x22,
    0x30, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x32
  };
  DIRFILE *D;
  int fd, n, error, r = 0;

  rmdirfile();
  mkdir(filedir, 0700); 

  MAKEFORMATFILE(format, "data RAW UINT8 8\n/ENCODING sie\n/ENDIAN little\n");

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 3 * 9 * sizeof(unsigned char));
  close(fd);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);

  n = gd_seek(D, "data", 500, 0, GD_SEEK_SET);
  CHECKI(n, 4000);

  error = gd_error(D);
  CHECKI(error, 0);

  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);


  return r;
}
