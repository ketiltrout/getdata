/* Copyright (C) 2017 D.V. Wiebe
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
#ifndef TEST_GZIP
  return 77;
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data1 = "dirfile/data1.gz";
  const char *data2 = "dirfile/data2.gz";
  const char *data3 = "dirfile/data3.gz";
  int r = 0;
  long no;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  /* This is a gzipped zero length file */
  const char data[] = {
    0x1F, 0x8B, 0x08, 0x00,
    0x00, 0x00, 0x00, 0x00,
    0x00, 0xFF, 0x03, 0x00,
    0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00
  };

  MAKEFORMATFILE(format,
      "/ENCODING gzip\n"
      "data1 RAW UINT8 8\n"
      "data2 RAW UINT8 8\n"
      "data3 RAW UINT8 8\n"
      );
  MAKERAWFILE(data1, data, 20);
  MAKERAWFILE(data2, data, 20);
  MAKERAWFILE(data3, data, 20);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);

  gd_open_limit(D, 5);

  gd_seek(D, "data1", 0, 0, GD_SEEK_SET | GD_SEEK_WRITE);

  no = gd_open_limit(D, GD_OLIMIT_COUNT);
  CHECKIi(1, no, 2);

  gd_seek(D, "data2", 0, 0, GD_SEEK_SET);

  no = gd_open_limit(D, GD_OLIMIT_COUNT);
  CHECKIi(2, no, 3);

  gd_raw_close(D, "data2");

  no = gd_open_limit(D, GD_OLIMIT_COUNT);
  CHECKIi(3, no, 2);

  gd_seek(D, "data2", 0, 0, GD_SEEK_SET | GD_SEEK_WRITE);

  no = gd_open_limit(D, GD_OLIMIT_COUNT);
  CHECKIi(4, no, 4);

  gd_seek(D, "data3", 0, 0, GD_SEEK_SET | GD_SEEK_WRITE);

  no = gd_open_limit(D, GD_OLIMIT_COUNT);
  CHECKIi(5, no, 4);

  gd_discard(D);

  unlink(data1);
  unlink(data2);
  unlink(data3);
  unlink(format);
  rmdir(filedir);

  return r;
#endif
}
