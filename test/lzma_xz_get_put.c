/* Copyright (C) 2014, 2015 D. V. Wiebe
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
#ifndef TEST_LZMA
  return 77;
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data_xz = "dirfile/data.xz";
  const char *data = "dirfile/data";
  const char *format_data = "data RAW UINT8 8\n";
  const unsigned char xzdata[300] = {
    0xfd, 0x37, 0x7a, 0x58, 0x5a, 0x00, 0x00, 0x04,
    0xe6, 0xd6, 0xb4, 0x46, 0x02, 0x00, 0x21, 0x01,
    0x16, 0x00, 0x00, 0x00, 0x74, 0x2f, 0xe5, 0xa3,
    0xe0, 0x00, 0xff, 0x00, 0xea, 0x5d, 0x00, 0x00,
    0x00, 0x52, 0x50, 0x0a, 0x84, 0xf9, 0x9b, 0xb2,
    0x80, 0x21, 0xa9, 0x69, 0xd6, 0x27, 0xe0, 0x3e,
    0x06, 0x5a, 0x5f, 0x04, 0x8d, 0x53, 0xd4, 0x04,
    0xba, 0x39, 0x57, 0x05, 0x09, 0xc1, 0x55, 0x24,
    0xde, 0x9d, 0xb8, 0x71, 0x59, 0x31, 0x60, 0xa1,
    0x9f, 0xf9, 0x6f, 0x49, 0x73, 0xf2, 0xc8, 0xea,
    0x8c, 0xba, 0x1a, 0x8b, 0x29, 0x69, 0x21, 0x80,
    0xfe, 0x33, 0x83, 0x66, 0xaf, 0x46, 0x6d, 0xec,
    0x9e, 0x89, 0x8a, 0x0b, 0x83, 0xf0, 0x3c, 0x0e,
    0x89, 0x8e, 0x3f, 0xed, 0x5f, 0xe7, 0x9e, 0x90,
    0xd9, 0x1c, 0xff, 0x32, 0xf4, 0xb2, 0xe0, 0x39,
    0x51, 0xb2, 0xd2, 0x14, 0x15, 0xb4, 0xc5, 0x71,
    0xba, 0xdb, 0x06, 0xe3, 0x79, 0x9a, 0x9f, 0xbb,
    0x38, 0xc1, 0xb0, 0x00, 0xac, 0x93, 0x0b, 0xaa,
    0x06, 0x19, 0x03, 0x12, 0x08, 0x15, 0x5b, 0x9b,
    0xc8, 0x48, 0xf0, 0x32, 0x2e, 0xfe, 0x2d, 0xa0,
    0x87, 0xc8, 0xf0, 0xa4, 0xe0, 0xd2, 0x51, 0xeb,
    0x8d, 0x67, 0x56, 0x92, 0xb2, 0x4d, 0x84, 0xc5,
    0xf1, 0x86, 0x31, 0xdf, 0x6a, 0x62, 0x5b, 0xc2,
    0x79, 0x2d, 0xd9, 0xf7, 0x3c, 0x73, 0xba, 0x74,
    0x74, 0x07, 0xd8, 0x3c, 0xa9, 0x56, 0x22, 0x24,
    0xa1, 0x66, 0xf8, 0x5a, 0x84, 0x5f, 0x30, 0x67,
    0xd2, 0xf6, 0x4b, 0x49, 0x2e, 0x7f, 0x20, 0xeb,
    0xdb, 0xf8, 0x10, 0x0e, 0x94, 0x78, 0x77, 0xc7,
    0x3f, 0x6b, 0xef, 0xb4, 0xcd, 0x95, 0xe2, 0x6f,
    0xf6, 0x44, 0x6e, 0x06, 0xcf, 0x0b, 0x82, 0x1a,
    0xcb, 0xdb, 0x7a, 0xf0, 0x57, 0x8d, 0x98, 0xff,
    0x90, 0xc0, 0x3e, 0xe6, 0xc1, 0x12, 0x41, 0x75,
    0xee, 0x03, 0x28, 0x96, 0xeb, 0x09, 0x37, 0x0e,
    0x1e, 0x00, 0x00, 0x00, 0xb0, 0x3a, 0xdb, 0x65,
    0x2f, 0x4b, 0x41, 0x72, 0x00, 0x01, 0x86, 0x02,
    0x80, 0x02, 0x00, 0x00, 0x2d, 0x12, 0xa1, 0x9f,
    0xb1, 0xc4, 0x67, 0xfb, 0x02, 0x00, 0x00, 0x00,
    0x00, 0x04, 0x59, 0x5a
  };
  uint8_t c[8], d[8];
#ifdef USE_LZMA
  char command[4096];
#endif
  int fd, i, m, n, e1, e2, e3, unlink_data, unlink_dataxz, r = 0;
  DIRFILE *D;

  memset(c, 0, 8);
  rmdirfile();
  mkdir(filedir, 0777);

  for (i = 0; i < 8; ++i)
    d[i] = (uint8_t)(80 + i);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data_xz, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, xzdata, 300);
  close(fd);

#ifdef USE_LZMA
  D = gd_open(filedir, GD_RDWR | GD_LZMA_ENCODED | GD_VERBOSE);
#else
  D = gd_open(filedir, GD_RDWR | GD_LZMA_ENCODED);
#endif
  n = gd_getdata(D, "data", 5, 0, 1, 0, GD_UINT8, c);
  e1 = gd_error(D);
  m = gd_putdata(D, "data", 5, 0, 1, 0, GD_UINT8, d);
  e2 = gd_error(D);

  e3 = gd_close(D);
  CHECKI(e3, 0);

#ifdef USE_LZMA
  /* uncompress */
  snprintf(command, 4096, "%s --decompress -f %s > /dev/null", XZ, data_xz);
  if (gd_system(command)) {
    r = 1;
  } else {
    uint8_t b;
    fd = open(data, O_RDONLY | O_BINARY);
    if (fd >= 0) {
      i = 0;
      while (read(fd, &b, sizeof(uint8_t))) {
        if (i < 40 || i >= 48) {
          CHECKIi(i, b, i);
        } else
          CHECKIi(i, b, i + 40);
        i++;
      }
      CHECKI(i, 256);
      close(fd);
    }
  }

  for (i = 0; i < 8; ++i)
    CHECKIi(i, c[i], 40 + i);
#endif

  unlink_data = unlink(data);
  unlink_dataxz = unlink(data_xz);
  unlink(format);
  rmdir(filedir);

#ifdef USE_LZMA
  CHECKI(unlink_data, 0);
  CHECKI(unlink_dataxz, -1);
  CHECKI(e1, GD_E_OK);
  CHECKI(e2, GD_E_OK);
  CHECKI(n, 8);
  CHECKI(m, 8);
#else
  CHECKI(unlink_data, -1);
  CHECKI(unlink_dataxz, 0);
  CHECKI(e1, GD_E_UNSUPPORTED);
  CHECKI(e2, GD_E_UNSUPPORTED);
  CHECKI(n, 0);
  CHECKI(m, 0);
#endif

  return r;
#endif
}