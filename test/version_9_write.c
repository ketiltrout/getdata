/* Copyright (C) 2010-2011 D. V. Wiebe
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
  const char *format[] = {
    "dirfile/format",
    "dirfile/format1",
    "dirfile/format2",
    "dirfile/format3",
    "dirfile/format4"
  };
  const char *data = "dirfile/ar";
  const char *format_data[] = {
    "/VERSION 8\n"
      "/INCLUDE format1\n",
    "\n\n\n\n\n"
      "/VERSION 9\n"
      "Xr RAW COMPLEX128 0xA\n"
      "Xy POLYNOM INDEX 8 055 0xAE 2\n"
      "ar WINDOW d INDEX SET 0x1\n"
      "/ALIAS BINDEX INDEX\n"
      "/ALIAS AINDEXYZ BINDEX\n"
      "/INCLUDE format2 A Z\n"
      "/ALIAS m AdYZ\n"
      "/INCLUDE format4\n"
      "/ALIAS z n\n"
      "/ALIAS d z\n"
      "/HIDDEN Xy\n",
    "/INCLUDE format3 \"\" Y\n",
    "/ALIAS d INDEX\n/HIDDEN d\n",
    "/ALIAS n m\n"
  };
  uint16_t c[8];
  unsigned char data_data[256];
  int fd, i, error, n, v, r = 0;
  DIRFILE *D;

  memset(c, 0, 16);
  rmdirfile();
  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (unsigned char)fd;

  for (i = 0; i < 5; ++i) {
    fd = open(format[i], O_CREAT | O_EXCL | O_WRONLY, 0666);
    write(fd, format_data[i], strlen(format_data[i]));
    close(fd);
  }

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 256);
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  gd_rewrite_fragment(D, GD_ALL_FRAGMENTS);
  error = gd_error(D);
  gd_close(D);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  v = gd_dirfile_standards(D, GD_VERSION_CURRENT);
  n = gd_getdata(D, "ar", 4, 0, 8, 0, GD_UINT16, c);

  gd_close(D);

  unlink(data);
  for (i = 0; i < 5; ++i)
    unlink(format[i]);
  rmdir(filedir);

  CHECKI(error,0);
  CHECKI(n,8);
  CHECKI(v,9);

  for (i = 0; i < n; ++i)
    CHECKUi(i,c[i], (i & 1) ? 4 + i : 0);

  return r;
}
