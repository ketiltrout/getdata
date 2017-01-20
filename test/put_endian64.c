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
/* Attempt to write UINT64 with the opposite endianness */
#include "test.h"

static int BigEndian(void)
{
  union {
    long int li;
    char ch[sizeof(long int)];
  } un;
  un.li = 1;
  return (un.ch[sizeof(long int) - 1] == 1);
}

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  char format_data[1000];
  uint64_t c = 0x0203000000040001, d = 0;
  const int big_endian = BigEndian();
  int fd, n, error, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700); 

  sprintf(format_data, "data RAW UINT64 1\nENDIAN %s\n", (big_endian)
      ? "little" : "big");

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);
  n = gd_putdata(D, "data", 5, 0, 1, 0, GD_UINT64, &c);
  error = gd_error(D);

  gd_discard(D);

  fd = open(data, O_RDONLY | O_BINARY);
  lseek(fd, 5 * sizeof(uint64_t), SEEK_SET);
  read(fd, &d, sizeof(uint64_t));
  close(fd);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKX(d,0x0100040000000302);
  CHECKI(n,1);
  CHECKI(error,0);

  return r;
}
