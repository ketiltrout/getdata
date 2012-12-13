/* Copyright (C) 2008-2011 D. V. Wiebe
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

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <inttypes.h>

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *format1 = "dirfile/format1";
  const char *data = "dirfile/data";
  const char *txtdata = "dirfile/data.txt";
  const char *format_data = "/INCLUDE format1\ndata RAW UINT16 11";
  const char *format1_data = "ENCODING text\n";
  uint16_t data_data[128];
  int r = 0;
  uint16_t d;
  char line[100];
  int fd, i, ret, error, ge_ret, unlink_data, unlink_txtdata;
  FILE* stream;
  gd_entry_t E;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0777);

  for (fd = 0; fd < 128; ++fd)
    data_data[fd] = fd * 0x201;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(format1, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format1_data, strlen(format1_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 256);
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_UNENCODED | GD_VERBOSE);
  ret = gd_move(D, "data", 1, 1);
  error = gd_error(D);
  ge_ret =  gd_entry(D, "data", &E);
  gd_close(D);

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

  CHECKI(ret, 0);
  CHECKI(error, GD_E_OK);
  CHECKI(ge_ret, 0);
  CHECKI(E.fragment_index, 1);
  CHECKI(unlink_data, -1);
  CHECKI(unlink_txtdata, 0);
  gd_free_entry_strings(&E);

  return r;
}
