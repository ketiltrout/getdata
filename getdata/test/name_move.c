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
#include "test.h"

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  const char *zata = "dirfile/zata";
  const char *format_data = "cata RAW UINT8 8\ndata RAW UINT8 8\n"
    "eata RAW UINT8 8\n";
  unsigned char data_data[256];
  int fd, ret, error, unlink_data, unlink_zata, r = 0;
  const char **fl;
  char *field_list[4];
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (unsigned char)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 256);
  close(fd);

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
  ret = gd_rename(D, "data", "zata", GD_REN_DATA);
  error = gd_error(D);
  fl = gd_field_list(D);

  field_list[0] = strdup(fl[0]);
  field_list[1] = strdup(fl[1]);
  field_list[2] = strdup(fl[2]);
  field_list[3] = strdup(fl[3]);

  gd_close(D);

  unlink_data = unlink(data);
  unlink_zata = unlink(zata);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKI(ret, 0);
  CHECKS(field_list[0], "INDEX");
  CHECKS(field_list[1], "cata");
  CHECKS(field_list[2], "eata");
  CHECKS(field_list[3], "zata");
  CHECKI(unlink_data, -1);
  CHECKI(unlink_zata, 0);
  free(field_list[0]);
  free(field_list[1]);
  free(field_list[2]);
  free(field_list[3]);

  return r;
}
