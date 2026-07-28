/* Copyright (C) 2014, 2017 D.V. Wiebe
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
  const char *encdata = "dirfile/data" ENC_SUFFIX;
  const char *data_raw = "dirfile/data";
  uint8_t data_in[256];
  DIRFILE *D;
#ifdef USE_ENC
  uint8_t d;
  char command[4096];
  int i;
#endif
  int fd, e1, e2, unlink_raw, r = 0;
  struct stat buf;

  rmdirfile();
  mkdir(filedir, 0700); 

  for (fd = 0; fd < 256; ++fd)
    data_in[fd] = (unsigned char)fd;

  MAKEFORMATFILE(format, "data RAW UINT8 8\n/ENCODING none\n/ENDIAN little\n");

  fd = open(data_raw, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_in, 256);
  close(fd);

#ifdef USE_ENC
  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);
#else
  D = gd_open(filedir, GD_RDWR);
#endif
  gd_alter_encoding(D, ENC_ENCODING, 0, 1);
  e1 = gd_error(D);

  e2 = gd_close(D);
  CHECKI(e2, 0);

#ifdef USE_ENC
  if (stat(encdata, &buf)) {
    perror("stat");
    r = 1;
  }
  CHECKI(stat(data_raw, &buf), -1);
#else
  if (stat(data_raw, &buf)) {
    perror("stat");
    r = 1;
  }
  CHECKI(stat(encdata, &buf), -1);
#endif

#ifdef USE_ENC
  /* uncompress */
  ENC_COMPRESS;
  if (gd_system(command)) {
    fprintf(stderr, "command failed: %s\n", command);
    r = 1;
  } else {
    fd = open(data_raw, O_RDONLY | O_BINARY);
    if (fd >= 0) {
      i = 0;
      while (read(fd, &d, sizeof(uint8_t))) {
        CHECKIi(i, d, i);
        i++;
      }
      CHECKI(i, 256);
      close(fd);
    }
  }
#endif

  unlink_raw = unlink(data_raw);
  unlink(format);
  rmdir(filedir);

  CHECKI(unlink_raw, 0);
#ifdef USE_ENC
  CHECKI(e1, GD_E_OK);
#else
  CHECKI(e1, GD_E_UNSUPPORTED);
#endif

  return r;
}
