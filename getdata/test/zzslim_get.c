/* Copyright (C) 2012 D. V. Wiebe
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
#if !defined(TEST_ZZIP) || !defined(TEST_SLIM)
  return 77;
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  const char *dataslm = "dirfile/data.slm";
  const char *testzip = "dirfile/test.zip";
  const char *format_data =
    "data RAW UINT16 8\n"
    "/ENCODING zzslim test\n";
  uint16_t c[8];
  char command[4096];
  uint16_t data_data[256];
  int fd, n, error, i, r = 0;
  DIRFILE *D;

  memset(c, 0, 8);
  rmdirfile();
  mkdir(filedir, 0777);

  for (fd = 0; fd < 256; ++fd)
    data_data[fd] = (unsigned char)fd;

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  fd = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(fd, data_data, 256 * sizeof(uint16_t));
  close(fd);

  /* compress, twice */
  snprintf(command, 4096, "%s -k %s > /dev/null", SLIM, data);
  if (gd_system(command))
    return 1;
  chdir(filedir);
  snprintf(command, 4096, "%s test data.slm > /dev/null", ZIP);
  if (gd_system(command))
    return 1;
  chdir("..");
  unlink(dataslm);
  unlink(data);

#ifdef USE_ZZSLIM
  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
#else
  D = gd_open(filedir, GD_RDONLY);
#endif
  n = gd_getdata(D, "data", 5, 0, 1, 0, GD_UINT16, c);
  error = gd_error(D);

  gd_close(D);

  unlink(testzip);
  unlink(format);
  rmdir(filedir);

#ifdef USE_ZZSLIM
  CHECKI(error, 0);
  CHECKI(n, 8);
  for (i = 0; i < 8; ++i)
    CHECKIi(i,c[i], 40 + i);
#else
  CHECKI(error, GD_E_UNSUPPORTED);
  CHECKI(n,0);
#endif

  return r;
#endif
}
