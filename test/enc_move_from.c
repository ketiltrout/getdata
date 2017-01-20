/* Copyright (C) 2008-2011, 2013, 2016, 2017 D.V. Wiebe
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
#ifdef ENC_SKIP_TEST
  return 77;
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *format1 = "dirfile/format1";
  const char *data = "dirfile/data";
  const char *encdata = "dirfile/data" ENC_SUFFIX;
  char command[4096];
  int ret, ge_ret, unlink_data, unlink_encdata, e1, e2, r = 0;
  DIRFILE *D;
  gd_entry_t E;
#ifdef USE_ENC
  uint16_t d;
  int fd, i = 0;
#endif

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
      "/INCLUDE format1\ndata RAW UINT16 11\nENCODING " ENC_NAME "\n");
  MAKEFORMATFILE(format1, "ENCODING none\n");
  MAKEDATAFILE(data, uint16_t, i * 0x201, 128);

  /* compress */
  ENC_COMPRESS;
  if (gd_system(command))
    return 1;

#ifdef USE_ENC
  D = gd_open(filedir, GD_RDWR | GD_VERBOSE | GD_UNENCODED);
#else
  D = gd_open(filedir, GD_RDWR | GD_UNENCODED);
#endif

  ret = gd_move(D, "data", 1, 1);
  e1 = gd_error(D);

#ifdef USE_ENC
  CHECKI(ret, 0);
  CHECKI(e1, 0);
#else
  CHECKI(ret, GD_E_UNSUPPORTED);
  CHECKI(e1, GD_E_UNSUPPORTED);
#endif

  ge_ret =  gd_entry(D, "data", &E);
  CHECKI(ge_ret, 0);
  gd_free_entry_strings(&E);

#ifdef USE_ENC
  CHECKI(E.fragment_index, 1);
#else
  CHECKI(E.fragment_index, 0);
#endif

  e2 = gd_close(D);
  CHECKI(e2, 0);

#ifdef USE_ENC
  fd = open(data, O_RDONLY | O_BINARY);

  if (fd >= 0) {
    while (read(fd, &d, sizeof(uint16_t))) {
      CHECKUi(i, d, i * 0x201);
      i++;
    }
    close(fd);
  } else {
    perror("open");
    r = 1;
  }
#endif

  unlink(format1);
  unlink(format);
  unlink_data = unlink(data);
  unlink_encdata = unlink(encdata);
  rmdir(filedir);

#ifdef USE_ENC
  CHECKI(unlink_data, 0);
  CHECKI(unlink_encdata, -1);
#else
  CHECKI(unlink_data, -1);
  CHECKI(unlink_encdata, 0);
#endif

  return r;
#endif
}
