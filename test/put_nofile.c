/* Copyright (C) 2015 D. V. Wiebe
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

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

#define GD_RLIM 20
int main(void)
{
#if ! defined HAVE_SETRLIMIT
  return 77;
#else
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data_fmt = "dirfile/ent%04i";

  struct rlimit rlim = { GD_RLIM, GD_RLIM };
  int i, e1, e2, e3, r = 0;
  const uint8_t data[8] = {1,2,3,4,5,6,7,8};
  char name[16];

  if (setrlimit(RLIMIT_NOFILE, &rlim)) {
    perror("setrlimit");
    return 77;
  }

  DIRFILE *D;

  rmdirfile();
  D = gd_open(filedir, GD_CREAT | GD_UNENCODED | GD_RDWR);

  /* now add a bunch of files */
  for (i = 0; i < GD_RLIM; ++i) {
    sprintf(name, "ent%04i", i);
    gd_add_raw(D, name, GD_UINT8, 1, 0);
    e1 = gd_error(D);
    CHECKIi(i,e1,GD_E_OK);
  }
  gd_flush(D, NULL);

  /* now try writing; this will pile up file descriptors */
  for (i = 0; i < GD_RLIM; ++i) {
    sprintf(name, "ent%04i", i);
    gd_putdata(D, name, 0, 0, 0, 8, GD_UINT8, data);
    e2 = gd_error(D);
    if (e2 == GD_E_RAW_IO) {
      /* closing everything and try again -- should work now */
      gd_raw_close(D, NULL);
      gd_putdata(D, name, 0, 0, 0, 8, GD_UINT8, data);
      e3 = gd_error(D);
      CHECKIi(i,e3,GD_E_OK);
    } else {
      CHECKIi(i,e2,GD_E_OK);
    }
  }

  gd_discard(D);

  for (i = 0; i < GD_RLIM; ++i) {
    sprintf(name, data_fmt, i);
    unlink(name);
  }
  unlink(format);
  rmdir(filedir);

  return r;
#endif
}
