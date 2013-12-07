/* Copyright (C) 2013 D. V. Wiebe
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

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *format_data = "da.ta RAW UINT8 8\n";
  int fd, r1, r2, e1, e2, r = 0;
  const char **fl;
#define NFIELDS 2
  const char *field_list1[NFIELDS] = { "INDEX", "data" };
  const char *field_list2[NFIELDS] = { "INDEX", "d.ata" };
  unsigned nf, i;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0777);

  fd = open(format, O_CREAT | O_EXCL | O_WRONLY, 0666);
  write(fd, format_data, strlen(format_data));
  close(fd);

  D = gd_open(filedir, GD_RDWR);

  r1 = gd_rename(D, "da.ta", "data", 0);
  e1 = gd_error(D);
  CHECKI(r1,0);
  CHECKI(e1,0);

  nf = gd_nfields(D);
  CHECKI(nf, NFIELDS);
  if (nf > NFIELDS)
    nf = NFIELDS;

  fl = gd_field_list(D);
  for (i = 0; i < nf; ++i)
    CHECKSi(i, fl[i], field_list1[i]);

  r2 = gd_rename(D, "data", "d.ata", 0);
  e2 = gd_error(D);
  CHECKI(r2,0);
  CHECKI(e2,0);

  nf = gd_nfields(D);
  CHECKI(nf, NFIELDS);
  if (nf > NFIELDS)
    nf = NFIELDS;

  fl = gd_field_list(D);
  for (i = 0; i < nf; ++i)
    CHECKSi(i + NFIELDS, fl[i], field_list2[i]);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
