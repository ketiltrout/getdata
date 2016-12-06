/* Copyright (C) 2016 D. V. Wiebe
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
#if defined ENC_SKIP_TEST
  return 77; /* skip test */
#else

#define NF 3765
#define SPF 20
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  const char *encdata = "dirfile/data" ENC_SUFFIX;
  uint32_t *c;
  char command[4096];
  int i, e1, e2, e3, r = 0;
  size_t n1, n2, n3;
  DIRFILE *D;

  /* Enough space for three times NF frames */
  c = malloc(sizeof(*c) * NF * 20 * 3);

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "data RAW UINT32 20");
  MAKEDATAFILE(data, uint32_t, i, NF * 10 * 20);

  /* Compress */
  ENC_COMPRESS;
  if (gd_system(command))
    return 1;

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n1 = gd_getdata(D, "data", 0, 0, NF, 0, GD_UINT32, c);
  CHECKI(n1, NF * SPF);
  e1 = gd_error(D);
  CHECKI(e1, 0);

  n2 = gd_getdata(D, "data", NF, 0, NF, 0, GD_UINT32, c + NF * SPF);
  CHECKI(n2, NF * SPF);
  e2 = gd_error(D);
  CHECKI(e2, 0);

  n3 = gd_getdata(D, "data", NF * 2, 0, NF, 0, GD_UINT32, c + 2 * NF * SPF);
  CHECKI(n3, NF * SPF);
  e3 = gd_error(D);
  CHECKI(e3, 0);

  for (i = 0; i < 3 * NF * 20; ++i)
    CHECKIi(i,c[i], i);

  gd_discard(D);

  unlink(encdata);
  unlink(format);
  rmdir(filedir);

  free(c);

  return r;
#endif
}
