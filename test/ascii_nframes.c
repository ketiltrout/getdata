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
/* Retreiving the number of frames should succeed cleanly */
#include "test.h"

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data.txt";
  int i, r = 0;
  FILE* stream;
  size_t n, m;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "data RAW UINT8 1\n");

  stream = fopen(data, "w");
  for (i = 0; i < 256; ++i)
    fprintf(stream, "%i\n", i);
  fclose(stream);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n = gd_nframes(D);
  m = gd_nframes(D);
  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(n, 256);
  CHECKI(m, 256);

  return r;
}
