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

int malloc_count = 0;

static void *malloc_func(size_t len)
{
  malloc_count++;
  return malloc(len);
}

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *spec = "string STRING value";
  int r = 0;
  char *tok;
  DIRFILE *D;

  rmdirfile();

  gd_alloc_funcs(malloc_func, NULL);

  D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_EXCL | GD_VERBOSE);

  tok = gd_strtok(D, spec);
  CHECKI(malloc_count, 1);
  free(tok);

  tok = gd_strtok(D, NULL);
  CHECKI(malloc_count, 2);
  free(tok);

  tok = gd_strtok(D, NULL);
  CHECKI(malloc_count, 3);
  free(tok);

  tok = gd_strtok(D, NULL);
  CHECKI(malloc_count, 3);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
