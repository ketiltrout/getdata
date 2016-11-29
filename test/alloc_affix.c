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
void *malloc_ptr[2];

static void *malloc_func(size_t len)
{
  malloc_count++;
  if (malloc_count > 2)
    return malloc(len);

  malloc_ptr[malloc_count - 1] = malloc(len);
  return malloc_ptr[malloc_count - 1];
}

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *format1 = "dirfile/format1";
  char *prefix = NULL;
  char *suffix = NULL;
  int e1, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "/INCLUDE format1 A Z\n");
  MAKEFORMATFILE(format1, "#");

  gd_alloc_funcs(malloc_func, NULL);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);

  e1 = gd_fragment_affixes(D, 1, &prefix, &suffix);
  CHECKI(e1, 0);

  if (prefix == malloc_ptr[0])
    CHECKPP(suffix, malloc_ptr[1]);
  else {
    CHECKPP(suffix, malloc_ptr[0]);
    CHECKPP(prefix, malloc_ptr[1]);
  }

  CHECKI(malloc_count, 2);
  CHECKS(prefix, "A");
  CHECKS(suffix, "Z");

  free(prefix);
  free(suffix);

  gd_discard(D);

  unlink(format1);
  unlink(format);
  rmdir(filedir);

  return r;
}
