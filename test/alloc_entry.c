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

int malloc_count = 0, mlen = 10;
int free_count = 0;
void **malloc_ptr = NULL;

static void *malloc_func(size_t len)
{
  if (malloc_count == mlen) {
    void *ptr = realloc(malloc_ptr, sizeof(void*) * (mlen *= 2));
    if (ptr == NULL)
      return malloc(len);
    malloc_ptr = ptr;
  }
    
  malloc_ptr[malloc_count] = malloc(len);
  return malloc_ptr[malloc_count++];
}

static void free_func(void *ptr)
{
  ptr = ptr;
  free_count++;

  /* We'll free things later */
  return;
}

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  int i, r = 0;
  DIRFILE *D;
  gd_entry_t E;

  malloc_ptr = malloc(sizeof(void*) * mlen);

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
      "lincom  LINCOM  Lorem ipsum dolor\n"
      "linterp LINTERP sit amet,\n"
      "polynom POLYNOM consectetur adipiscing elit,\n"
      "indir   INDIR   sed do\n"
      "recip   RECIP   eiusmod tempor\n"
      "sbit    SBIT    incididunt ut labore\n"
      "raw     RAW     UINT8 et\n"
      "window  WINDOW  dolor magna EQ aliqua.\n"
      "mplex   MPLEX   Ut enim as minim\n"
      );

  gd_alloc_funcs(malloc_func, free_func);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);

  gd_entry(D, "lincom", &E);
  CHECKI(malloc_count, 4);
  gd_free_entry_strings(&E);
  CHECKI(free_count, 4);

  gd_entry(D, "linterp", &E);
  CHECKI(malloc_count, 7);
  gd_free_entry_strings(&E);
  CHECKI(free_count, 7);

  gd_entry(D, "polynom", &E);
  CHECKI(malloc_count, 11);
  gd_free_entry_strings(&E);
  CHECKI(free_count, 11);

  gd_entry(D, "indir", &E);
  CHECKI(malloc_count, 14);
  gd_free_entry_strings(&E);
  CHECKI(free_count, 14);

  gd_entry(D, "recip", &E);
  CHECKI(malloc_count, 17);
  gd_free_entry_strings(&E);
  CHECKI(free_count, 17);

  gd_entry(D, "sbit", &E);
  CHECKI(malloc_count, 21);
  gd_free_entry_strings(&E);
  CHECKI(free_count, 21);

  gd_entry(D, "raw", &E);
  CHECKI(malloc_count, 23);
  gd_free_entry_strings(&E);
  CHECKI(free_count, 23);

  gd_entry(D, "window", &E);
  CHECKI(malloc_count, 27);
  gd_free_entry_strings(&E);
  CHECKI(free_count, 27);

  gd_entry(D, "mplex", &E);
  CHECKI(malloc_count, 32);
  gd_free_entry_strings(&E);
  CHECKI(free_count, 32);

  for (i = 0; i < malloc_count; ++i)
    free(malloc_ptr[i]);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
