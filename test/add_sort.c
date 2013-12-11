/* Copyright (C) 2008-2011, 2013 D. V. Wiebe
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

int r = 0;

void CheckSPF(DIRFILE *D, const char *f, unsigned int v)
{
  gd_entry_t e;

  gd_entry(D, f, &e);

  if (gd_error(D)) {
    r = 1;
    return;
  }

  CHECKS(e.field, f);
  CHECKI(e.EN(raw,spf), v);
  gd_free_entry_strings(&e);
}

int main (void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *a = "dirfile/a";
  const char *b = "dirfile/b";
  const char *c = "dirfile/c";
  const char *d = "dirfile/d";
  const char *e = "dirfile/e";
  const char *f = "dirfile/f";
  const char *g = "dirfile/g";
  const char *h = "dirfile/h";
  const char *i = "dirfile/i";
  const char *j = "dirfile/j";
  const char *k = "dirfile/k";
  DIRFILE *D;

  rmdirfile();
  D = gd_open(filedir, GD_RDWR | GD_CREAT | GD_EXCL | GD_VERBOSE);
  gd_add_raw(D, "d", GD_FLOAT64, 1, 0);
  gd_add_raw(D, "b", GD_FLOAT64, 2, 0);
  gd_add_raw(D, "h", GD_FLOAT64, 3, 0);
  gd_add_raw(D, "e", GD_FLOAT64, 4, 0);
  gd_add_raw(D, "g", GD_FLOAT64, 5, 0);
  gd_add_raw(D, "c", GD_FLOAT64, 6, 0);
  gd_add_raw(D, "k", GD_FLOAT64, 7, 0);
  gd_add_raw(D, "a", GD_FLOAT64, 8, 0);
  gd_add_raw(D, "f", GD_FLOAT64, 9, 0);
  gd_add_raw(D, "i", GD_FLOAT64, 10, 0);
  gd_add_raw(D, "j", GD_FLOAT64, 11, 0);

  /* The idea here is that a field look-up will fail unless the library has
   * added the field in the correct location */
  CheckSPF(D, "a", 8);
  CheckSPF(D, "b", 2);
  CheckSPF(D, "c", 6);
  CheckSPF(D, "d", 1);
  CheckSPF(D, "e", 4);
  CheckSPF(D, "f", 9);
  CheckSPF(D, "g", 5);
  CheckSPF(D, "h", 3);
  CheckSPF(D, "i", 10);
  CheckSPF(D, "j", 11);
  CheckSPF(D, "k", 7);
  gd_discard(D);

  unlink(k);
  unlink(j);
  unlink(i);
  unlink(h);
  unlink(g);
  unlink(f);
  unlink(e);
  unlink(d);
  unlink(c);
  unlink(b);
  unlink(a);
  unlink(format);
  rmdir(filedir);

  return r;
}
