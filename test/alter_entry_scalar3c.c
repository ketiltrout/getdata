/* Copyright (C) 2013, 2016, 2017 D.V. Wiebe
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
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  int ret, error, n, r = 0;
#ifdef GD_NO_C99_API
  const double d[2] = {3.1, 4.5};
#else
  const double _Complex d = 3.1 + _Complex_I * 4.5;
#endif
  DIRFILE *D;
  gd_entry_t E;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "data RECIP in c1\nc1 CONST COMPLEX128 3.1;4.5\n");

  D = gd_open(filedir, GD_RDWR | GD_VERBOSE);

  memset(&E, 0, sizeof(E));
  E.field_type = GD_RECIP_ENTRY;
  E.in_fields[0] = "in";
  E.flags = GD_EN_COMPSCAL;
  ret = gd_alter_entry(D, "data", &E, 0);
  error = gd_error(D);

  CHECKI(ret,0);
  CHECKI(error,0);

  n = gd_entry(D, "data", &E);

  CHECKI(n,0);
  CHECKU(E.flags & GD_EN_COMPSCAL, GD_EN_COMPSCAL);
  CHECKC(E.EN(recip,cdividend), d);
  CHECKP(E.scalar[0]);
  gd_free_entry_strings(&E);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
