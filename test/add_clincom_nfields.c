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
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  DIRFILE *D;
  const char *in_fields[2] = {"in1", "in2"};
#ifdef GD_NO_C99_API
  const double m[4] = {1, 3.3, 0.3, 18.3};
  const double b[4] = {2, 3.8, 2.1, 9.8};
#else
  const double complex m[2] = {1 + _Complex_I * 3.3, 0.3 + _Complex_I * 18.3};
  const double complex b[2] = {2 + _Complex_I * 3.8, 2.1 + _Complex_I * 9.8};
#endif
  int error, r = 0;

  rmdirfile();
  D = gd_open(filedir, GD_RDWR | GD_CREAT);

  gd_add_clincom(D, "new", 22, in_fields, m, b, 0);
  error = gd_error(D);
  CHECKI(error, GD_E_BAD_ENTRY);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
