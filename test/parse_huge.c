/* Copyright (C) 2016, 2017 D.V. Wiebe
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

/* Fun with preprocessors */
#define n 18446744073709551615
#define llu(n) n ## LLU
#define LLU(n) llu(n)
#define N LLU(n)
#define s_(x) #x
#define s(x) s_(x)
#define S s(n)

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  int error, r = 0;
  DIRFILE *D;
  uint64_t u;
  double d, c[2];

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "u CONST UINT64 " S "\n"
    "r CONST FLOAT64 " S"\n"
    "c CONST COMPLEX128 " S ";" S "\n"
  );

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  error = gd_error(D);
  CHECKI(error,GD_E_OK);

  gd_get_constant(D, "u", GD_UINT64, &u);
  CHECKU(u, N);

  gd_get_constant(D, "r", GD_FLOAT64, &d);
  CHECKF(d, N);

  gd_get_constant(D, "c", GD_COMPLEX128, &c);
  CHECKF(c[0], N);
  CHECKF(c[1], N);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  return r;
}
