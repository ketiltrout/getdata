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
#include "test.h"

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  int e1, e2, e3, e4, e5, e6, r = 0;
  double d, h, i, n, p;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "hex CONST FLOAT64 0xABC\n"
    "dec CONST FLOAT64 1.3e3\n"
    "flt CONST FLOAT64 -0x1.3p+3\n"
    "inf CONST FLOAT64 INF\n"
    "nan CONST FLOAT64 NAN\n"
  );

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  e1 = gd_error(D);

  gd_get_constant(D, "dec", GD_FLOAT64, &d);
  e2 = gd_error(D);
  
  gd_get_constant(D, "flt", GD_FLOAT64, &p);
  e3 = gd_error(D);
  
  gd_get_constant(D, "inf", GD_FLOAT64, &i);
  e4 = gd_error(D);
  
  gd_get_constant(D, "nan", GD_FLOAT64, &n);
  e5 = gd_error(D);
  
  gd_get_constant(D, "hex", GD_FLOAT64, &h);
  e6 = gd_error(D);
  
  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  CHECKI(e1,GD_E_OK);
  CHECKI(e2,GD_E_OK);
  CHECKI(e3,GD_E_OK);
  CHECKI(e4,GD_E_OK);
  CHECKI(e5,GD_E_OK);
  CHECKI(e6,GD_E_OK);
  CHECKF(h,2748.);
  CHECKF(d,1300.);
  CHECKF(p,-9.5);
  CHECK(isfinite(i),i,"%g","infinity",i);
  CHECKNAN(n);
  return r;
}
