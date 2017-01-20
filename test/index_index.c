/* Copyright (C) 2009-2011, 2013, 2017 D.V. Wiebe
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
/* Frameindex look-up */
#include "test.h"

#define F(x) sqrt(((x) + 600.) / 500.)
/* inverse of F(x) via linear interpolation between x and x+1 */
#define G(x,y) (x + ((y - F(x)) / (F(x+1) - F(x))))
int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  double d[1000], f1, f2, f3, f4;
  int i, error, r = 0;
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  for (i = 0; i < 1000; ++i)
    d[i] = F(i);

  MAKEFORMATFILE(format, "data RAW FLOAT64 1\n");

  i = open(data, O_CREAT | O_EXCL | O_WRONLY | O_BINARY, 0666);
  write(i, d, 1000 * sizeof(double));
  close(i);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  f1 = gd_framenum(D, "data", 1.09);
  f2 = gd_framenum(D, "data", 1.49);
  f3 = gd_framenum(D, "data", 1.79);
  f4 = gd_framenum(D, "data", F(128));
  error = gd_error(D);

  gd_discard(D);

  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error, 0);
  CHECKF(f1, G(   0, 1.09)); /* =   -5.96730894763915 */
  CHECKF(f2, G( 510, 1.49)); /* =  510.050010695549 */
  CHECKF(f3, G( 998, 1.79)); /* = 1002.04807025292 */
  CHECKF(f4, 128.); /* exact */

  return r;
}
