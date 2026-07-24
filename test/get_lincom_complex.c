/* Copyright (C) 2026 Graeme Smecher
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

/* A LINCOM with purely real coefficients must return the same values when
 * read with a complex return type as it does when read with a real return
 * type, and must propagate the imaginary parts of complex-valued inputs.
 *
 * The first two fields sum copies of a real-valued input, so:
 * lincom2[n] = 2 * data[n] and lincom3[n] = 3 * data[n], with zero
 * imaginary parts.
 *
 * The last two fields have a complex-valued input, cdata[n] = 2n + (2n+1)i,
 * whose imaginary part must survive scaling by the real coefficients:
 * clincom2[n] = 2 * cdata[n] and clincom1[n] = 2 * cdata[n] + 3.  (The
 * single-input clincom1 avoids gain 1 and offset 0, which GetData
 * short-circuits as a rename without computing anything.)
 *
 * The reads start at sample 5 rather than sample 0 because sample 0 can be
 * computed correctly even by an implementation that confuses complex and
 * real buffer indices (index 0 is index 0 either way). */

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  const char *data = "dirfile/data";
  const char *cdata = "dirfile/cdata";
  double c1[16], c2[16], c3[16], c4[16];
  int i, n1, n2, n3, n4, error1, error2, error3, error4, r = 0;
  DIRFILE *D;

  memset(c1, 0, sizeof(c1));
  memset(c2, 0, sizeof(c2));
  memset(c3, 0, sizeof(c3));
  memset(c4, 0, sizeof(c4));

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format,
    "lincom2 LINCOM 2 data 1 0 data 1 0\n"
    "lincom3 LINCOM 3 data 1 0 data 1 0 data 1 0\n"
    "clincom2 LINCOM 2 cdata 1 0 cdata 1 0\n"
    "clincom1 LINCOM 1 cdata 2 3\n"
    "data RAW UINT8 1\n"
    "cdata RAW COMPLEX128 1\n"
  );
  MAKEDATAFILE(data, unsigned char, i, 256);
  MAKEDATAFILE(cdata, double, i, 512);

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  n1 = gd_getdata(D, "lincom2", 5, 0, 8, 0, GD_COMPLEX128, &c1);
  error1 = gd_error(D);

  n2 = gd_getdata(D, "lincom3", 5, 0, 8, 0, GD_COMPLEX128, &c2);
  error2 = gd_error(D);

  n3 = gd_getdata(D, "clincom2", 5, 0, 8, 0, GD_COMPLEX128, &c3);
  error3 = gd_error(D);

  n4 = gd_getdata(D, "clincom1", 5, 0, 8, 0, GD_COMPLEX128, &c4);
  error4 = gd_error(D);

  gd_discard(D);

  unlink(cdata);
  unlink(data);
  unlink(format);
  rmdir(filedir);

  CHECKI(error1, 0);
  CHECKI(n1, 8);
  for (i = 0; i < 8; ++i) {
    CHECKFi(i, c1[2 * i], 2 * (5 + i));
    CHECKFi(i, c1[2 * i + 1], 0);
  }

  CHECKI(error2, 0);
  CHECKI(n2, 8);
  for (i = 0; i < 8; ++i) {
    CHECKFi(i, c2[2 * i], 3 * (5 + i));
    CHECKFi(i, c2[2 * i + 1], 0);
  }

  CHECKI(error3, 0);
  CHECKI(n3, 8);
  for (i = 0; i < 8; ++i) {
    CHECKFi(i, c3[2 * i], 4 * (5 + i));
    CHECKFi(i, c3[2 * i + 1], 4 * (5 + i) + 2);
  }

  CHECKI(error4, 0);
  CHECKI(n4, 8);
  for (i = 0; i < 8; ++i) {
    CHECKFi(i, c4[2 * i], 4 * (5 + i) + 3);
    CHECKFi(i, c4[2 * i + 1], 4 * (5 + i) + 2);
  }

  return r;
}
