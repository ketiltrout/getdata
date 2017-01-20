/* Copyright (C) 2013, 2017 D.V. Wiebe
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
/* Parser check */
#include "test.h"

int main(void)
{
  const char *filedir = "dirfile";
  const char *format = "dirfile/format";
  int i, error, r = 0;
#define STRLEN 11
  const unsigned char u[STRLEN] =
  {
    0x65, /* U+65 */
    0xD9, 0x94, /* U+654 */
    0xE6, 0x95, 0x83, /* U+6543 */
    0xF1, 0xA5, 0x90, 0xB2, /* U+65432 */
    0x00
  };
  unsigned char s[STRLEN];
  DIRFILE *D;

  rmdirfile();
  mkdir(filedir, 0700);

  MAKEFORMATFILE(format, "string STRING \\u65\\u654\\u6543\\u65432\n");

  D = gd_open(filedir, GD_RDONLY | GD_VERBOSE);
  error = gd_error(D);
  CHECKI(error,GD_E_OK);

  gd_get_string(D, "string", STRLEN, (char*)s);

  gd_discard(D);

  unlink(format);
  rmdir(filedir);

  for (i = 0; i < STRLEN; ++i)
    CHECKXi(i, s[i], u[i]);

  return r;
}
