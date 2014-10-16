/* Copyright (C) 2008-2011, 2013, 2014 D. V. Wiebe
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

#include <string.h>

#define DIRFILENAME "a non-existant dirfile"
int main(void)
{
  char *string;
  int error, r = 0;
  DIRFILE *D = gd_open(DIRFILENAME, 0);

  string = gd_error_string(D, NULL, 0);
  error = gd_error(D);
  gd_discard(D);

  CHECKI(error, GD_E_IO);
  CHECKSS(string, DIRFILENAME);

  free(string);
  return r;
}
