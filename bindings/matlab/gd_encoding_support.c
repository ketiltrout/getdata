/* Copyright (C) 2014 D. V. Wiebe
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
#include "gd_matlab.h"

/*
 % GD_ENCODING_SUPPORT  Run-time detection of supported GetData encodings
 %
 %   N = GD_ENCODING_SUPPORT(ENCODING)
 %             returns GD.RDWR if the ENCODING specified can be both read and
 %             written by the library, GD.RDONLY if it can only be read, or
 %             -1 if neither is supported, or if the specified ENCODING was
 %             invalid.
 %
 %   GD.RDWR and GD.RDONLY are provided by the GETDATA_CONSTANTS() function,
 %   and the ENCODING should be specified using one one of the GD.xxx_ENCODED
 %   symbols provided by that function.
 %
 %   See the documentation on the C API function gd_encoding_support(3) in
 %   section of the UNIX manual for more details.
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  char *field_code = NULL;
  int n;
  unsigned long e;

  GDMX_CHECK_RHS(1);

  e = gdmx_to_ulong(prhs, 0);
  n = gd_encoding_support(e);

  plhs[0] = gdmx_from_int(n);
}
