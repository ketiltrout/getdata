/* Copyright (C) 2013 D. V. Wiebe
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
 % GD_ENCODING  Report the data encoding of a fragment
 %
 %   E = GD_ENCODING(DIRFILE,FRAGMENT)
 %             reports the encoding, E, of the fragment indexed by FRAGMENT
 %             in the dirfile DIRFILE.  The returned value will be one of the
 %             GD.xxx_ENCODED symbols provided by GETDATA_CONSTANTS.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_encoding(3) in
 %   section 3 of the UNIX manual for more details.
 %
 %   See also GD_ALTER_ENCODING, GD_OPEN, GETDATA_CONSTANTS
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  unsigned long n;
  int i;

  GDMX_CHECK_RHS(2);

  D = gdmx_to_dirfile(prhs[0]);
  i = gdmx_to_int(prhs, 1);

  n = gd_encoding(D, i);

  gdmx_err(D, 0);

  plhs[0] = gdmx_from_ulong(n);
}
