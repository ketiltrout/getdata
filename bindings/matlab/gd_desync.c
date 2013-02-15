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
 % GD_DESYNC  Modify the byte sex of a fragment
 %
 %   B = GD_DESYNC(DIRFILE[,FLAGS])
 %             returns a boolean, B, indicating whether the dirfile DIRFILE on
 %             disk has been changed by a third-party since having been opened
 %             by GetData.  If given, FLAGS should be a bitwise-or'd collection
 %             of zero or more of the GD.DESYNC_... symbols provided by
 %             GETDATA_CONSTANTS.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_desync(3) in section 3
 %   the UNIX manual for more details.
 %
 %   See also GD_OPEN, GETDATA_CONSTANTS
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  void *data;
  unsigned int flags = 0;
  int n;

  GDMX_CHECK_RHS2(1,2);

  D = gdmx_to_dirfile(prhs[0]);
  if (nrhs > 1)
    flags = gdmx_to_uint(prhs, 1);

  n = gd_desync(D, 0);

  gdmx_err(D, 0);

  plhs[0] = gdmx_from_bool(n);
}
