/* Copyright (C) 2013, 2016 D. V. Wiebe
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
 % GD_INVALID_DIRFILE  Create an invalid dirfile object
 %
 %   D = GD_INVALID_DIRFILE()
 %             creates an invalid dirfile object, D.  It should be deallocated
 %             with GD_CLOSE or GD_DISCARD when no longer needed.
 %
 %   See the documentation on the C API function gd_invalid_dirfile(3) in
 %   section 3 of the UNIX manual for more details.
 %
 %   See also GD_CLOSE, GD_DISCARD, GD_OPEN
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;

  GDMX_CHECK_RHS(0);

  gdmx_initialise();

  D = gd_invalid_dirfile();

  plhs[0] = gdmx_from_dirfile(D);
}
