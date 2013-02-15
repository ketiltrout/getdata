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
 % GD_CLOSE  Save and close a dirfile
 %
 %   GD_CLOSE(DIRFILE)
 %             flushes all changes and closes the dirfile DIRFILE.  The DIRFILE
 %             object supplied will no longer be valid if this function
 %             succeeds.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_close(3) in section 3
 %   of the UNIX manual for more details.
 %
 %   See also GD_DISCARD, GD_OPEN, GD_FLUSH
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  int n;

  GDMX_CHECK_RHS(1);

  D = gdmx_to_dirfile(prhs[0]);

  n = gd_close(D);

  if (n)
    gdmx_err(D, 1);
}
