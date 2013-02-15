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
 % GD_FLAGS  Modify or report dirfile flags
 %
 %   F = GD_FLAGS(DIRFILE,SET,RESET)
 %             sets the operational flags of the dirfile DIRFILE according to 
 %             SET and RESET, which should be bitwise or'd collections of the
 %             Dirfile flags provided by GETDATA_CONSTANTS.  This function then
 %             returns the update flag register, F.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_flags(3) in section 3
 %   of the UNIX manual for more details.
 %
 %   See also GD_OPEN, GETDATA_CONSTANTS
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  void *data;
  unsigned long set, reset, flags;

  GDMX_CHECK_RHS(3);

  D = gdmx_to_dirfile(prhs[0]);
  set = gdmx_to_ulong(prhs, 1);
  reset = gdmx_to_ulong(prhs, 2);

  flags = gd_flags(D, set, reset);

  gdmx_err(D, 0);

  plhs[0] = gdmx_from_ulong(flags);
}
