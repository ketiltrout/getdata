/* Copyright (C) 2017 D. V. Wiebe
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
 % GD_OPEN_LIMIT  Limit or report the number of descriptors in use
 %
 %   V = GD_OPEN_LIMIT(DIRFILE[,LIMIT])
 %             if LIMIT is the GD.OLIMIT_COUNT constant provided by
 %             GETDATA_CONSTANTS, then this function reports the number of
 %             file descriptors currently in use by DIRFILE.  Otherwise, if
 %             LIMIT is given, then DIRFILE's open descriptor to that value.
 %             If LIMIT is zero, descriptor limiting is disabled.  Returns
 %             the current limit, after possibly updating it.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_open_limit(3) in section 3
 %   of the UNIX manual for more details.
 %
 %   See also GD_OPEN, GETDATA_CONSTANTS
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  long limit = GD_OLIMIT_CURRENT;

  GDMX_CHECK_RHS2(1,2);

  D = gdmx_to_dirfile(prhs[0]);
  if (nrhs > 1)
    limit = gdmx_to_int(prhs, 1);

  limit = gd_open_limit(D, limit);

  gdmx_err(D, 0);

  plhs[0] = gdmx_from_long(limit);
}
