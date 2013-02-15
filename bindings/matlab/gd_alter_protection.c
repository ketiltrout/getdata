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
 % GD_ALTER_PROTECTION  Modify the byte sex of a fragment
 %
 %   GD_ALTER_PROTECTION(DIRFILE,LEVEL,FRAGMENT)
 %             sets the protection level of the metadata fragment indexed by
 %             FRAGMENT in the dirfile DIRFILE to LEVEL, which should be one of
 %             the GD.PROTECT_[...] symbols provided by GETDATA_CONSTANTS.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_alter_protection(3) in
 %   section 3 of the UNIX manual for more details.
 %
 %   See also GD_PROTECTION, GD_OPEN, GETDATA_CONSTANTS
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  void *data;
  int i, n;

  GDMX_NO_LHS;
  GDMX_CHECK_RHS(3);

  D = gdmx_to_dirfile(prhs[0]);
  i = gdmx_to_int(prhs, 1);
  n = gdmx_to_int(prhs, 2);

  gd_alter_protection(D, n, i);

  gdmx_err(D, 0);
}
