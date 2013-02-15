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
 % GD_RENAME  Rename a field or alias
 %
 %   GD_RENAME(DIRFILE,OLD_NAME,NEW_NAME[,FLAGS])
 %             renames the field called OLD_NAME to NEW_NAME.  If given, FLAGS
 %             should be zero or more of the GD.REN_... symbols provided by
 %             GETDATA_CONSTANTS.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_rename(3) in section 3
 %   of the UNIX manual for more details.
 %
 %   See also GD_OPEN
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  char *field_code, *new_code;
  unsigned int flags = 0;

  GDMX_NO_LHS;
  GDMX_CHECK_RHS2(3,4);

  D = gdmx_to_dirfile(prhs[0]);
  field_code = gdmx_to_string(prhs, 1, 0);
  new_code = gdmx_to_string(prhs, 2, 0);
  if (nrhs > 3)
    flags = gdmx_to_uint(prhs, 3);

  gd_rename(D, field_code, new_code, flags);

  mxFree(field_code);
  gdmx_err(D, 0);
}
