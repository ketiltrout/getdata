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
 % GD_GET_SARRAY  Retrieve SARRAY data
 %
 %   A = GD_GET_SARRAY(DIRFILE,FIELD_CODE)
 %             retrieves an array, A, of value of the SARRAY field called
 %             FIELD_CODE.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_get_sarray_slice(3) in
 %   section 3 of the UNIX manual for more details.
 %
 %   See also GD_GET_SARRAY_SLICE, GD_PUT_SARRAY, GD_OPEN, GETDATA_CONSTANTS
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  char *field_code;
  size_t len;
  const char **fl;

  GDMX_CHECK_RHS(2);

  D = gdmx_to_dirfile(prhs[0]);
  field_code = gdmx_to_string(prhs, 1, 0);

  len = gd_array_len(D, field_code);
  gdmx_err(D, 0);

  fl = mxMalloc(sizeof(*fl) * len);

  gd_get_sarray(D, field_code, fl);

  mxFree(field_code);
  gdmx_err(D, 0);

  plhs[0] = gdmx_from_nstring_list(fl, len);
  mxFree(fl);
}
