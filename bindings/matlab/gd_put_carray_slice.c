/* Copyright (C) 2013, 2014 D. V. Wiebe
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
 % GD_PUT_CARRAY_SLICE  Modify CARRAY or CONST values
 %
 %   GD_PUT_CARRAY_SLICE(DIRFILE,FIELD_CODE,START,VALUES)
 %             sets elements of the CARRAY or CONST field called FIELD_CODE
 %             starting at element START to the values in the array VALUE.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_put_carray_slice(3) in
 %   section 3 of the UNIX manual for more details.
 %
 %   See also GD_GET_CARRAY_SLICE, GD_OPEN
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  void *data;
  char *field_code;
  unsigned int start;
  size_t nsamp, n;
  gd_type_t type;

  GDMX_NO_LHS;
  GDMX_CHECK_RHS(4);

  D = gdmx_to_dirfile(prhs[0]);
  field_code = gdmx_to_string(prhs, 1, 0);
  start = gdmx_to_uint(prhs, 2);
  gdmx_to_data(&data, &type, &nsamp, prhs[3], 3);

  gd_put_carray_slice(D, field_code, start, nsamp, type, data);

  gdmx_free_data(data, type);
  mxFree(field_code);
  gdmx_err(D, 0);
}
