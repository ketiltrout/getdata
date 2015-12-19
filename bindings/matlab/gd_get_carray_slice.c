/* Copyright (C) 2013, 2015 D. V. Wiebe
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
 % GD_GET_CARRAY_SLICE  Retrieve CARRAY data
 %
 %   A = GD_GET_CARRAY_SLICE(DIRFILE,FIELD_CODE,START,LEN[,TYPE])
 %             retrieves a subset, A, of the CARRAY field called FIELD_CODE.
 %             LEN elements are retrieved, starting with element START.  If TYPE
 %             is given, it should be one of the data type symbols provided by
 %             GETDATA_CONSTANTS, otherwise the data are returned in their
 %             native type.
 %
 %             If TYPE is the NULL data type, this function simply returns zero
 %             on success.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_get_carray_slice(3) in
 %   section 3 of the UNIX manual for more details.
 %
 %   See also GD_GET_CARRAY, GD_PUT_CARRAY_SLICE, GD_OPEN, GETDATA_CONSTANTS
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  void *data;
  char *field_code;
  unsigned int start;
  size_t len;
  gd_type_t type;

  GDMX_CHECK_RHS2(4,5);

  D = gdmx_to_dirfile(prhs[0]);
  field_code = gdmx_to_string(prhs, 1, 0);
  start = gdmx_to_uint(prhs, 2);
  len = gdmx_to_size_t(prhs, 3);
  if (nrhs > 4)
    type = gdmx_to_gd_type(prhs, 4);
  else {
    type = gd_native_type(D, field_code);
    gdmx_err(D, 0);
  }

  if (type == GD_NULL) {
    gd_get_carray_slice(D, field_code, start, len, GD_NULL, NULL);
    plhs[0] = gdmx_from_int(0);
  } else {
    plhs[0] = gdmx_vector(type, len, &data);

    gd_get_carray_slice(D, field_code, start, len, type, data);

    mxFree(field_code);
    gdmx_err(D, 0);

    gdmx_fix_vector(plhs[0], type, len, data);
  }
}
