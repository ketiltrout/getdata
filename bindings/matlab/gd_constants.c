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
 % GD_CONSTANTS  Fetch all CONST values
 %
 %   A = GD_CONSTANTS(DIRFILE[,TYPE])
 %             returns a numeric array, A, containing all the CONST data in the
 %             dirfile DIRFILE.  A corresponding array of field names can be
 %             produced with GD_FIELD_LIST_BY_TYPE.  The type of the returned
 %             data is given by TYPE, one of the data type symbols provided by
 %             GETDATA_CONSTANTS.  If omitted, the default type, GD.FLOAT64, is
 %             used.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_constants(3) in section 3
 %   of the UNIX manual for more details.
 %
 %   See also GD_MCONSTANTS, GD_FIELD_LIST_BY_TYPE, GD_GET_CONSTANT,
 %            GETDATA_CONSTANTS
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  const void *data;
  unsigned int n;
  gd_type_t type = GD_FLOAT64;

  GDMX_CHECK_RHS2(1,2);

  D = gdmx_to_dirfile(prhs[0]);
  if (nrhs > 1)
    type = gdmx_to_gd_type(prhs, 1);

  data = gd_constants(D, type);

  gdmx_err(D, 0);
  
  /* length */
  n = gd_nfields_by_type(D, GD_CONST_ENTRY);

  gdmx_err(D, 0);

  plhs[0] = gdmx_from_data(data, type, n);
}
