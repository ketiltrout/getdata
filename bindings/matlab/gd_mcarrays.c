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
#include <string.h>

/*
 % GD_MCARRAYS  Fetch all CARRAY metafield values
 %
 %   A = GD_MCARRAYS(DIRFILE,PARENT[,TYPE])
 %             returns a cell array of numeric arrays, A, containing all the
 %             CARRAY data in the for metafields under parent field PARENT.  A
 %             corresponding array of field names can be produced with
 %             GD_MFIELD_LIST_BY_TYPE.  The type of the returned data is given
 %             by TYPE, one of the data type symbols provided by
 %             GETDATA_CONSTANTS.  If omitted, the default type, GD.FLOAT64, is
 %             used.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_mcarrays(3) in section 3
 %   of the UNIX manual for more details.
 %
 %   See also GD_CARRAYS, GD_MFIELD_LIST_BY_TYPE, GD_GET_CARRAY_SLICE
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  gd_type_t type = GD_FLOAT64;
  const gd_carray_t *c;
  char *parent;

  GDMX_CHECK_RHS2(2,3);

  D = gdmx_to_dirfile(prhs[0]);
  parent = gdmx_to_string(prhs, 1, 0);
  if (nrhs > 2)
    type = gdmx_to_gd_type(prhs, 2);

  c = gd_mcarrays(D, parent, type);
  mxFree(parent);

  gdmx_err(D, 0);

  /* convert to array of arrays */
  plhs[0] = gdmx_from_carrays(c, type);
}
