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
 % GD_MADD_CARRAY  Add a CARRAY field
 %
 %   GD_MADD_CARRAY(DIRFILE,PARENT,NAME,TYPE,VALUES)
 %             adds a CARRAY metafield called NAME to the dirfile specified by
 %             DIRFILE under parent field PARENT.  The storage type is TYPE,
 %             and the values of the field are in VALUES.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_madd_carray(3) in section 3
 %   of the UNIX manual for more details.
 %
 %   See also GD_MADD, GD_ADD_CARRAY, GD_OPEN
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  void *data;
  char *field_name, *parent;
  size_t n;
  gd_type_t const_type, data_type;

  GDMX_NO_LHS;
  GDMX_CHECK_RHS(5);

  D = gdmx_to_dirfile(prhs[0]);
  parent = gdmx_to_string(prhs, 1, 0);
  field_name = gdmx_to_string(prhs, 2, 0);
  const_type = gdmx_to_gd_type(prhs, 3);
  gdmx_to_data(&data, &data_type, &n, prhs[4], 4);

  gd_madd_carray(D, parent, field_name, const_type, n, data_type, data);

  gdmx_free_data(data, data_type);
  mxFree(field_name);
  mxFree(parent);
  gdmx_err(D, 0);
}
