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
 % GD_ADD_SARRAY  Add a SARRAY field
 %
 %   GD_ADD_SARRAY(DIRFILE,NAME,VALUES,FRAGMENT)
 %             adds a SARRAY field called NAME to the dirfile specified by
 %             DIRFILE.  The values of the field are in VALUES.  The field is
 %              added to the fragment indexed by FRAGMENT.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_add_sarray(3) in section 3
 %   of the UNIX manual for more details.
 %
 %   See also GD_ADD, GD_MADD_SARRAY, GD_OPEN
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  const char **data;
  char *field_name;
  size_t n;
  int fragment_index;

  GDMX_NO_LHS;
  GDMX_CHECK_RHS(4);

  D = gdmx_to_dirfile(prhs[0]);
  field_name = gdmx_to_string(prhs, 1, 0);
  gdmx_to_sdata(&data, &n, prhs[2], 2);
  fragment_index = gdmx_to_int(prhs, 3);

  gd_add_sarray(D, field_name, n, data, fragment_index);

  gdmx_free_sdata(data, n);
  mxFree(field_name);
  gdmx_err(D, 0);
}
