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
 % GD_ADD_SPEC  Add a field
 %
 %   GD_ADD_SPEC(DIRFILE,SPEC,FRAGMENT)
 %             adds a field described by the field specification line SPEC to
 %             the dirfile DIRFILE.  The field is added to the fragment indexed 
 %             by FRAGMENT.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_add_spec(3) in section 3
 %   of the UNIX manual for more details.
 %
 %   See also GD_ADD, GD_MADD_SPEC, GD_OPEN
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  char *spec;
  int fragment_index;

  GDMX_NO_LHS;
  GDMX_CHECK_RHS(3);

  D = gdmx_to_dirfile(prhs[0]);
  spec = gdmx_to_string(prhs, 1, 0);
  fragment_index = gdmx_to_int(prhs, 2);

  gd_add_spec(D, spec, fragment_index);

  mxFree(spec);
  gdmx_err(D, 0);
}
