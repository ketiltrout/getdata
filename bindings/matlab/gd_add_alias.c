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
 % GD_ADD_ALIAS  Add a field alias
 %
 %   GD_ADD_ALIAS(DIRFILE,NAME,TARGET,FRAGMENT)
 %             adds a field alias called NAME pointing to TARGET in the metadata
 %             fragment indexed by FRAGMENT of the dirfile DIRFILE.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_add_alias(3) in section 3
 %   of the UNIX manual for more details.
 %
 %   See also GD_MADD_ALIAS, GD_OPEN
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  void *data;
  char *field_code, *target;
  int fragment_index;

  GDMX_NO_LHS;
  GDMX_CHECK_RHS(4);

  D = gdmx_to_dirfile(prhs[0]);
  field_code = gdmx_to_string(prhs, 1, 0);
  target = gdmx_to_string(prhs, 2, 0);
  fragment_index = gdmx_to_int(prhs, 3);

  gd_add_alias(D, field_code, target, fragment_index);

  mxFree(target);
  mxFree(field_code);
  gdmx_err(D, 0);
}
