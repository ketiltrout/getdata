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
 % GD_MADD_STRING  Add a STRING field
 %
 %   GD_MADD_STRING(DIRFILE,PARENT,NAME,VALUE)
 %             adds a STRING metafield called NAME to the dirfile specified by
 %             DIRFILE under parent field PARENT.  The value of the field is
 %             set to VALUE.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_madd_string(3) in section 3
 %   of the UNIX manual for more details.
 %
 %   See also GD_MADD, GD_ADD_STRING, GD_OPEN
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  char *field_name, *value, *parent;

  GDMX_NO_LHS;
  GDMX_CHECK_RHS(4);

  D = gdmx_to_dirfile(prhs[0]);
  parent = gdmx_to_string(prhs, 1, 0);
  field_name = gdmx_to_string(prhs, 2, 0);
  value = gdmx_to_string(prhs, 3, 0);

  gd_madd_string(D, parent, field_name, value);

  mxFree(field_name);
  mxFree(value);
  mxFree(parent);
  gdmx_err(D, 0);
}
