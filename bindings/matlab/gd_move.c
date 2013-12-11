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
 % GD_MOVE  Move a field between fragments
 %
 %   GD_MOVE(DIRFILE,FIELD_CODE,NEW_FRAGMENT[,MOVE_DATA])
 %             moves the field called FIELD_CODE to the new fragment indexed by
 %             NEW_FRAGMENT.  If MOVE_DATA is given and non-zero, and the field
 %             is a RAW field, the binary file associated with it will also be
 %             moved, if necessary.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_move(3) in
 %   section 3 of the UNIX manual for more details.
 %
 %   See also GD_MOVE_ALIAS, GD_OPEN
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  char *field_code;
  int new_frag, move_data = 0;

  GDMX_NO_LHS;
  GDMX_CHECK_RHS2(3,4);

  D = gdmx_to_dirfile(prhs[0]);
  field_code = gdmx_to_string(prhs, 1, 0);
  new_frag = gdmx_to_int(prhs, 2);
  if (nrhs > 3)
    move_data = gdmx_to_int(prhs, 3);

  gd_move(D, field_code, new_frag, move_data);

  mxFree(field_code);
  gdmx_err(D, 0);
}