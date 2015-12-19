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
 % GD_BOF  Report the start of data in a field
 %
 %   S = GD_BOF(DIRFILE,FIELD_CODE)
 %             reports the sample number, S,  where the data of the field
 %             specified by FIELD_CODE starts.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_bof(3) in section 3
 %   of the UNIX manual for more details.
 %
 %   See also GD_EOF, GD_OPEN, GD_NFRAMES
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  void *data;
  char *field_code;
  uint64_t n;

  GDMX_CHECK_RHS(2);

  D = gdmx_to_dirfile(prhs[0]);
  field_code = gdmx_to_string(prhs, 1, 0);

  n = gd_bof64(D, field_code);

  mxFree(field_code);
  gdmx_err(D, 0);

  plhs[0] = gdmx_from_uint64(n);
}
