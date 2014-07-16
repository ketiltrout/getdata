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
 % GD_GETSTRDATA  Retrieve string vector data
 %
 %   V = GD_GETSTRDATA(DIRFILE,FIELD_CODE,FIRST_FRAME,FIRST_SAMP,NFRAMES,NSAMP)
 %             retrieves a cell array, V, of NFRAMES frames plus NSAMPLES
 %             samples of string data from the SINDIR vector field FIELD_CODE
 %             starting FIRST_SAMP samples past the start of FIRST_FRAME.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_getstrdata(3) in
 %   section 3 of the UNIX manual for more details.
 %
 %   See also GD_PUTDATA, GD_OPEN
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  const char **data;
  char *field_code;
  long long first_frame, first_samp;
  size_t nsamp;

  GDMX_CHECK_RHS(6);

  D = gdmx_to_dirfile(prhs[0]);
  field_code = gdmx_to_string(prhs, 1, 0);
  first_frame = gdmx_to_llong(prhs, 2);
  first_samp = gdmx_to_llong(prhs, 3);
  nsamp = gdmx_to_nsamp(D, field_code, prhs, 4, 5);

  data = mxMalloc(sizeof(*data) * nsamp);

  gd_getstrdata64(D, field_code, (gd_off64_t)first_frame,
      (gd_off64_t)first_samp, 0, nsamp, data);

  mxFree(field_code);
  gdmx_err(D, 0);

  plhs[0] = gdmx_from_nstring_list(data, nsamp);
  mxFree(data);
}
