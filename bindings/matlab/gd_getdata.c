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

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  void *data;
  char *field_code;
  long long first_frame, first_samp;
  size_t nsamp;
  gd_type_t type;

  GDMX_CHECK_RHS2(6,7);

  D = gdmx_to_dirfile(prhs[0]);
  field_code = gdmx_to_string(prhs, 1, 0);
  first_frame = gdmx_to_llong(prhs, 2);
  first_samp = gdmx_to_llong(prhs, 3);
  nsamp = gdmx_to_nsamp(D, field_code, prhs, 4, 5);
  if (nrhs > 6)
    type = gdmx_to_gd_type(prhs, 6);
  else {
    type = gd_native_type(D, field_code);
    gdmx_err(D, 0);
  }

  plhs[0] = gdmx_vector(type, nsamp, &data);

  gd_getdata64(D, field_code, (gd_off64_t)first_frame, (gd_off64_t)first_samp,
      0, nsamp, type, data);

  mxFree(field_code);
  gdmx_err(D, 0);

  gdmx_fix_vector(plhs[0], type, nsamp, data);
}
