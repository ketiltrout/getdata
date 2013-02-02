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
  double r, value;
  long long first_samp = 0, last_samp = 0;

  GDMX_CHECK_RHS2(3,5);

  D = gdmx_to_dirfile(prhs[0]);
  field_code = gdmx_to_string(prhs, 1, 0);
  value = gdmx_to_double(prhs, 2);
  if (nrhs > 3)
    first_samp = gdmx_to_llong(prhs, 3);
  if (nrhs > 4)
    last_samp = gdmx_to_llong(prhs, 4);

  r = gd_framenum_subset64(D, field_code, value, first_samp, last_samp);

  mxFree(field_code);
  gdmx_err(D, 0);

  plhs[0] = gdmx_from_double(r);
}
