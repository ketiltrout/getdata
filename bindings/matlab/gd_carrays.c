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
#include <string.h>

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  gd_type_t type = GD_FLOAT64;
  size_t i, n, comp;
  const gd_carray_t *c;
  mxClassID id;

  GDMX_CHECK_RHS2(1,2);

  D = gdmx_to_dirfile(prhs[0]);
  if (nrhs > 1)
    type = gdmx_to_gd_type(prhs, 1);

  id = gdmx_classid(type);
  comp = type & GD_COMPLEX;

  c = gd_carrays(D, type);

  gdmx_err(D, 0);

  /* count */
  for (n = 0; c[n].n; ++n)
    ;

  /* convert to array of arrays */
  plhs[0] = mxCreateCellMatrix(1, n);

  for (n = 0; c[n].n; ++n) {
    mxArray *a = mxCreateNumericMatrix(1, c[n].n, id,
        comp ? mxCOMPLEX : mxREAL);
    void *pr = mxGetData(a);

    if (type == GD_COMPLEX128) {
      double *pi = mxGetImagData(a);
      for (i = 0; i < c[n].n; ++i) {
        ((double*)pr)[i] = ((double*)c[n].d)[2 * i];
        pi[i] = ((double*)c[n].d)[2 * i + 1];
      }
    } else if (type == GD_COMPLEX64) {
      float *pi = mxGetImagData(a);
      for (i = 0; i < c[n].n; ++i) {
        ((float*)pr)[i] = ((float*)c[n].d)[2 * i];
        pi[i] = ((float*)c[n].d)[2 * i + 1];
      }
    } else
      memcpy(pr, c[n].d, GD_SIZE(type) * c[n].n);

    mxSetCell(plhs[0], n, a);
  }
}
