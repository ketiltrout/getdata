/* Copyright (C) 2013, 2014, 2015 D. V. Wiebe
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
 % GD_PUTDATA  Write vector data
 %
 %   GD_PUTDATA(DIRFILE,FIELD_CODE,FIRST_FRAME,FIRST_SAMPLE,DATA)
 %             writes the data in DATA to the field called FIELD_CODE.  The
 %             sequential samples are written starting FIRST_SAMPLE samples
 %             after the start of frame FIRST_FRAME.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_putdata(3) in section 3
 %   of the UNIX manual for more details.
 %
 %   See also GD_GETDATA, GD_PUT_CARRAY_SLICE, GD_OPEN
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  void *data;
  char *field_code;
  int64_t first_frame, first_samp;
  size_t nsamp, n;
  gd_type_t type;

  GDMX_CHECK_RHS(5);

  D = gdmx_to_dirfile(prhs[0]);
  field_code = gdmx_to_string(prhs, 1, 0);
  first_frame = gdmx_to_int64(prhs, 2);
  first_samp = gdmx_to_int64(prhs, 3);
  gdmx_to_data(&data, &type, &nsamp, prhs[4], 4);

  n = gd_putdata64(D, field_code, (gd_off64_t)first_frame,
      (gd_off64_t)first_samp, 0, nsamp, type, data);

  gdmx_free_data(data, type);
  mxFree(field_code);
  gdmx_err(D, 0);

  plhs[0] = gdmx_from_size_t(n);
}
