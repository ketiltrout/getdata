/* Copyright (C) 2013, 2015, 2016 D. V. Wiebe
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
 % GD_GETDATA  Retrieve vector data
 %
 %   V = GD_GETDATA(DIRFILE,FIELD_CODE,FIRST_FRAME,FIRST_SAMP,NFRAMES,NSAMP ...
 %                  [,TYPE])
 %             retrieves a vector, V, of NFRAMES frames plus NSAMPLES samples of
 %             data from the vector field FIELD_CODE starting FIRST_SAMP samples
 %             past the start of FIRST_FRAME.  If TYPE is given, it should be
 %             one of the data type symbols provided by GETDATA_CONSTANTS,
 %             otherwise the data are returned in their native type.
 %
 %             If TYPE is the NULL data type, this function simply returns the
 %             number of samples read, instead of a vector of data.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_getdata(3) in
 %   section 3 of the UNIX manual for more details.
 %
 %   See also GD_PUTDATA, GD_OPEN
 */


void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  char *field_code;
  int64_t first_frame, first_samp;
  size_t nsamp;
  gd_type_t type;
  void *data;

  GDMX_CHECK_RHS2(6,7);

  D = gdmx_to_dirfile(prhs[0]);
  field_code = gdmx_to_string(prhs, 1, 0);
  first_frame = gdmx_to_int64(prhs, 2);
  first_samp = gdmx_to_int64(prhs, 3);
  nsamp = gdmx_to_nsamp(D, field_code, prhs, 4, 5);
  if (nrhs > 6)
    type = gdmx_to_gd_type(prhs, 6);
  else {
    type = gd_native_type(D, field_code);
    gdmx_err(D, 0);
  }

  if (gd_entry_type(D, field_code) == GD_SINDIR_ENTRY) {
    const char **data = mxMalloc(sizeof(*data) * nsamp);

    nsamp = gd_getdata64(D, field_code, (gd_off64_t)first_frame,
        (gd_off64_t)first_samp, 0, nsamp, GD_STRING, data);

    mxFree(field_code);
    gdmx_err(D, 0);

    plhs[0] = gdmx_from_nstring_list(data, nsamp);
    mxFree(data);
  } else if (type == GD_NULL) {
    nsamp = gd_getdata64(D, field_code, (gd_off64_t)first_frame,
        (gd_off64_t)first_samp, 0, nsamp, GD_NULL, NULL);

    mxFree(field_code);
    gdmx_err(D, 0);

    plhs[0] = gdmx_from_int64(nsamp);
  } else {
    plhs[0] = gdmx_vector(type, nsamp, &data);

    nsamp = gd_getdata64(D, field_code, (gd_off64_t)first_frame,
        (gd_off64_t)first_samp, 0, nsamp, type, data);

    mxFree(field_code);
    gdmx_err(D, 0);

    gdmx_fix_vector(plhs[0], type, nsamp, data);
  }
}
