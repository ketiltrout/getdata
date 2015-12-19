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
 % GD_SEEK  Set the location of a field's I/O pointer
 %
 %   P = GD_SEEK(DIRFILE,FIELD_CODE,FRAME,SAMPLE,FLAGS)
 %             places the I/O pointer of the field FIELD_CODE SAMPLE samples
 %             after the start of frame number FRAME.  Flags should be one of
 %             GD.SEEK_SET, GD.SEEK_CUR, or GD.SEEK_END, possibly bitwise or'd
 %             with GD.SEEK_WRITE.  These symbols are provided by
 %             GETDATA_CONSTANTS.  Returns the position, P, in samples, after
 %             setting it.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_seek(3) in section 3
 %   of the UNIX manual for more details.
 %
 %   See also GD_TELL, GD_GETDATA, GD_OPEN.
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  void *data;
  char *field_code;
  int64_t first_frame, first_samp;
  int flags;
  uint64_t pos;

  GDMX_CHECK_RHS(5);

  D = gdmx_to_dirfile(prhs[0]);
  field_code = gdmx_to_string(prhs, 1, 0);
  first_frame = gdmx_to_int64(prhs, 2);
  first_samp = gdmx_to_int64(prhs, 3);
  flags = gdmx_to_int(prhs, 4);

  pos = gd_seek64(D, field_code, (gd_off64_t)first_frame,
      (gd_off64_t)first_samp, flags);

  mxFree(field_code);
  gdmx_err(D, 0);

  plhs[0] = gdmx_from_uint64(pos);
}
