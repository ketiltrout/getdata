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
 % GD_SYNC  Flush modifications to disk
 %
 %   GD_SYNC(DIRFILE[,FIELD_CODE])
 %             flushes the binary data files associated with the dirfile DIRFILE
 %             to disk.  If FIELD_CODE is given and not numeric zero, only the
 %             specified field is synced.  Otherwise all open data files are
 %             synced.  In this second case, modified metadata is also flushed.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_sync(3) in section 3
 %   of the UNIX manual for more details.
 %
 %   See also GD_CLOSE, GD_FLUSH, GD_RAW_CLOSE, GD_OPEN
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  char *field_code = NULL;

  GDMX_NO_LHS;
  GDMX_CHECK_RHS2(1,2);

  D = gdmx_to_dirfile(prhs[0]);
  if (nrhs > 1)
    field_code = gdmx_to_string(prhs, 1, 1);

  gd_sync(D, field_code);

  mxFree(field_code);
  gdmx_err(D, 0);
}
