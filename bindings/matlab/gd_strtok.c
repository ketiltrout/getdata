/* Copyright (C) 2013, 2016 D. V. Wiebe
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
 % GD_STRTOK  Tokenise a string using the GetData parser
 %
 %   A = GD_STRTOK(DIRFILE,STRING)
 %             Converts the string STRING into a cell array of strings, A, by
 %             tokenising it using the GetData library parser.  Unlike the C API
 %             this function completely tokenises the provided string at once,
 %             returning all tokens, and should not be called repeatedly.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_strtok(3) in section 3
 %   of the UNIX manual for more details.
 %
 %   See also GD_OPEN, GD_DIRFILE_STANDARDS
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  char *tok, *s;
  mxArray **sl = NULL;
  size_t i, n = 0;

  GDMX_CHECK_RHS(2);

  D = gdmx_to_dirfile(prhs[0]);
  s = gdmx_to_string(prhs, 1, 0);

  /* convert */
  for (tok = gd_strtok(D, s); tok; tok = gd_strtok(D, NULL)) {
    mxArray **ptr = (mxArray**)mxRealloc(sl, sizeof(mxArray*) * (n + 1));
    if (ptr == NULL)
      mexErrMsgIdAndTxt("GetData:GDMX:Alloc", "Out of memory.");

    sl = ptr;
    sl[n++] = mxCreateString(tok);
    mxFree(tok);
  }

  gdmx_err(D, 0);

  plhs[0] = mxCreateCellMatrix(1, n);
  for (i = 0; i < n; ++i)
    mxSetCell(plhs[0], i, sl[i]);
  mxFree(sl);
}
