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
 % GD_OPEN  Open or create a dirfile
 %
 %   D = GD_OPEN(PATH[,FLAGS])
 %             opens the dirfile PATH, returning a dirfile object, D.  If
 %             given, FLAGS should be a bitwise or'd collection of dirfile flags
 %             provided by GETDATA_CONSTANTS.  If omitted, FLAGS defaults to
 %             GD.RDONLY.
 %
 %   Unlike in the C API, an error in GD_OPEN does not result in the creation of
 %   an invalid dirfile (so GD_ERROR and GD_ERROR_STRING cannot be used in a
 %   CATCH block after a failed a GD_OPEN call).  Open dirfiles should be
 %   deallocated by calling GD_DISCARD or GD_CLOSE when no longer needed.
 %
 %   See the documentation on the C API function gd_open(3) in section 3
 %   of the UNIX manual for more details.
 %
 %   See also GD_DISCARD, GD_CLOSE, GETDATA_CONSTANTS
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  char *filename;
  unsigned long flags = GD_RDONLY;
  DIRFILE *D;

  GDMX_CHECK_RHS2(1,2);

  filename = gdmx_to_string(prhs, 0, 0);

  if (nrhs > 1)
    flags = gdmx_to_ulong(prhs, 1);

  gdmx_initialise();

  D = gd_open(filename, flags);
  mxFree(filename);

  gdmx_err(D, 1);

  plhs[0] = gdmx_from_dirfile(D);
}
