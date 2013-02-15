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
 % GD_ALTER_ENDIANNESS  Modify the byte sex of a fragment
 %
 %   GD_ALTER_ENDIANNESS(DIRFILE,ENDIANNESS,FRAGMENT[,RECODE])
 %             sets the byte sex of the metadata fragment indexed by FRAGMENT
 %             in the dirfile DIRFILE to ENDIANNESS, which should be one of:
 %             numeric zero, indicating the native type of the platform;
 %             GD.BIG_ENDIAN, or GD.LITTLE_ENDIAN, optionally bitwise or'd with
 %             either GD.ARM_ENDIAN or GD.NOT_ARM_ENDIAN.  See GETDATA_CONSTANTS
 %             for these constants.  If RECODE is given and non-zero, binary
 %             files affected by the byte sex change will be converted.
 %             Otherwise, the binary files are not modified.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_alter_endianness(3) in
 %   section 3 of the UNIX manual for more details.
 %
 %   See also GD_ENDIANNESS, GD_OPEN, GETDATA_CONSTANTS
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  void *data;
  int i, r = 0;
  unsigned long e;

  GDMX_NO_LHS;
  GDMX_CHECK_RHS2(3,4);

  D = gdmx_to_dirfile(prhs[0]);
  e = gdmx_to_ulong(prhs, 1);
  i = gdmx_to_int(prhs, 2);
  if (nrhs > 3)
    r = gdmx_to_int(prhs, 3);

  gd_alter_endianness(D, e, i, r);

  gdmx_err(D, 0);
}
