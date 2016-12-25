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
 % GD_NENTRIES  Retrieve a count of fields
 %
 %   N = GD_NENTRIES(DIRFILE [, PARENT [, TYPE [, FLAGS]]])
 %             returns the number of fields, N, in the dirfile DIRFILE which
 %             satisfy the supplied PARENT, TYPE, and/or FLAGS.
 %
 %   If PARENT is given, and not numeric zero, metafields of the specified
 %   parent field will be searched.  Otherwise, only top level fields are
 %   considered.  If non-zero, TYPE should be either one of the GD.xxx_ENTRY
 %   symbols or else one of the special GD.xxx_ENTRIES symbols provided by
 %   GETDATA_CONSTANTS.  FLAGS, if given, should be zero or more of the
 %   GD.ENTRIES_... flags, bitwise or'd together.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_nentries(3) in
 %   section 3 of the UNIX manual for more details.
 %
 %   See also GD_ENTRY_LIST, GD_OPEN, GETDATA_CONSTANTS
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  char *parent = NULL;
  unsigned int n, type = 0, flags = 0;

  GDMX_CHECK_RHS2(1,4);

  D = gdmx_to_dirfile(prhs[0]);
  if (nrhs > 1)
    parent = gdmx_to_string(prhs, 1, 1);
  if (nrhs > 2)
    type = gdmx_to_uint(prhs, 2);
  if (nrhs > 3)
    flags = gdmx_to_uint(prhs, 3);

  n = gd_nentries(D, parent, type, flags);

  mxFree(parent);
  gdmx_err(D, 0);

  plhs[0] = gdmx_from_uint(n);
}
