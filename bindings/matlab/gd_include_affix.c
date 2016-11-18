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
 % GD_INCLUDE  Add a format metadata fragment
 %
 %   GD_INCLUDE_AFFIX(DIRFILE,PATH,PARENT[,PREFIX[,SUFFIX[,FLAGS]]])
 %             adds the fragment at PATH to the open dirfile DIRFILE below the
 %             current fragment indexed by PARENT.  If given, FLAGS should be
 %             zero or more Dirfile open flags provided by GETDATA_CONSTANTS,
 %             bitwise-or'd together.  If given an not numeric zero, PREFIX and
 %             SUFFIX provide the fragment affixes.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_include_affix(3) in
 %   section 3 of the UNIX manual for more details.
 %
 %   See also GD_INCLUDE, GD_UNINCLUDE, GD_FRAGMENT_AFFIXES, GD_ALTER_AFFIXES,
 %   GD_OPEN
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  void *data;
  char *filename;
  char *prefix = NULL, *suffix = NULL;
  int parent, n;
  unsigned long flags = 0;

  GDMX_CHECK_RHS2(3,6);

  D = gdmx_to_dirfile(prhs[0]);
  filename = gdmx_to_string(prhs, 1, 0);
  parent = gdmx_to_int(prhs, 2);
  if (nrhs > 3)
    prefix = gdmx_to_string(prhs, 3, 1);
  if (nrhs > 4)
    suffix = gdmx_to_string(prhs, 4, 1);
  if (nrhs > 5)
    flags = gdmx_to_ulong(prhs, 5);

  n = gd_include_affix(D, filename, parent, prefix, suffix, flags);

  mxFree(filename);
  mxFree(prefix);
  mxFree(suffix);
  gdmx_err(D, 0);

  plhs[0] = gdmx_from_int(n);
}
