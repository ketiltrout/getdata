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
 % GD_INCLUDE  Add a format metadata fragment with a namespace
 %
 %   GD_INCLUDE(DIRFILE,PATH,PARENT,[NS[,FLAGS]])
 %             adds the fragment at PATH to the open dirfile DIRFILE below the
 %             current fragment indexed by PARENT, using NS, if given, as the
 %             new fragment's root namespace.  If given, FLAGS should be
 %             zero or more Dirfile open flags provided by GETDATA_CONSTANTS,
 %             bitwise-or'd together.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_include_ns(3) in
 %   section 3 of the UNIX manual for more details.
 %
 %   See also GD_INCLUDE_AFFIX, GD_UNINCLUDE, GD_FRAGMENT_NAMESPACE, GD_OPEN
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  void *data;
  char *filename;
  char *ns = NULL;
  int parent, n;
  unsigned long flags = 0;

  GDMX_CHECK_RHS2(3,5);

  D = gdmx_to_dirfile(prhs[0]);
  filename = gdmx_to_string(prhs, 1, 0);
  parent = gdmx_to_int(prhs, 2);
  if (nrhs > 3)
    ns = gdmx_to_string(prhs, 3, 1);
  if (nrhs > 4)
    flags = gdmx_to_ulong(prhs, 4);

  n = gd_include_ns(D, filename, parent, ns, flags);

  mxFree(filename);
  mxFree(ns);
  gdmx_err(D, 0);

  plhs[0] = gdmx_from_int(n);
}
