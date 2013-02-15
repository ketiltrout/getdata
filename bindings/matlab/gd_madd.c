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
 % GD_MADD  Add a metafield
 %
 %   GD_MADD(DIRFILE,ENTRY,PARENT)
 %             adds a metafield described by ENTRY to the dirfile DIRFILE under
 %             parent field PARENT.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   The ENTRY object should be an entry struct; see GETDATA and gd_entry(3) in
 %   the UNIX manual for details.
 %
 %   See the documentation on the C API function gd_madd(3) in section 3
 %   of the UNIX manual for more details.
 %
 %   See also GD_ADD, GD_OPEN, GETDATA
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  void *data;
  char *parent;
  gd_entry_t *E;

  GDMX_NO_LHS;
  GDMX_CHECK_RHS(3);

  D = gdmx_to_dirfile(prhs[0]);
  E = gdmx_to_entry(prhs, 1, GDMX_ENO_FRAG);
  parent = gdmx_to_string(prhs, 2, 0);

  gd_madd(D, E, parent);

  gdmx_free_entry(E);
  gdmx_err(D, 0);
}
