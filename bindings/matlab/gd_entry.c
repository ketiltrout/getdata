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
 % GD_ENTRY  Retrieve field metadata
 %
 %   E = GD_ENTRY(DIRFILE,FIELD_CODE)
 %             returns an structure, E, containing the metadata of the field
 %             specified by FIELD_CODE in the dirfile DIRFILE.  See GETDATA 
 %             and the corresponding C API documentation for information on
 %             this structure.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_entry(3) in section 3
 %   of the UNIX manual for more details.
 %
 %   See also GD_ADD, GD_ENTRY_TYPE, GD_ALTER_ENTRY, GD_OPEN, GETDATA
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  void *data;
  char *field_code;
  gd_entry_t E;

  GDMX_CHECK_RHS(2);

  D = gdmx_to_dirfile(prhs[0]);
  field_code = gdmx_to_string(prhs, 1, 0);

  gd_entry(D, field_code, &E);

  mxFree(field_code);
  gdmx_err(D, 0);

  plhs[0] = gdmx_from_entry(&E);
  gd_free_entry_strings(&E);
}
