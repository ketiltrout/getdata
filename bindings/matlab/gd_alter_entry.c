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
 % GD_ALTER_ENTRY  Modify field metadata
 %
 %   GD_ALTER_ENTRY(DIRFILE,FIELD_CODE,ENTRY[,RECODE])
 %             modifies the metadata of the field FIELD_CODE in the dirfile
 %             DIRFILE according to the contents of the ENTRY struct.  If
 %             RECODE is given and non-zero, other data will be updated to
 %             reflect metadata changes.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   This function ignores both the .FIELD member and the .FRAGMENT_INDEX
 %   member of the supplied ENTRY structure.  They may be omitted.  For a
 %   discussion of the ENTRY structure, see GETDATA.
 %
 %   See the documentation on the C API function gd_alter_endianness(3) in
 %   section 3 of the UNIX manual for more details.
 %
 %   See also GD_ADD, GD_OPEN, GETDATA
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  void *data;
  char *field_code;
  gd_entry_t *E;
  int recode = 0;

  GDMX_NO_LHS;
  GDMX_CHECK_RHS2(3,4);

  D = gdmx_to_dirfile(prhs[0]);
  field_code = gdmx_to_string(prhs, 1, 0);
  E = gdmx_to_entry(prhs, 2, GDMX_ENO_FRAG | GDMX_ENO_FIELD);
  if (nrhs > 3)
    recode = gdmx_to_int(prhs, 3);

  gd_alter_entry(D, field_code, E, recode);

  mxFree(field_code);
  gdmx_free_entry(E);
  gdmx_err(D, 0);
}
