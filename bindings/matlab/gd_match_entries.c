/* Copyright (C) 2016 D. V. Wiebe
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
 % GD_MATCH_ENTRIES  Retrieve a list of field names
 %
 %   L = GD_MATCH_ENTRIES(DIRFILE [, REGEX [, FRAGMENT [, TYPE [, FLAGS]]]])
 %             returns a cell array of strings, L, listing all fields in the
 %             dirfile DIRFILE which satisfy the supplied REGEX, FRAGMENT,
 %             TYPE, and/or FLAGS.
 %
 %   If REGEX is given, and not numeric zero, it provides a regular expression
 %   to match against the field lists.  If FRAGMENT is given and not
 %   GD.ALL_FRAGMENTS, only the specified fragment is searched.  If non-zero
 %   (matching all types), TYPE should be either one of the GD.xxx_ENTRY
 %   symbols or else one of the special GD.xxx_ENTRIES symbols provided by
 %   GETDATA_CONSTANTS.  FLAGS, if given, should be zero or more of the
 %   GD.ENTRIES_... and GD.REGEX_... flags, bitwise or'd together.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_match_entries(3) in
 %   section 3 of the UNIX manual for more details.
 %
 %   See also GD_ENTRY_LIST, GD_OPEN, GETDATA_CONSTANTS
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  char *regex = NULL;
  int fragment = GD_ALL_FRAGMENTS, type = 0;
  unsigned int flags = 0;
  const char **fl;

  GDMX_CHECK_RHS2(1,5);

  D = gdmx_to_dirfile(prhs[0]);
  if (nrhs > 1)
    regex = gdmx_to_string(prhs, 1, 1);
  if (nrhs > 2)
    fragment = gdmx_to_int(prhs, 2);
  if (nrhs > 3)
    type = gdmx_to_int(prhs, 3);
  if (nrhs > 4)
    flags = gdmx_to_uint(prhs, 4);

  gd_match_entries(D, regex, fragment, type, flags, &fl);

  mxFree(regex);
  gdmx_err(D, 0);

  plhs[0] = gdmx_from_string_list(fl);
}
