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
 % GD_ALTER_AFFIXES  Modify the affixes of an included fragment
 %
 %   GD_ALTER_ENDIANNESS(DIRFILE,FRAGMENT,PREFIX,SUFFIX)
 %             modifies the prefix and suffix associated with the fragment
 %             indexed by FRAGMENT in the dirfile DIRFILE.  If PREFIX or SUFFIX
 %             is numeric zero, no change is made to the associated affix.
 %             Otherwise, the affix is set to the value of the parameter.  To
 %             remove an affix, set it to the empty string: ''.
 %             Otherwise, the binary files are not modified.
 %
 %   The DIRFILE object should have previously been created with GD_OPEN.
 %
 %   See the documentation on the C API function gd_alter_affixes(3) in
 %   section 3 of the UNIX manual for more details.
 %
 %   See also GD_FRAGMENT_AFFIXES, GD_INCLUDE, GD_OPEN
 */

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  int i;
  char *p = NULL, *s = NULL;

  GDMX_NO_LHS;
  GDMX_CHECK_RHS(4);

  D = gdmx_to_dirfile(prhs[0]);
  i = gdmx_to_int(prhs, 1);
  p = gdmx_to_string(prhs, 2, 1);
  s = gdmx_to_string(prhs, 3, 1);

  gd_alter_affixes(D, i, p, s);

  mxFree(p);
  mxFree(s);
  gdmx_err(D, 0);
}
