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

void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  DIRFILE *D;
  void *data;
  char *field_name;
  size_t n;
  int fragment_index;
  gd_type_t const_type, data_type;

  GDMX_NO_LHS;
  GDMX_CHECK_RHS(5);

  D = gdmx_to_dirfile(prhs[0]);
  field_name = gdmx_to_string(prhs, 1, 0);
  const_type = gdmx_to_gd_type(prhs, 2);
  gdmx_to_data(&data, &data_type, &n, prhs, 3);
  fragment_index = gdmx_to_int(prhs, 4);

  gd_add_const(D, field_name, const_type, data_type, data, fragment_index);

  gdmx_free_data(data, data_type);
  mxFree(field_name);
  gdmx_err(D, 0);
}
