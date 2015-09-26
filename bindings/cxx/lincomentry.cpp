// Copyright (C) 2008-2011, 2013 D. V. Wiebe
//
///////////////////////////////////////////////////////////////////////////
//
// This file is part of the GetData project.
//
// GetData is free software; you can redistribute it and/or modify it under
// the terms of the GNU Lesser General Public License as published by the
// Free Software Foundation; either version 2.1 of the License, or (at your
// option) any later version.
//
// GetData is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
// License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with GetData; if not, write to the Free Software Foundation, Inc.,
// 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
//
#include "internal.h"

using namespace GetData;

LincomEntry::LincomEntry(const char* field_code, int n_fields,
    const char** in_fields, double* m, double* b, int fragment_index) :
  Entry()
{
  int i;

  E.field = strdup(field_code);
  E.field_type = GD_LINCOM_ENTRY;
  E.u.lincom.n_fields = n_fields;
  E.fragment_index = fragment_index;
  E.flags = 0;
  for (i = 0; i < n_fields; ++i) {
    E.in_fields[i] = strdup(in_fields[i]);
    E.u.lincom.m[i] = m[i];
    E.u.lincom.b[i] = b[i];
  }
}

LincomEntry::LincomEntry(const char* field_code, int n_fields,
    const char** in_fields, std::complex<double>* cm, std::complex<double>* cb,
    int fragment_index) : Entry()
{
  int i;

  E.field = strdup(field_code);
  E.field_type = GD_LINCOM_ENTRY;
  E.u.lincom.n_fields = n_fields;
  E.fragment_index = fragment_index;
  E.flags = GD_EN_COMPSCAL;
  for (i = 0; i < n_fields; ++i) {
    E.in_fields[i] = strdup(in_fields[i]);
    E.u.lincom.cm[i][0] = cm[i].real();
    E.u.lincom.cm[i][1] = cm[i].imag();
    E.u.lincom.cb[i][0] = cb[i].real();
    E.u.lincom.cb[i][1] = cb[i].imag();
  }
}

int LincomEntry::SetInput(const char* field, int index)
{
  if (index < 0 || index >= GD_MAX_LINCOM)
    return -1;

  char* ptr = strdup(field);

  if (ptr == NULL)
    return -1;

  free(E.in_fields[index]);
  E.in_fields[index] = ptr;

  if (D != NULL)
    return gd_alter_entry(D->D, E.field, &E, 0);
  
  return 0;
}

int LincomEntry::SetScale(double scale, int index)
{
  if (index < 0 || index >= GD_MAX_LINCOM)
    return -1;

  E.u.lincom.cm[index][0] = E.u.lincom.m[index] = scale;
  E.u.lincom.cm[index][1] = 0;

  if (D != NULL)
    return gd_alter_entry(D->D, E.field, &E, 0);
  
  return 0;
}

int LincomEntry::SetScale(const char *scale, int index)
{
  int r = 0;

  if (index < 0 || index >= GD_MAX_LINCOM)
    return -1;

  SetScalar(index, scale);

  if (D != NULL) {
    r = gd_alter_entry(D->D, E.field, &E, 0);

    if (!r) {
      r = gd_get_constant(D->D, scale, GD_COMPLEX128, E.u.lincom.cm + index);
      E.u.lincom.m[index] = E.u.lincom.cm[index][0];
    }
  }
  
  return r;
}

int LincomEntry::SetScale(std::complex<double> scale, int index)
{
  if (index < 0 || index >= GD_MAX_LINCOM)
    return -1;

  E.u.lincom.m[index] = E.u.lincom.cm[index][0] = scale.real();
  E.u.lincom.cm[index][1] = scale.imag();
  E.flags = GD_EN_COMPSCAL;

  if (D != NULL)
    return gd_alter_entry(D->D, E.field, &E, 0);
  
  return 0;
}

int LincomEntry::SetOffset(double offset, int index)
{
  if (index < 0 || index >= GD_MAX_LINCOM)
    return -1;

  E.u.lincom.cb[index][0] = E.u.lincom.b[index] = offset;
  E.u.lincom.cb[index][1] = 0;

  if (D != NULL)
    return gd_alter_entry(D->D, E.field, &E, 0);
  
  return 0;
}

int LincomEntry::SetOffset(const char *scale, int index)
{
  int r = 0;

  if (index < 0 || index >= GD_MAX_LINCOM)
    return -1;

  SetScalar(index + GD_MAX_LINCOM, scale);

  if (D != NULL) {
    r = gd_alter_entry(D->D, E.field, &E, 0);

    if (!r) {
      r = gd_get_constant(D->D, scale, GD_COMPLEX128, E.u.lincom.cb + index);
      E.u.lincom.b[index] = E.u.lincom.cb[index][0];
    }
  }
  
  return r;
}

int LincomEntry::SetOffset(std::complex<double> offset, int index)
{
  if (index < 0 || index >= GD_MAX_LINCOM)
    return -1;

  E.u.lincom.b[index] = E.u.lincom.cb[index][0] = offset.real();
  E.u.lincom.cb[index][1] = offset.imag();
  E.flags = GD_EN_COMPSCAL;

  if (D != NULL)
    return gd_alter_entry(D->D, E.field, &E, 0);
  
  return 0;
}

int LincomEntry::SetNFields(int nfields)
{
  int old_n = E.u.lincom.n_fields;

  if (nfields < 1 || nfields > GD_MAX_LINCOM)
    return -1;

  if (nfields > old_n) {
    int i;

    for (i = old_n; i < nfields; ++i) {
      free(E.in_fields[i]);
      E.in_fields[i] = strdup("INDEX");
      E.u.lincom.m[i] = E.u.lincom.b[i] = 0;
    }
  }

  E.u.lincom.n_fields = nfields;

  if (D != NULL)
    return gd_alter_entry(D->D, E.field, &E, 0);

  return 0;
}

const char *LincomEntry::Scalar(int index) const
{
  if (index < 0 || index >= E.u.lincom.n_fields)
    return NULL;

  return E.scalar[index];
}

int LincomEntry::ScalarIndex(int index) const
{
  if (index < 0 || index >= E.u.lincom.n_fields)
    return 0;

  return E.scalar_ind[index];
}
