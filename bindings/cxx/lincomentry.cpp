// (C) 2008, 2009 D. V. Wiebe
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
#include "getdata/lincomentry.h"
#include "getdata/entry.h"
#include "getdata/dirfile.h"

#include <stdlib.h>
#include <cstring>

using namespace GetData;

LincomEntry::LincomEntry(const char* field_code, int n_fields,
    const char** in_fields, double* m, double* b, int fragment_index) :
  Entry::Entry()
{
  int i;

  E.field = strdup(field_code);
  E.field_type = GD_LINCOM_ENTRY;
  E.n_fields = n_fields;
  E.fragment_index = fragment_index;
  E.comp_scal = 0;
  for (i = 0; i < n_fields; ++i) {
    E.in_fields[i] = strdup(in_fields[i]);
    E.m[i] = m[i];
    E.b[i] = b[i];
  }
}

LincomEntry::LincomEntry(const char* field_code, int n_fields,
    const char** in_fields, double complex* cm, double complex* cb,
    int fragment_index) : Entry::Entry()
{
  int i;

  E.field = strdup(field_code);
  E.field_type = GD_LINCOM_ENTRY;
  E.n_fields = n_fields;
  E.fragment_index = fragment_index;
  E.comp_scal = 1;
  for (i = 0; i < n_fields; ++i) {
    E.in_fields[i] = strdup(in_fields[i]);
    E.cm[i] = cm[i];
    E.cb[i] = cb[i];
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
    return dirfile_alter_entry(D->D, E.field, &E, 0);
  
  return 0;
}

int LincomEntry::SetScale(double scale, int index)
{
  if (index < 0 || index >= GD_MAX_LINCOM)
    return -1;

  E.cm[index] = E.m[index] = scale;

  if (D != NULL)
    return dirfile_alter_entry(D->D, E.field, &E, 0);
  
  return 0;
}

int LincomEntry::SetScale(const char *scale, int index)
{
  int r = 0;
  double complex c128;

  if (index < 0 || index >= GD_MAX_LINCOM)
    return -1;

  free(E.scalar[index]);

  if (scale == NULL)
    E.scalar[index] = NULL;
  else
    E.scalar[index] = strdup(scale);

  if (D != NULL) {
    r = dirfile_alter_entry(D->D, E.field, &E, 0);

    if (!r) {
      r = get_constant(D->D, scale, GD_COMPLEX128, &c128);
      E.cm[index] = c128;
      E.m[index] = creal(c128);
    }
  }
  
  return r;
}

int LincomEntry::SetScale(double complex scale, int index)
{
  if (index < 0 || index >= GD_MAX_LINCOM)
    return -1;

  E.cm[index] = scale;
  E.m[index] = creal(E.cm[index]);
  E.comp_scal = 1;

  if (D != NULL)
    return dirfile_alter_entry(D->D, E.field, &E, 0);
  
  return 0;
}

int LincomEntry::SetOffset(double offset, int index)
{
  if (index < 0 || index >= GD_MAX_LINCOM)
    return -1;

  E.cb[index] = E.b[index] = offset;

  if (D != NULL)
    return dirfile_alter_entry(D->D, E.field, &E, 0);
  
  return 0;
}

int LincomEntry::SetOffset(const char *scale, int index)
{
  int r = 0;
  double complex c128;

  if (index < 0 || index >= GD_MAX_LINCOM)
    return -1;

  free(E.scalar[index + GD_MAX_LINCOM]);

  if (scale == NULL)
    E.scalar[index + GD_MAX_LINCOM] = NULL;
  else
    E.scalar[index + GD_MAX_LINCOM] = strdup(scale);

  if (D != NULL) {
    r = dirfile_alter_entry(D->D, E.field, &E, 0);

    if (!r) {
      r = get_constant(D->D, scale, GD_COMPLEX128, &c128);
      E.cm[index + GD_MAX_LINCOM] = c128;
      E.m[index + GD_MAX_LINCOM] = creal(c128);
    }
  }
  
  return r;
}

int LincomEntry::SetOffset(double complex offset, int index)
{
  if (index < 0 || index >= GD_MAX_LINCOM)
    return -1;

  E.cb[index] = offset;
  E.b[index] = creal(E.cb[index]);
  E.comp_scal = 1;

  if (D != NULL)
    return dirfile_alter_entry(D->D, E.field, &E, 0);
  
  return 0;
}

int LincomEntry::SetNFields(int nfields)
{
  int old_n = E.n_fields;

  if (nfields < 1 || nfields > GD_MAX_LINCOM)
    return -1;

  if (nfields > old_n) {
    int i;

    for (i = old_n; i < nfields; ++i) {
      free(E.in_fields[i]);
      E.in_fields[i] = strdup("INDEX");
      E.m[i] = E.b[i] = 0;
    }
  }

  E.n_fields = nfields;

  if (D != NULL)
    return dirfile_alter_entry(D->D, E.field, &E, 0);

  return 0;
}

const char *LincomEntry::Scalar(int index)
{
  if (index < 0 || index >= E.n_fields)
    return NULL;

  return E.scalar[index];
}
