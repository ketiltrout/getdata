// (C) 2009 D. V. Wiebe
//
///////////////////////////////////////////////////////////////////////////
//
// This file is part of the GetData project.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// GetData is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.
//
// You should have received a copy of the GNU General Public License
// along with GetData; if not, write to the Free Software Foundation,
// Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
//
#include "getdata/polynomentry.h"
#include "getdata/entry.h"
#include "getdata/dirfile.h"

#include <stdlib.h>
#include <cstring>

using namespace GetData;

PolynomEntry::PolynomEntry(const char* field_code, int n_fields,
    const char** in_fields, double* m, double* b, int fragment_index) :
  Entry::Entry()
{
  int i;

  E.field = strdup(field_code);
  E.field_type = GD_LINCOM_ENTRY;
  E.n_fields = n_fields;
  E.fragment_index = fragment_index;
  for (i = 0; i < n_fields; ++i) {
    E.in_fields[i] = strdup(in_fields[i]);
    E.m[i] = m[i];
    E.b[i] = b[i];
  }
}

int PolynomEntry::SetInput(const char* field)
{
  char* ptr = strdup(field);

  if (ptr == NULL)
    return -1;

  free(E.in_fields[0]);
  E.in_fields[0] = ptr;

  if (D != NULL)
    return dirfile_alter_entry(D->D, E.field, &E, 0);
  
  return 0;
}

int PolynomEntry::SetCoefficient(double coeff, int index)
{
  if (index < 0 || index > 5)
    return -1;

  E.a[index] = coeff;

  if (D != NULL)
    return dirfile_alter_entry(D->D, E.field, &E, 0);
  
  return 0;
}

int PolynomEntry::SetPolyOrd(int poly_ord)
{
  int old_n = E.poly_ord;

  if (poly_ord < 2 || poly_ord > 5)
    return -1;

  if (poly_ord > old_n) {
    int i;

    for (i = old_n + 1; i <= poly_ord; ++i)
      E.a[i] = 0;
  }

  E.poly_ord = poly_ord;

  if (D != NULL)
    return dirfile_alter_entry(D->D, E.field, &E, 0);

  return 0;
}
