// Copyright (C) 2009, 2010, 2011, 2013 D. V. Wiebe
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

PolynomEntry::PolynomEntry(const char* field_code, int poly_ord,
    const char* in_field, double* a, int fragment_index) : Entry()
{
  int i;

  E.field = strdup(field_code);
  E.field_type = GD_POLYNOM_ENTRY;
  E.u.polynom.poly_ord = poly_ord;
  E.fragment_index = fragment_index;
  E.flags = 0;
  E.in_fields[0] = strdup(in_field);
  for (i = 0; i <= poly_ord; ++i)
    E.u.polynom.a[i] = a[i];
}

PolynomEntry::PolynomEntry(const char* field_code, int poly_ord,
    const char* in_field, std::complex<double>* ca, int fragment_index) :
  Entry()
{
  int i;

  E.field = strdup(field_code);
  E.field_type = GD_POLYNOM_ENTRY;
  E.u.polynom.poly_ord = poly_ord;
  E.fragment_index = fragment_index;
  E.flags = GD_EN_COMPSCAL;
  E.in_fields[0] = strdup(in_field);
  for (i = 0; i <= poly_ord; ++i) {
    E.u.polynom.ca[i][0] = ca[i].real();
    E.u.polynom.ca[i][1] = ca[i].imag();
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
    return gd_alter_entry(D->D, E.field, &E, 0);
  
  return 0;
}

int PolynomEntry::SetCoefficient(double coeff, int index)
{
  if (index < 0 || index > GD_MAX_POLYORD)
    return -1;

  E.u.polynom.ca[index][0] = E.u.polynom.a[index] = coeff;
  E.u.polynom.ca[index][1] = 0;

  if (D != NULL)
    return gd_alter_entry(D->D, E.field, &E, 0);
  
  return 0;
}

int PolynomEntry::SetCoefficient(const char *scale, int index)
{
  int r = 0;

  if (index < 0 || index > GD_MAX_POLYORD)
    return -1;

  SetScalar(index, scale);

  if (D != NULL) {
    r = gd_alter_entry(D->D, E.field, &E, 0);

    if (!r) {
      r = gd_get_constant(D->D, scale, GD_COMPLEX128, E.u.polynom.ca + index);
      E.u.polynom.a[index] = E.u.polynom.ca[index][0];
    }
  }
  
  return r;
}

int PolynomEntry::SetCoefficient(std::complex<double> coeff, int index)
{
  if (index < 0 || index > GD_MAX_POLYORD)
    return -1;

  E.u.polynom.a[index] = E.u.polynom.ca[index][0] = coeff.real();
  E.u.polynom.ca[index][1] = coeff.imag();
  E.flags = GD_EN_COMPSCAL;

  if (D != NULL)
    return gd_alter_entry(D->D, E.field, &E, 0);
  
  return 0;
}

int PolynomEntry::SetPolyOrd(int poly_ord)
{
  int old_n = E.u.polynom.poly_ord;

  if (poly_ord < 2 || poly_ord > GD_MAX_POLYORD)
    return -1;

  if (poly_ord > old_n) {
    int i;

    for (i = old_n + 1; i <= poly_ord; ++i)
      E.u.polynom.a[i] = 0;
  }

  E.u.polynom.poly_ord = poly_ord;

  if (D != NULL)
    return gd_alter_entry(D->D, E.field, &E, 0);

  return 0;
}

const char *PolynomEntry::Scalar(int index) const
{
  if (index < 0 || index > E.u.polynom.poly_ord)
    return NULL;

  return E.scalar[index];
}

int PolynomEntry::ScalarIndex(int index) const
{
  if (index < 0 || index > E.u.polynom.poly_ord)
    return 0;

  return E.scalar_ind[index];
}
