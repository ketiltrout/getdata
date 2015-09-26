// Copyright (C) 2010, 2011, 2013 D. V. Wiebe
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

RecipEntry::RecipEntry(const char* field_code, const char* in_field1,
    double dividend, int fragment_index) : Entry()
{
  E.field = strdup(field_code);
  E.field_type = GD_RECIP_ENTRY;
  E.in_fields[0] = strdup(in_field1);
  E.scalar[0] = 0;
  E.u.recip.cdividend[0] = E.u.recip.dividend = dividend;
  E.u.recip.cdividend[1] = 0;
  E.flags = 0;
  E.fragment_index = fragment_index;
}

RecipEntry::RecipEntry(const char* field_code, const char* in_field1,
    std::complex<double> dividend, int fragment_index) : Entry()
{
  E.field = strdup(field_code);
  E.field_type = GD_RECIP_ENTRY;
  E.in_fields[0] = strdup(in_field1);
  E.scalar[0] = 0;
  E.u.recip.cdividend[0] = E.u.recip.dividend = dividend.real();
  E.u.recip.cdividend[1] = dividend.imag();
  E.flags = GD_EN_COMPSCAL;
  E.fragment_index = fragment_index;
}

int RecipEntry::SetInput(const char* field)
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

int RecipEntry::SetDividend(double dividend)
{
  E.u.recip.cdividend[0] = E.u.recip.dividend = dividend;
  E.u.recip.cdividend[1] = 0;

  if (D != NULL)
    return gd_alter_entry(D->D, E.field, &E, 0);
  
  return 0;
}

int RecipEntry::SetDividend(const char *scale)
{
  int r = 0;

  SetScalar(0, scale);

  if (D != NULL) {
    r = gd_alter_entry(D->D, E.field, &E, 0);

    if (!r) {
      r = gd_get_constant(D->D, scale, GD_COMPLEX128, &E.u.recip.cdividend);
      E.u.recip.dividend = E.u.recip.cdividend[0];
    }
  }
  
  return r;
}

int RecipEntry::SetDividend(std::complex<double> dividend)
{
  E.u.recip.dividend = E.u.recip.cdividend[0] = dividend.real();
  E.u.recip.cdividend[1] = dividend.imag();
  E.flags = GD_EN_COMPSCAL;

  if (D != NULL)
    return gd_alter_entry(D->D, E.field, &E, 0);
  
  return 0;
}
