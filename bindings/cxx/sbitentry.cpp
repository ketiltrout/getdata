// (C) 2008-2010 D. V. Wiebe
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
#ifdef HAVE_CONFIG_H
#include "../../src/config.h"
#endif
#undef GETDATA_LEGACY_API
#include "getdata/dirfile.h"

#include <cstring>
#include <stdlib.h>

using namespace GetData;

SBitEntry::SBitEntry(const char* field_code, const char* in_field,
    gd_bit_t bitnum, gd_bit_t numbits, int fragment_index) : Entry::Entry()
{
  E.field = strdup(field_code);
  E.field_type = GD_BIT_ENTRY;
  E.in_fields[0] = strdup(in_field);
  E.bitnum = bitnum;
  E.numbits = numbits;
  E.fragment_index = fragment_index;
}

int SBitEntry::SetInput(const char* field)
{
  char* ptr = strdup(field);

  if (ptr == NULL)
    return 0;

  free(E.in_fields[0]);
  E.in_fields[0] = ptr;

  if (D != NULL)
    return gd_alter_entry(D->D, E.field, &E, 0);
  
  return 0;
}

int SBitEntry::SetFirstBit(gd_bit_t first_bit)
{
  E.bitnum = first_bit;

  if (D != NULL)
    return gd_alter_entry(D->D, E.field, &E, 0);
  
  return 0;
}

int SBitEntry::SetNumBits(gd_bit_t num_bits)
{
  E.numbits = num_bits;

  if (D != NULL)
    return gd_alter_entry(D->D, E.field, &E, 0);
  
  return 0;
}

const char *SBitEntry::Scalar(int index)
{
  if (index != 0 && index != 1)
    return NULL;

  return E.scalar[index];
}

int SBitEntry::SetFirstBit(const char *first_bit)
{
  int r = 0;

  free(E.scalar[0]);
  if (first_bit == NULL)
    E.scalar[0] = NULL;
  else
    E.scalar[0] = strdup(first_bit);

  if (D != NULL) {
    r = gd_alter_entry(D->D, E.field, &E, 0);

    if (!r)
      r = gd_get_constant(D->D, first_bit, GD_INT16, &E.bitnum);
  }
  
  return r;
}

int SBitEntry::SetNumBits(const char *num_bits)
{
  int r = 0;

  free(E.scalar[1]);
  if (num_bits == NULL)
    E.scalar[1] = NULL;
  else
    E.scalar[1] = strdup(num_bits);

  if (D != NULL) {
    r = gd_alter_entry(D->D, E.field, &E, 0);

    if (!r)
      r = gd_get_constant(D->D, num_bits, GD_INT16, &E.numbits);
  }
  
  return r;
}
