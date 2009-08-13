// (C) 2008 D. V. Wiebe
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
#include "getdata/sbitentry.h"
#include "getdata/entry.h"
#include "getdata/dirfile.h"

#include <cstring>
#include <stdlib.h>

using namespace GetData;

SBitEntry::SBitEntry(const char* field_code, const char* in_field, int bitnum,
    int numbits, int fragment_index) : Entry::Entry()
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
    return dirfile_alter_entry(D->D, E.field, &E, 0);
  
  return 0;
}

int SBitEntry::SetFirstBit(int first_bit)
{
  E.bitnum = first_bit;

  if (D != NULL)
    return dirfile_alter_entry(D->D, E.field, &E, 0);
  
  return 0;
}

int SBitEntry::SetNumBits(int num_bits)
{
  E.numbits = num_bits;

  if (D != NULL)
    return dirfile_alter_entry(D->D, E.field, &E, 0);
  
  return 0;
}
