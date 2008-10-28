// (C) 2008 D. V. Wiebe
//
//#########################################################################
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
#include "getdata/entry.h"

#include <cstring>

using namespace GetData;

Entry::Entry()
{
  memset(&E, 0, sizeof(E));
}

Entry::Entry(DIRFILE *dirfile, const char* field_code)
{
  get_entry(dirfile, field_code, &E);
}

Entry::~Entry()
{
  dirfile_free_entry_strings(&E);
}

int Entry::CheckIndex(gd_entype_t field_type, int n_fields, int index)
{
  if (index < 0)
    return 0;

  switch (field_type) {
    case GD_RAW_ENTRY:
      return 0;
    case GD_LINCOM_ENTRY:
      if (index > n_fields)
        return 0;
      break;
    case GD_MULTIPLY_ENTRY:
      if (index > 2)
        return 0;
    default:
      if (index > 1)
        return 0;
  }

  return 1;
}

RawEntry::RawEntry(const char* field_code, DataType data_type, unsigned int spf,
      int fragment_index) : Entry::Entry()
{
  E.field = strdup(field_code);
  E.field_type = GD_RAW_ENTRY;
  E.spf = spf;
  E.data_type = (gd_type_t)data_type;
  E.fragment_index = fragment_index;
}

LincomEntry::LincomEntry(const char* field_code, int n_fields,
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

LinterpEntry::LinterpEntry(const char* field_code, const char* in_field,
    const char* table, int fragment_index) : Entry::Entry()
{
  E.field = strdup(field_code);
  E.field_type = GD_LINTERP_ENTRY;
  E.in_fields[0] = strdup(in_field);
  E.table = strdup(table);
  E.fragment_index = fragment_index;
}

BitEntry::BitEntry(const char* field_code, const char* in_field, int bitnum,
    int numbits, int fragment_index) : Entry::Entry()
{
  E.field = strdup(field_code);
  E.field_type = GD_BIT_ENTRY;
  E.in_fields[0] = strdup(in_field);
  E.bitnum = bitnum;
  E.numbits = numbits;
  E.fragment_index = fragment_index;
}

MultiplyEntry::MultiplyEntry(const char* field_code, const char* in_field1,
    const char* in_field2, int fragment_index) : Entry::Entry()
{
  E.field = strdup(field_code);
  E.field_type = GD_MULTIPLY_ENTRY;
  E.in_fields[0] = strdup(in_field1);
  E.in_fields[1] = strdup(in_field2);
  E.fragment_index = fragment_index;
}

PhaseEntry::PhaseEntry(const char* field_code, const char* in_field, int shift,
    int fragment_index) : Entry::Entry()
{
  E.field = strdup(field_code);
  E.field_type = GD_PHASE_ENTRY;
  E.in_fields[0] = strdup(in_field);
  E.shift = shift;
  E.fragment_index = fragment_index;
}
