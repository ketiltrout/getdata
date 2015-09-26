// Copyright (C) 2008-2011 D. V. Wiebe
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

LinterpEntry::LinterpEntry(const char* field_code, const char* in_field,
    const char* table, int fragment_index) : Entry()
{
  E.field = strdup(field_code);
  E.field_type = GD_LINTERP_ENTRY;
  E.in_fields[0] = strdup(in_field);
  E.u.linterp.table = strdup(table);
  E.fragment_index = fragment_index;
}

int LinterpEntry::SetInput(const char* field)
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

int LinterpEntry::SetTable(const char* table, int move_table)
{
  char* ptr = strdup(table);

  if (ptr == NULL)
    return 0;

  free(E.u.linterp.table);
  E.u.linterp.table = ptr;

  if (D != NULL)
    return gd_alter_entry(D->D, E.field, &E, move_table);
  
  return 0;
}
