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
// Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
//
#include "entry.h"

#include <string>

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
