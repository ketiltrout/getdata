// (C) 2008 D. V. Wiebe
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
#include "getdata/rawentry.h"
#include "getdata/entry.h"
#include "getdata/dirfile.h"

#include <cstring>

using namespace GetData;

RawEntry::RawEntry(const char* field_code, DataType data_type, unsigned int spf,
      int fragment_index) : Entry::Entry()
{
  E.field = strdup(field_code);
  E.field_type = GD_RAW_ENTRY;
  E.spf = spf;
  E.data_type = (gd_type_t)data_type;
  E.fragment_index = fragment_index;
}

int RawEntry::SetSamplesPerFrame(unsigned int spf, int recode)
{
  E.spf = spf;

  if (D != NULL)
    return dirfile_alter_entry(D->D, E.field, &E, recode);
  
  return 0;
}

int RawEntry::SetType(DataType type, int recode)
{
  E.data_type = (gd_type_t)type;

  if (D != NULL)
    return dirfile_alter_entry(D->D, E.field, &E, recode);
  
  return 0;
}

const char* RawEntry::FileName()
{
  return get_raw_filename(D->D, E.field);
}
