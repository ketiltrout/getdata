// Copyright (C) 2008-2013 D. V. Wiebe
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

RawEntry::RawEntry(const char* field_code, DataType data_type, unsigned int spf,
      int fragment_index) : Entry()
{
  E.field = strdup(field_code);
  E.field_type = GD_RAW_ENTRY;
  E.u.raw.spf = spf;
  E.u.raw.data_type = (gd_type_t)data_type;
  E.fragment_index = fragment_index;
  filename = NULL;
}

RawEntry::~RawEntry()
{
  free(filename);
}

int RawEntry::SetSamplesPerFrame(unsigned int spf, int recode)
{
  E.u.raw.spf = spf;

  if (D != NULL)
    return gd_alter_entry(D->D, E.field, &E, recode);
  
  return 0;
}

int RawEntry::SetSamplesPerFrame(const char *spf, int recode)
{
  int r = 0;

  SetScalar(0, spf);

  if (D != NULL) {
    r = gd_alter_entry(D->D, E.field, &E, recode);

    if (!r)
      r = gd_get_constant(D->D, spf, GD_UINT16, &E.u.raw.spf);
  }
  
  return r;
}

int RawEntry::SetType(DataType type, int recode)
{
  E.u.raw.data_type = (gd_type_t)type;

  if (D != NULL)
    return gd_alter_entry(D->D, E.field, &E, recode);
  
  return 0;
}

const char* RawEntry::FileName()
{
  free(filename);
  filename = gd_raw_filename(D->D, E.field);
  return filename;
}
