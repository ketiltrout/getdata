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
#include "getdata/entry.h"
#include "getdata/dirfile.h"
#include "getdata/rawentry.h"
#include "getdata/lincomentry.h"
#include "getdata/linterpentry.h"
#include "getdata/bitentry.h"
#include "getdata/multiplyentry.h"
#include "getdata/phaseentry.h"
#include "getdata/constentry.h"
#include "getdata/stringentry.h"
#include "getdata/indexentry.h"

#include <stdlib.h>
#include <cstring>

using namespace GetData;

Entry::Entry()
{
  memset(&E, 0, sizeof(E));
  D = NULL;
}

Entry::Entry(GetData::Dirfile *dirfile, const char* field_code)
{
  D = dirfile;
  get_entry(D->D, field_code, &E);
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

int Entry::Move(int new_fragment, int move_data)
{
  int ret = -1;

  if (D != NULL)
    ret = dirfile_move(D->D, E.field, new_fragment, move_data);

  if (!ret)
    E.fragment_index = new_fragment;

  return ret;
}

int Entry::Rename(const char* new_name, int move_data)
{
  char* ptr;
  int ret = -1;

  if (D != NULL)
    ret = dirfile_rename(D->D, E.field, new_name, move_data);

  if (ret) {
    char* nn = (char*)malloc(strlen(E.field) + strlen(new_name));
    strcpy(nn, E.field);
    ptr = strchr(nn, '/');

    if (ptr) { /* metafield */
      strcpy(ptr + 1, new_name);
    } else {
      free(nn);
      nn = strdup(new_name);
    }

    free(E.field);
    E.field = nn;
  }

  return ret;
}

void Entry::SetDirfile(GetData::Dirfile* dirfile)
{
  D = dirfile;
}
