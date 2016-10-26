// Copyright (C) 2008-2014 D. V. Wiebe
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

Entry::Entry()
{
  memset(&E, 0, sizeof(E));
  D = NULL;
}

Entry::Entry(const GetData::Dirfile *dirfile, const char* field_code)
{
  D = dirfile;
  if (gd_entry(D->D, field_code, &E))
    memset(&E, 0, sizeof(E));
}

Entry::~Entry()
{
  gd_free_entry_strings(&E);
}

int Entry::CheckIndex(gd_entype_t field_type, int n_fields, int index)
{
  if (index < 0)
    return 0;

  switch (field_type) {
    case GD_RAW_ENTRY:
    case GD_INDEX_ENTRY:
    case GD_CONST_ENTRY:
    case GD_CARRAY_ENTRY:
    case GD_SARRAY_ENTRY:
    case GD_STRING_ENTRY:
    case GD_NO_ENTRY:
    case GD_ALIAS_ENTRY:
      return 0;
    case GD_LINCOM_ENTRY:
      if (index > n_fields)
        return 0;
      break;
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
    case GD_INDIR_ENTRY:
    case GD_SINDIR_ENTRY:
    case GD_WINDOW_ENTRY:
    case GD_MPLEX_ENTRY:
      if (index > 2)
        return 0;
    case GD_LINTERP_ENTRY:
    case GD_BIT_ENTRY:
    case GD_PHASE_ENTRY:
    case GD_POLYNOM_ENTRY:
    case GD_SBIT_ENTRY:
    case GD_RECIP_ENTRY:
      if (index > 1)
        return 0;
  }

  return 1;
}

int Entry::Move(int new_fragment, unsigned flags)
{
  int ret = -1;

  if (D != NULL)
    ret = gd_move(D->D, E.field, new_fragment, flags);

  if (!ret)
    E.fragment_index = new_fragment;

  return ret;
}

int Entry::Rename(const char* new_name, unsigned flags)
{
  char* ptr;
  int ret = -1;

  if (D != NULL)
    ret = gd_rename(D->D, E.field, new_name, flags);

  if (ret) {
    if (E.field == NULL) {
      E.field = strdup(new_name);
    } else {
      /* this buffer is used if E is a metafield, in which case we'll
       * replace the subfield name in E.field with new_name.  The length
       * of the new code is
       *
       *   strlen(E.field) - strlen(<subfield-name>) + strlen(new_name)
       *     + 1 (for the trailing NUL).
       *
       * The subfield name in E.field must be at least one character long, so
       * the length of the new code is at most:
       *
       *   strlen(E.field) - 1 + strlen(new_name) + 1
       *   = strlen(E.field) + strlen(new_name)
       */
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
  }

  return ret;
}

void Entry::SetDirfile(const GetData::Dirfile* dirfile)
{
  D = dirfile;
}

void Entry::SetName(const char* name)
{
  this->Rename(name);
}

void Entry::SetFragmentIndex(int fragment_index)
{
  this->Move(fragment_index);
}

static inline int scalar_ok(const gd_entry_t &E, int index)
{
  if (index < 0)
    return 0;

  switch (E.field_type) {
    case GD_LINCOM_ENTRY:
      if (index >= GD_MAX_LINCOM + E.u.lincom.n_fields ||
          (index >= E.u.lincom.n_fields && index < GD_MAX_LINCOM))
      {
        return 0;
      }
      break;
    case GD_POLYNOM_ENTRY:
      if (index > E.u.polynom.poly_ord)
        return 0;
      break;
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
    case GD_MPLEX_ENTRY:
      if (index >= 2)
        return 0;
      break;
    case GD_RAW_ENTRY:
    case GD_PHASE_ENTRY:
    case GD_RECIP_ENTRY:
    case GD_WINDOW_ENTRY:
      if (index >= 1)
        return 0;
      break;
    case GD_LINTERP_ENTRY:
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
    case GD_INDIR_ENTRY:
    case GD_SINDIR_ENTRY:
    case GD_INDEX_ENTRY:
    case GD_CONST_ENTRY:
    case GD_CARRAY_ENTRY:
    case GD_SARRAY_ENTRY:
    case GD_STRING_ENTRY:
    case GD_NO_ENTRY:
    case GD_ALIAS_ENTRY:
      return 0;
  }

  return 1;
}

const char *Entry::Scalar(int index) const
{
  return scalar_ok(E, index) ? E.scalar[index] : NULL;
}

int Entry::ScalarIndex(int index) const
{
  return scalar_ok(E, index) ? E.scalar_ind[index] : 0;
}

void Entry::SetScalar(int n, const char *code)
{
  free(E.scalar[n]);
  if (code == NULL)
    E.scalar[n] = NULL;
  else {
    E.scalar[n] = strdup(code);
    char *ptr = strchr(E.scalar[n], '<');
    if (ptr) {
      *ptr = '\0';
      E.scalar_ind[n] = atoi(ptr + 1);
    } else
      E.scalar_ind[n] = -1;
  }
}
