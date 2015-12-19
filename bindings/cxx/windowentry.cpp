// Copyright (C) 2011, 2012, 2015 D. V. Wiebe
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

WindowEntry::WindowEntry(const char* field_code, const char* in_field,
    const char* check, WindOpType windop, gd_triplet_t threshold,
    int fragment_index) : Entry()
{
  dtrace("\"%s\", \"%s\", \"%s\", %i, {%g,%" PRIX64 ",%" PRId64 "}, %i",
      field_code, in_field, check, (unsigned)windop, threshold.r, threshold.u,
      threshold.i, fragment_index);

  E.field = strdup(field_code);
  E.field_type = GD_WINDOW_ENTRY;
  E.in_fields[0] = strdup(in_field);
  E.in_fields[1] = strdup(check);
  E.scalar[0] = 0;
  E.u.window.windop = (gd_windop_t)windop;
  E.u.window.threshold = threshold;
  E.fragment_index = fragment_index;

  dreturnvoid();
}

int WindowEntry::SetInput(const char* field, int index)
{
  if (index < 0 || index > 1)
    return -1;

  char* ptr = strdup(field);

  if (ptr == NULL)
    return -1;

  free(E.in_fields[index]);
  E.in_fields[index] = ptr;

  if (D != NULL)
    return gd_alter_entry(D->D, E.field, &E, 0);
  
  return 0;
}

int WindowEntry::SetWindOp(WindOpType windop)
{
  int ret = 0;

  dtrace("0x%X", (unsigned)windop);

  E.u.window.windop = (gd_windop_t)windop;

  if (D != NULL)
    ret = gd_alter_entry(D->D, E.field, &E, 0);
  
  dreturn("%i", ret);
  return ret;
}

int WindowEntry::SetThreshold(gd_triplet_t threshold)
{
  int ret = 0;

  dtrace("{%g,%" PRIX64 ",%" PRId64 "}", threshold.r, threshold.u, threshold.i);

  E.u.window.threshold = threshold;

  if (D != NULL)
    ret = gd_alter_entry(D->D, E.field, &E, 0);
  
  dreturn("%i", ret);
  return ret;
}

int WindowEntry::SetThreshold(const char *threshold)
{
  int r = 0;

  dtrace("\"%s\"", threshold);

  SetScalar(0, threshold);

  if (D != NULL) {
    r = gd_alter_entry(D->D, E.field, &E, 0);

    if (!r) {
      switch(E.u.window.windop) {
        case GD_WINDOP_EQ:
        case GD_WINDOP_NE:
          r = gd_get_constant(D->D, threshold, GD_INT64,
              &E.u.window.threshold.i);
          break;
        case GD_WINDOP_SET:
        case GD_WINDOP_CLR:
          r = gd_get_constant(D->D, threshold, GD_UINT64,
              &E.u.window.threshold.u);
          break;
        default:
          r = gd_get_constant(D->D, threshold, GD_FLOAT64,
              &E.u.window.threshold.r);
          break;
      }
    }
  }
  
  dreturn("%i", r);
  return r;
}
