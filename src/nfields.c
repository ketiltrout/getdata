/* Copyright (C) 2008-2011 D. V. Wiebe
 *
 ***************************************************************************
 *
 * This file is part of the GetData project.
 *
 * GetData is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation; either version 2.1 of the License, or (at your
 * option) any later version.
 *
 * GetData is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with GetData; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
#include "internal.h"

unsigned int gd_nfields(DIRFILE* D) gd_nothrow
{
  dtrace("%p", D);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%u", 0);
    return 0;
  }

  _GD_ClearError(D);

  dreturn("%u", D->n_entries - D->n_hidden - D->n_meta);
  return D->n_entries - D->n_hidden - D->n_meta;
}

unsigned int gd_nvectors(DIRFILE* D) gd_nothrow
{
  dtrace("%p", D);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%u", 0);
    return 0;
  }

  _GD_ClearError(D);

  dreturn("%u", D->n_entries - D->n_hidden - D->n_meta
      - D->n[_GD_EntryIndex(GD_STRING_ENTRY)]
      - D->n[_GD_EntryIndex(GD_CONST_ENTRY)]
      - D->n[_GD_EntryIndex(GD_CARRAY_ENTRY)]);
  return D->n_entries - D->n_hidden - D->n_meta
    - D->n[_GD_EntryIndex(GD_STRING_ENTRY)]
    - D->n[_GD_EntryIndex(GD_CONST_ENTRY)]
    - D->n[_GD_EntryIndex(GD_CARRAY_ENTRY)];
}

unsigned int gd_nfields_by_type(DIRFILE* D, gd_entype_t type) gd_nothrow
{
  const int index = _GD_EntryIndex(type);

  dtrace("%p, %i", D, type);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%u", 0);
    return 0;
  }

  _GD_ClearError(D);

  if (index == -1) {
      _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_ENTRY_TYPE, NULL, type, NULL);
      dreturn("%u", 0);
      return 0;
  }

  dreturn("%u", D->n[index]);
  return D->n[index];
}
