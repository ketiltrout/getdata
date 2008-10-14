/* (C) 2008 D. V. Wiebe
 *
 ***************************************************************************
 *
 * This file is part of the GetData project.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * GetData is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with GetData; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
#include "internal.h"

unsigned int get_nmeta_fields(DIRFILE* D, const char* parent)
{
  dtrace("%p", D);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%u", 0);
    return 0;
  }

  _GD_ClearError(D);

  const gd_entry_t* P = _GD_FindField(D, parent, NULL);

  if (P == NULL) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, parent);
    dreturn("%u", 0);
    return 0;
  }

  _GD_ClearError(D);

  dreturn("%u", P->e->n_meta);
  return P->e->n_meta;
}

unsigned int get_nmeta_vectors(DIRFILE* D, const char* parent)
{
  dtrace("%p", D);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%u", 0);
    return 0;
  }

  _GD_ClearError(D);

  const gd_entry_t* P = _GD_FindField(D, parent, NULL);

  if (P == NULL) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, parent);
    dreturn("%u", 0);
    return 0;
  }

  _GD_ClearError(D);

  dreturn("%u", P->e->n_meta - P->e->n_meta_string - P->e->n_meta_const);
  return P->e->n_meta - P->e->n_meta_string - P->e->n_meta_const;
}

unsigned int get_nmeta_fields_by_type(DIRFILE* D, const char* parent,
    gd_entype_t type)
{
  unsigned int r = 0;
  int i;

  dtrace("%p, %i", D, type);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%u", 0);
    return 0;
  }

  _GD_ClearError(D);

  const gd_entry_t* P = _GD_FindField(D, parent, NULL);

  if (P == NULL) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, parent);
    dreturn("%u", 0);
    return 0;
  }

  _GD_ClearError(D);
  if (type == GD_STRING_ENTRY)
    r = P->e->n_meta_string;
  else if (type == GD_CONST_ENTRY)
    r = P->e->n_meta_const;
  else
    for (i = 0; i < P->e->n_meta; ++i)
      if (P->e->meta_entry[i]->field_type == type)
        r++;

  dreturn("%u", r);
  return r;
}
