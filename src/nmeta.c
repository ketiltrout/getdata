/* Copyright (C) 2008, 2010 D. V. Wiebe
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

unsigned int gd_nmfields(DIRFILE* D, const char* parent) gd_nothrow
{
  gd_entry_t *P;

  dtrace("%p", D);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%u", 0);
    return 0;
  }

  _GD_ClearError(D);

  P = _GD_FindField(D, parent, D->entry, D->n_entries, NULL);

  if (P == NULL || P->e->n_meta == -1) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, parent);
    dreturn("%u", 0);
    return 0;
  }

  _GD_ClearError(D);

  dreturn("%u", P->e->n_meta);
  return P->e->n_meta;
}

unsigned int gd_nmvectors(DIRFILE* D, const char* parent) gd_nothrow
{
  gd_entry_t *P;

  dtrace("%p", D);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%u", 0);
    return 0;
  }

  _GD_ClearError(D);

  P = _GD_FindField(D, parent, D->entry, D->n_entries, NULL);

  if (P == NULL || P->e->n_meta == -1) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, parent);
    dreturn("%u", 0);
    return 0;
  }

  _GD_ClearError(D);

  dreturn("%u", P->e->n_meta - P->e->n_meta_string - P->e->n_meta_const -
      P->e->n_meta_carray);
  return P->e->n_meta - P->e->n_meta_string - P->e->n_meta_const -
    P->e->n_meta_carray;
}

unsigned int gd_nmfields_by_type(DIRFILE* D, const char* parent,
    gd_entype_t type) gd_nothrow
{
  unsigned int r = 0;
  int i;
  gd_entry_t *P;

  dtrace("%p, %i", D, type);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%u", 0);
    return 0;
  }

  _GD_ClearError(D);

  P = _GD_FindField(D, parent, D->entry, D->n_entries, NULL);

  if (P == NULL || P->e->n_meta == -1) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, parent);
    dreturn("%u", 0);
    return 0;
  }

  _GD_ClearError(D);
  if (type == GD_STRING_ENTRY)
    r = P->e->n_meta_string;
  else if (type == GD_CONST_ENTRY)
    r = P->e->n_meta_const;
  else if (type == GD_CARRAY_ENTRY)
    r = P->e->n_meta_carray;
  else
    for (i = 0; i < P->e->n_meta; ++i)
      if (P->e->p.meta_entry[i]->field_type == type)
        r++;

  dreturn("%u", r);
  return r;
}
