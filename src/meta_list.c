/* (C) 2008-2009 D. V. Wiebe
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

#ifdef STDC_HEADERS
#include <stdlib.h>
#include <string.h>
#endif

/* a zero length list */
static const char* zero_list[1] = { NULL };

const void* get_mconstants(DIRFILE* D, const char* parent,
    gd_type_t return_type)
{
  dtrace("%p, \"%s\", 0x%x", D, parent, return_type);

  int i, n;
  char* fl;

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_ClearError(D);

  const gd_entry_t* P = _GD_FindField(D, parent, D->entry, D->n_entries, NULL);

  if (P == NULL || P->e->n_meta == -1) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, parent);
    dreturn("%p", NULL);
    return NULL;
  }

  struct _gd_private_entry* e = P->e;

  if (e->n_meta_const == 0) {
    dreturn("%p", NULL);
    return NULL;
  }

  free(e->const_value_list);
  fl = _GD_Alloc(D, return_type, e->n_meta_const);

  if (fl == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  /* DoField will implicitly choose GD_REPR_AUTO for complex data being returned
   * as purely real */
  for (i = n = 0; i < e->n_meta; ++i) {
    if (e->meta_entry[i]->field_type == GD_CONST_ENTRY)
      if (_GD_DoField(D, e->meta_entry[i], 0, 0, 0, return_type,
            fl + n++ * GD_SIZE(return_type)) != 1)
        break;
  }

  e->const_value_list = fl;

  dreturn("%p", e->const_value_list);
  return e->const_value_list;
}

const char** get_mstrings(DIRFILE* D, const char* parent)
{
  dtrace("%p, \"%s\"", D, parent);

  int i, n;
  char** fl;

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_ClearError(D);

  const gd_entry_t* P = _GD_FindField(D, parent, D->entry, D->n_entries, NULL);

  if (P == NULL || P->e->n_meta == -1) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, parent);
    dreturn("%p", NULL);
    return NULL;
  }

  struct _gd_private_entry* e = P->e;

  if (e->n_meta_string == 0) {
    dreturn("%p", zero_list);
    return zero_list;
  }

  fl = realloc((char**)e->string_value_list, sizeof(const char*) *
      (e->n_meta_string + 1));

  if (fl == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  for (i = n = 0; i < e->n_meta; ++i) {
    if (e->meta_entry[i]->field_type == GD_STRING_ENTRY)
      fl[n++] = e->meta_entry[i]->e->string;
  }
  fl[n] = NULL;

  e->string_value_list = (const char**)fl;

  dreturn("%p", e->string_value_list);
  return e->string_value_list;
}

const char** get_mfield_list_by_type(DIRFILE* D, const char* parent,
    gd_entype_t type)
{
  dtrace("%p, \"%s\", %x", D, parent, type);

  int i, index = -1;
  unsigned int n;
  char** fl;

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_ClearError(D);

  const gd_entry_t* P = _GD_FindField(D, parent, D->entry, D->n_entries, NULL);

  if (P == NULL || P->e->n_meta == -1) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, parent);
    dreturn("%p", NULL);
    return NULL;
  }

  struct _gd_private_entry* e = P->e;
  size_t offs = strlen(P->field) + 1;

  n = get_nmfields_by_type(D, parent, type);

  if (D->error) {
    dreturn("%p", NULL);
    return NULL;
  }

  if (n == 0) {
    dreturn("%p", zero_list);
    return zero_list;
  }

  /* find the index -- get_nfields_by_type should have already tripped up
   * if the type is invalid */
  for (i = 0; i < GD_N_ENTYPES; ++i)
    if (_gd_entype_index[i] == type) {
      index = i;
      break;
    }

  if (index == -1) {
    _GD_InternalError(D);
    dreturn("%p", NULL);
    return NULL;
  }

  fl = realloc(e->type_list[index], sizeof(const char*) * (n + 1));

  if (fl == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  for (i = n = 0; i < e->n_meta; ++i) {
    if (e->meta_entry[i]->field_type == type)
      fl[n++] = e->meta_entry[i]->field + offs;
  }
  fl[n] = NULL;

  e->type_list[index] = fl;

  dreturn("%p", e->type_list[index]);
  return (const char**)e->type_list[index];
}

const char** get_mvector_list(DIRFILE* D, const char* parent)
{
  dtrace("%p, \"%s\"", D, parent);

  int i, n;
  char** fl;

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_ClearError(D);

  const gd_entry_t* P = _GD_FindField(D, parent, D->entry, D->n_entries, NULL);

  if (P == NULL) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, parent);
    dreturn("%p", NULL);
    return NULL;
  }

  struct _gd_private_entry* e = P->e;

  size_t offs = strlen(P->field) + 1;

  n = e->n_meta - e->n_meta_string - e->n_meta_const;

  if (n == 0) {
    dreturn("%p", zero_list);
    return zero_list;
  }

  fl = realloc((char**)e->vector_list, sizeof(const char*) * (n + 1));

  if (fl == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  for (i = n = 0; i < e->n_meta; ++i) {
    if (!(e->meta_entry[i]->field_type & GD_SCALAR_ENTRY))
      fl[n++] = e->meta_entry[i]->field + offs;
  }
  fl[n] = NULL;

  e->vector_list = (const char**)fl;

  dreturn("%p", e->vector_list);
  return e->vector_list;
}

const char** get_mfield_list(DIRFILE* D, const char* parent)
{
  dtrace("%p, \"%s\"", D, parent);

  int i, n;
  char** fl;

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_ClearError(D);

  const gd_entry_t* P = _GD_FindField(D, parent, D->entry, D->n_entries, NULL);

  if (P == NULL) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, parent);
    dreturn("%p", NULL);
    return NULL;
  }

  struct _gd_private_entry* e = P->e;

  size_t offs = strlen(P->field) + 1;

  if (e->n_meta == 0) {
    dreturn("%p", zero_list);
    return zero_list;
  }

  fl = realloc((char**)e->field_list, sizeof(const char*) *
      (e->n_meta + 1));

  if (fl == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  for (i = n = 0; i < e->n_meta; ++i)
    fl[n++] = e->meta_entry[i]->field + offs;
  fl[n] = NULL;

  e->field_list = (const char**)fl;

  dreturn("%p", e->field_list);
  return e->field_list;
}
