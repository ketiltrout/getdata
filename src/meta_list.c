/* Copyright (C) 2008-2010 D. V. Wiebe
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
static const char *zero_list[1] = { NULL };
static const gd_carray_t zero_carrays[1] = { {0, NULL} };

const void *gd_mconstants(DIRFILE* D, const char* parent,
    gd_type_t return_type) gd_nothrow
{
  int i, n;
  char* fl;
  gd_entry_t *P;
  struct _gd_private_entry *e;

  dtrace("%p, \"%s\", 0x%x", D, parent, return_type);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_ClearError(D);

  P = _GD_FindField(D, parent, D->entry, D->n_entries, NULL);

  if (P == NULL || P->e->n_meta == -1) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, parent);
    dreturn("%p", NULL);
    return NULL;
  }

  e = P->e;

  if (e->n_meta_const == 0) {
    dreturn("%p", NULL);
    return NULL;
  }

  free(e->const_value_list);
  fl = (char *)_GD_Alloc(D, return_type, e->n_meta_const);

  if (fl == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  /* DoField will implicitly choose GD_REPR_AUTO for complex data being returned
   * as purely real */
  for (i = n = 0; i < e->n_meta; ++i) {
    if (e->p.meta_entry[i]->field_type == GD_CONST_ENTRY)
      if (_GD_DoField(D, e->p.meta_entry[i], 0, 0, 1, return_type,
            fl + n++ * GD_SIZE(return_type)) != 1)
        break;
  }

  e->const_value_list = fl;

  dreturn("%p", e->const_value_list);
  return e->const_value_list;
}

const gd_carray_t *gd_mcarrays(DIRFILE* D, const char* parent,
    gd_type_t return_type) gd_nothrow
{
  int i, n;
  gd_carray_t *fl;
  gd_entry_t *P;
  struct _gd_private_entry *e;

  dtrace("%p, \"%s\", 0x%x", D, parent, return_type);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_ClearError(D);

  P = _GD_FindField(D, parent, D->entry, D->n_entries, NULL);

  if (P == NULL || P->e->n_meta == -1) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, parent);
    dreturn("%p", NULL);
    return NULL;
  }

  e = P->e;

  if (e->n_meta_carray == 0) {
    dreturn("%p", zero_carrays);
    return zero_carrays;
  }

  if (e->carray_value_list)
    for (i = 0; e->carray_value_list[i].n != 0; ++i)
      free(e->carray_value_list[i].d);
  free(e->carray_value_list);

  fl = (gd_carray_t *)malloc(sizeof(gd_carray_t) * (e->n_meta_carray + 1));
  dwatch("%i", e->n_meta_carray);

  if (fl == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  /* DoField will implicitly choose GD_REPR_AUTO for complex data being returned
   * as purely real */
  for (i = n = 0; i < e->n_meta; ++i) {
    if (e->p.meta_entry[i]->field_type == GD_CARRAY_ENTRY) {
      fl[n].n = e->p.meta_entry[i]->EN(scalar,array_len);
      fl[n].d = _GD_Alloc(D, return_type, fl[n].n);
      if (D->error || _GD_DoField(D, e->p.meta_entry[i], 0, 0, fl[n].n,
            return_type, fl[n].d) != 1)
        break;
      n++;
    }
  }
  fl[n].n = 0;

  e->carray_value_list = fl;

  dreturn("%p", D->error ? NULL : fl);
  return D->error ? NULL : fl;
}

const char **gd_mstrings(DIRFILE* D, const char* parent) gd_nothrow
{
  int i, n;
  char** fl;
  gd_entry_t *P;
  struct _gd_private_entry *e;

  dtrace("%p, \"%s\"", D, parent);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_ClearError(D);

  P = _GD_FindField(D, parent, D->entry, D->n_entries, NULL);

  if (P == NULL || P->e->n_meta == -1) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, parent);
    dreturn("%p", NULL);
    return NULL;
  }

  e = P->e;

  if (e->n_meta_string == 0) {
    dreturn("%p", zero_list);
    return zero_list;
  }

  fl = (char **)realloc((char **)e->string_value_list, sizeof(const char*) *
      (e->n_meta_string + 1));

  if (fl == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  for (i = n = 0; i < e->n_meta; ++i) {
    if (e->p.meta_entry[i]->field_type == GD_STRING_ENTRY)
      fl[n++] = e->p.meta_entry[i]->e->u.string;
  }
  fl[n] = NULL;

  e->string_value_list = (const char **)fl;

  dreturn("%p", e->string_value_list);
  return e->string_value_list;
}

const char **gd_mfield_list_by_type(DIRFILE* D, const char* parent,
    gd_entype_t type) gd_nothrow
{
  int i, index = -1;
  unsigned int n;
  char **fl;
  gd_entry_t *P;
  struct _gd_private_entry *e;
  size_t offs;

  dtrace("%p, \"%s\", %x", D, parent, type);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_ClearError(D);

  P = _GD_FindField(D, parent, D->entry, D->n_entries, NULL);

  if (P == NULL || P->e->n_meta == -1) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, parent);
    dreturn("%p", NULL);
    return NULL;
  }

  e = P->e;
  offs = strlen(P->field) + 1;

  n = gd_nmfields_by_type(D, parent, type);

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

  fl = (char **)realloc(e->type_list[index], sizeof(const char*) * (n + 1));

  if (fl == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  for (i = n = 0; i < e->n_meta; ++i) {
    if (e->p.meta_entry[i]->field_type == type)
      fl[n++] = e->p.meta_entry[i]->field + offs;
  }
  fl[n] = NULL;

  e->type_list[index] = fl;

  dreturn("%p", e->type_list[index]);
  return (const char **)e->type_list[index];
}

const char **gd_mvector_list(DIRFILE* D, const char* parent) gd_nothrow
{
  int i, n;
  char **fl;
  struct _gd_private_entry* e;
  size_t offs;
  gd_entry_t *P;

  dtrace("%p, \"%s\"", D, parent);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_ClearError(D);

  P = _GD_FindField(D, parent, D->entry, D->n_entries, NULL);

  if (P == NULL) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, parent);
    dreturn("%p", NULL);
    return NULL;
  }

  e = P->e;
  offs = strlen(P->field) + 1;

  n = e->n_meta - e->n_meta_string - e->n_meta_const - e->n_meta_carray;

  if (n == 0) {
    dreturn("%p", zero_list);
    return zero_list;
  }

  fl = (char **)realloc((char **)e->vector_list, sizeof(const char*) * (n + 1));

  if (fl == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  for (i = n = 0; i < e->n_meta; ++i) {
    if (!(e->p.meta_entry[i]->field_type & GD_SCALAR_ENTRY))
      fl[n++] = e->p.meta_entry[i]->field + offs;
  }
  fl[n] = NULL;

  e->vector_list = (const char **)fl;

  dreturn("%p", e->vector_list);
  return e->vector_list;
}

const char **gd_mfield_list(DIRFILE* D, const char* parent) gd_nothrow
{
  int i, n;
  char** fl;
  struct _gd_private_entry *e;
  gd_entry_t *P;
  size_t offs;

  dtrace("%p, \"%s\"", D, parent);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_ClearError(D);

  P = _GD_FindField(D, parent, D->entry, D->n_entries, NULL);

  if (P == NULL) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, parent);
    dreturn("%p", NULL);
    return NULL;
  }

  e = P->e;

  offs = strlen(P->field) + 1;

  if (e->n_meta == 0) {
    dreturn("%p", zero_list);
    return zero_list;
  }

  fl = (char **)realloc((char **)e->field_list, sizeof(const char*) *
      (e->n_meta + 1));

  if (fl == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  for (i = n = 0; i < e->n_meta; ++i)
    fl[n++] = e->p.meta_entry[i]->field + offs;
  fl[n] = NULL;

  e->field_list = (const char **)fl;

  dreturn("%p", e->field_list);
  return e->field_list;
}
