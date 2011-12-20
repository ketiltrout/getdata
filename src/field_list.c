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

/* zero length lists */
static const char* zero_list[1] = { NULL };
static const gd_carray_t zero_carrays[1] = { {0, NULL} };

const void *gd_constants(DIRFILE* D, gd_type_t return_type) gd_nothrow
{
  unsigned int i, n;
  char* fl;

  dtrace("%p, 0x%x", D, return_type);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_ClearError(D);

  if ((n = D->n[_GD_EntryIndex(GD_CONST_ENTRY)]) == 0) {
    dreturn("%p", NULL);
    return NULL;
  }

  fl = (char *)_GD_Alloc(D, return_type, n);

  if (fl == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  for (i = n = 0; i < D->n_entries; ++i) {
    if (D->entry[i]->field_type == GD_CONST_ENTRY &&
        D->entry[i]->e->n_meta != -1 && !D->entry[i]->hidden)
      if (_GD_DoField(D, D->entry[i], 0, 0, 1, return_type,
            fl + n++ * GD_SIZE(return_type)) != 1)
        break;
  }

  free(D->const_value_list);
  D->const_value_list = fl;

  dreturn("%p", D->error ? NULL : D->const_value_list);
  return D->error ? NULL : D->const_value_list;
}

const gd_carray_t *gd_carrays(DIRFILE* D, gd_type_t return_type) gd_nothrow
{
  unsigned int i, n;
  gd_carray_t* fl;

  dtrace("%p, 0x%x", D, return_type);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_ClearError(D);

  if ((n = D->n[_GD_EntryIndex(GD_CARRAY_ENTRY)]) == 0) {
    dreturn("%p", zero_carrays);
    return zero_carrays;
  }

  fl = (gd_carray_t *)_GD_Malloc(D, sizeof(gd_carray_t) * (n + 1));

  if (fl == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  memset(fl, 0, sizeof(gd_carray_t) * (n + 1));

  for (i = n = 0; i < D->n_entries; ++i) {
    if (D->entry[i]->field_type == GD_CARRAY_ENTRY &&
        D->entry[i]->e->n_meta != -1 && !D->entry[i]->hidden)
    {
      fl[n].n = D->entry[i]->EN(scalar,array_len);
      fl[n].d = _GD_Alloc(D, return_type, fl[n].n);
      if (D->error || _GD_DoField(D, D->entry[i], 0, 0, fl[n].n, return_type,
            fl[n].d) != 1)
        break;
      n++;
    }
  }
  fl[n].n = 0;

  if (D->carray_value_list)
    for (i = 0; D->carray_value_list[i].n != 0; ++i)
      free(D->carray_value_list[i].d);
  free(D->carray_value_list);
  D->carray_value_list = fl;

  dreturn("%p", D->error ? NULL : fl);
  return D->error ? NULL : fl;
}

const char **gd_strings(DIRFILE* D) gd_nothrow
{
  unsigned int i, n;
  char** fl;

  dtrace("%p", D);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_ClearError(D);

  if ((n = D->n[_GD_EntryIndex(GD_STRING_ENTRY)]) == 0) {
    dreturn("%p", zero_list);
    return zero_list;
  }

  if (D->list_validity & GD_LIST_VALID_STRING_VALUE) {
    /* list already made */
    dreturn("%p", D->string_value_list);
    return D->string_value_list;
  }

  fl = (char **)_GD_Malloc(D, sizeof(const char*) * (n + 1));

  if (fl == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  for (i = n = 0; i < D->n_entries; ++i) {
    if (D->entry[i]->field_type == GD_STRING_ENTRY &&
        D->entry[i]->e->n_meta != -1 && !D->entry[i]->hidden)
      fl[n++] = D->entry[i]->e->u.string;
  }
  fl[n] = NULL;

  free(D->string_value_list);
  D->string_value_list = (const char **)fl;
  D->list_validity |= GD_LIST_VALID_STRING_VALUE;

  dreturn("%p", D->string_value_list);
  return D->string_value_list;
}

const char **gd_field_list_by_type(DIRFILE* D, gd_entype_t type) gd_nothrow
{
  unsigned int i, n;
  char** fl;
  const int index = _GD_EntryIndex(type);

  dtrace("%p, %x", D, type);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_ClearError(D);

  n = gd_nfields_by_type(D, type);

  if (D->error) {
    dreturn("%p", NULL);
    return NULL;
  }

  if (n == 0) {
    dreturn("%p", zero_list);
    return zero_list;
  }

  if (D->type_list_validity & (1 << index)) {
    /* list already made */
    dreturn("%p", D->type_list[index]);
    return D->type_list[index];
  }

  fl = (char **)_GD_Malloc(D, sizeof(const char*) * (n + 1));

  if (fl == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  for (i = n = 0; i < D->n_entries; ++i) {
    if (D->entry[i]->field_type == type && D->entry[i]->e->n_meta != -1 &&
        !D->entry[i]->hidden)
    {
      fl[n++] = D->entry[i]->field;
    }
  }
  fl[n] = NULL;

  free(D->type_list[index]);
  D->type_list[index] = (const char **)fl;
  D->type_list_validity |= 1 << index;

  dreturn("%p", D->type_list[index]);
  return D->type_list[index];
}

const char **gd_vector_list(DIRFILE* D) gd_nothrow
{
  unsigned int i, n;
  char **fl;

  dtrace("%p", D);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_ClearError(D);

  n = gd_nvectors(D);

  if (n == 0) {
    dreturn("%p", zero_list);
    return zero_list;
  }

  if (D->list_validity & GD_LIST_VALID_VECTOR) {
    /* list already made */
    dreturn("%p", D->vector_list);
    return D->vector_list;
  }

  fl = (char **)_GD_Malloc(D, sizeof(const char*) * (n + 1));

  if (fl == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  for (i = n = 0; i < D->n_entries; ++i) {
    if (!(D->entry[i]->field_type & GD_SCALAR_ENTRY) &&
        D->entry[i]->e->n_meta != -1 && !D->entry[i]->hidden)
      fl[n++] = D->entry[i]->field;
  }
  fl[n] = NULL;

  free(D->vector_list);
  D->vector_list = (const char **)fl;
  D->list_validity |= GD_LIST_VALID_VECTOR;

  dreturn("%p", D->vector_list);
  return D->vector_list;
}

const char **gd_field_list(DIRFILE* D) gd_nothrow
{
  unsigned int i, n;
  char **fl;

  dtrace("%p", D);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_ClearError(D);

  if (D->n_entries - D->n_hidden - D->n_meta == 0) {
    dreturn("%p", zero_list);
    return zero_list;
  }

  if (D->list_validity & GD_LIST_VALID_FIELD) {
    /* list already made */
    dreturn("%p (old)", D->field_list);
    return D->field_list;
  }

  fl = (char **)_GD_Malloc(D, sizeof(const char*) *
      (D->n_entries + 1 - D->n_meta - D->n_hidden));

  if (fl == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  for (i = n = 0; i < D->n_entries; ++i)
    if (D->entry[i]->e->n_meta != -1 && !D->entry[i]->hidden)
      fl[n++] = D->entry[i]->field;
  fl[n] = NULL;

  free(D->field_list);
  D->field_list = (const char **)fl;
  D->list_validity |= GD_LIST_VALID_FIELD;

  dreturn("%p", D->field_list);
  return D->field_list;
}
