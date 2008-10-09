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

#ifdef STDC_HEADERS
#include <stdlib.h>
#endif

const void* get_meta_constant_values(DIRFILE* D, const char* parent,
    gd_type_t return_type)
{
  dtrace("%p, \"%s\", 0x%x", D, parent, return_type);

  unsigned int i, n;
  void* fl;

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  if (D->n_const == 0) {
    dreturn("%p", NULL);
    return NULL;
  }

  free(D->const_value_list);
  fl = _GD_Alloc(D, return_type, D->n_const);

  if (fl == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  for (i = n = 0; i < D->n_entries; ++i) {
    if (D->entry[i]->field_type == GD_CONST_ENTRY)
      if (_GD_DoField(D, D->entry[i], D->entry[i]->field, 0, 0, 0, 0,
            return_type, fl + n++ * GD_SIZE(return_type)) != 1)
        break;
  }

  D->const_value_list = fl;
  D->list_validity |= LIST_VALID_CONST;

  dreturn("%p", D->const_value_list);
  return D->const_value_list;
}

const char** get_meta_constant_list(DIRFILE* D, const char* parent)
{
  dtrace("%p, \"%s\"", D, parent);

  unsigned int i, n;
  char** fl;

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  if (D->n_const == 0) {
    dreturn("%p", NULL);
    return NULL;
  }

  if (D->list_validity & LIST_VALID_CONST) {
    /* list already made */
    dreturn("%p", D->const_list);
    return D->const_list;
  }

  fl = realloc((char**)D->const_list, sizeof(const char*) * (D->n_const + 1));

  if (fl == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  for (i = n = 0; i < D->n_entries; ++i) {
    if (D->entry[i]->field_type == GD_CONST_ENTRY)
      fl[n++] = D->entry[i]->field;
  }
  fl[n] = NULL;

  D->const_list = (const char**)fl;
  D->list_validity |= LIST_VALID_CONST;

  dreturn("%p", D->const_list);
  return D->const_list;
}

const char** get_meta_string_values(DIRFILE* D, const char* parent)
{
  dtrace("%p, \"%s\"", D, parent);

  unsigned int i, n;
  char** fl;

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  if (D->n_string == 0) {
    dreturn("%p", NULL);
    return NULL;
  }

  if (D->list_validity & LIST_VALID_STRING_VALUE) {
    /* list already made */
    dreturn("%p", D->string_value_list);
    return D->string_value_list;
  }

  fl = realloc((char**)D->string_value_list, sizeof(const char*) *
      (D->n_string + 1));

  if (fl == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  for (i = n = 0; i < D->n_entries; ++i) {
    if (D->entry[i]->field_type == GD_STRING_ENTRY)
      fl[n++] = D->entry[i]->e->string;
  }
  fl[i] = NULL;

  D->string_value_list = (const char**)fl;
  D->list_validity |= LIST_VALID_STRING_VALUE;

  dreturn("%p", D->string_value_list);
  return D->string_value_list;
}

const char** get_meta_string_list(DIRFILE* D, const char* parent)
{
  dtrace("%p, \"%s\"", D, parent);

  unsigned int i, n;
  char** fl;

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  if (D->n_string == 0) {
    dreturn("%p", NULL);
    return NULL;
  }

  if (D->list_validity & LIST_VALID_STRING) {
    /* list already made */
    dreturn("%p", D->string_list);
    return D->string_list;
  }

  fl = realloc((char**)D->string_list, sizeof(const char*) * (D->n_string + 1));

  if (fl == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  for (i = n = 0; i < D->n_entries; ++i) {
    if (D->entry[i]->field_type == GD_STRING_ENTRY)
      fl[n++] = D->entry[i]->field;
  }
  fl[n] = NULL;

  D->string_list = (const char**)fl;
  D->list_validity |= LIST_VALID_STRING;

  dreturn("%p", D->string_list);
  return D->string_list;
}

const char** get_meta_vector_list(DIRFILE* D, const char* parent)
{
  dtrace("%p, \"%s\"", D, parent);

  unsigned int i, n;
  char** fl;

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  if (D->n_entries == 0) {
    dreturn("%p", NULL);
    return NULL;
  }

  if (D->list_validity & LIST_VALID_VECTOR) {
    /* list already made */
    dreturn("%p", D->vector_list);
    return D->vector_list;
  }

  fl = realloc((char**)D->vector_list, sizeof(const char*) *
      (D->n_entries + 1 - D->n_string - D->n_const - D->n_meta));

  if (fl == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  for (i = n = 0; i < D->n_entries; ++i) {
    if (D->entry[i]->field_type != GD_CONST_ENTRY && 
        D->entry[i]->field_type != GD_STRING_ENTRY)
      fl[n++] = D->entry[i]->field;
  }
  fl[n] = NULL;

  D->vector_list = (const char**)fl;
  D->list_validity |= LIST_VALID_VECTOR;

  dreturn("%p", D->vector_list);
  return D->vector_list;
}

const char** get_meta_field_list(DIRFILE* D, const char* parent)
{
  dtrace("%p, \"%s\"", D, parent);

  unsigned int i, n;
  char** fl;

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  if (D->n_entries == 0) {
    dreturn("%p", NULL);
    return NULL;
  }

  if (D->list_validity & LIST_VALID_FIELD) {
    /* list already made */
    dreturn("%p", D->field_list);
    return D->field_list;
  }

  fl = realloc((char**)D->field_list, sizeof(const char*) *
      (D->n_entries + 1 - D->n_meta));

  if (fl == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  for (i = n = 0; i < D->n_entries; ++i)
    if (!D->entry[i]->e->n_meta)
      fl[n++] = D->entry[i]->field;
  fl[n] = NULL;

  D->field_list = (const char**)fl;
  D->list_validity |= LIST_VALID_FIELD;

  dreturn("%p", D->field_list);
  return D->field_list;
}
