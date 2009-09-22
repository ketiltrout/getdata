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
#endif

/* a zero length list */
static const char* zero_list[1] = { NULL };

/* correspondence between type_list index and gd_enttype_t */
const gd_entype_t _gd_entype_index[GD_N_ENTYPES] =
{
  GD_RAW_ENTRY, GD_LINCOM_ENTRY, GD_LINTERP_ENTRY, GD_BIT_ENTRY,
  GD_MULTIPLY_ENTRY, GD_PHASE_ENTRY, GD_INDEX_ENTRY, GD_POLYNOM_ENTRY,
  GD_SBIT_ENTRY, GD_CONST_ENTRY, GD_STRING_ENTRY
};

const void* get_constants(DIRFILE* D, gd_type_t return_type)
{
  dtrace("%p, 0x%x", D, return_type);

  unsigned int i, n;
  char* fl;

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_ClearError(D);

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
    if (D->entry[i]->field_type == GD_CONST_ENTRY &&
        D->entry[i]->e->n_meta != -1)
      if (_GD_DoField(D, D->entry[i], 0, 0, 0, return_type,
            fl + n++ * GD_SIZE(return_type)) != 1)
        break;
  }

  D->const_value_list = fl;

  dreturn("%p", D->const_value_list);
  return D->const_value_list;
}

const char** get_strings(DIRFILE* D)
{
  dtrace("%p", D);

  unsigned int i, n;
  char** fl;

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_ClearError(D);

  if (D->n_string == 0) {
    dreturn("%p", zero_list);
    return zero_list;
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
    if (D->entry[i]->field_type == GD_STRING_ENTRY &&
        D->entry[i]->e->n_meta != -1)
      fl[n++] = D->entry[i]->e->string;
  }
  fl[n] = NULL;

  D->string_value_list = (const char**)fl;
  D->list_validity |= LIST_VALID_STRING_VALUE;

  dreturn("%p", D->string_value_list);
  return D->string_value_list;
}

const char** get_field_list_by_type(DIRFILE* D, gd_entype_t type)
{
  dtrace("%p, %x", D, type);

  unsigned int i, n;
  char** fl;
  int index = -1;

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_ClearError(D);

  n = get_nfields_by_type(D, type);

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

  if (D->type_list_validity & (1 << index)) {
    /* list already made */
    dreturn("%p", D->type_list[index]);
    return D->type_list[index];
  }

  fl = realloc((char**)D->type_list[index], sizeof(const char*) * (n + 1));

  if (fl == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  for (i = n = 0; i < D->n_entries; ++i) {
    if (D->entry[i]->field_type == type && D->entry[i]->e->n_meta != -1)
      fl[n++] = D->entry[i]->field;
  }
  fl[n] = NULL;

  D->type_list[index] = (const char**)fl;
  D->type_list_validity |= 1 << index;

  dreturn("%p", D->type_list[index]);
  return D->type_list[index];
}

const char** get_vector_list(DIRFILE* D)
{
  dtrace("%p", D);

  unsigned int i, n;
  char** fl;

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_ClearError(D);

  n = D->n_entries - D->n_meta - D->n_string - D->n_const;

  if (n == 0) {
    dreturn("%p", zero_list);
    return zero_list;
  }

  if (D->list_validity & LIST_VALID_VECTOR) {
    /* list already made */
    dreturn("%p", D->vector_list);
    return D->vector_list;
  }

  fl = realloc((char**)D->vector_list, sizeof(const char*) * (n + 1));

  if (fl == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  for (i = n = 0; i < D->n_entries; ++i) {
    if (!(D->entry[i]->field_type & GD_SCALAR_ENTRY) &&
        D->entry[i]->e->n_meta != -1)
      fl[n++] = D->entry[i]->field;
  }
  fl[n] = NULL;

  D->vector_list = (const char**)fl;
  D->list_validity |= LIST_VALID_VECTOR;

  dreturn("%p", D->vector_list);
  return D->vector_list;
}

const char** get_field_list(DIRFILE* D)
{
  dtrace("%p", D);

  unsigned int i, n;
  char** fl;

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_ClearError(D);

  if (D->n_entries == 0) {
    dreturn("%p", zero_list);
    return zero_list;
  }

  if (D->list_validity & LIST_VALID_FIELD) {
    /* list already made */
    dreturn("%p (old)", D->field_list);
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
    if (D->entry[i]->e->n_meta != -1)
      fl[n++] = D->entry[i]->field;
  fl[n] = NULL;

  D->field_list = (const char**)fl;
  D->list_validity |= LIST_VALID_FIELD;

  dreturn("%p", D->field_list);
  return D->field_list;
}
