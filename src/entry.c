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
#include <string.h>
#include <stdlib.h>
#endif

void _GD_FreeE(gd_entry_t* entry, int priv)
{
  int i;

  dtrace("%p, %i", entry, priv);

  if (!entry || entry->field_type == GD_NO_ENTRY) {
    dreturnvoid();
    return;
  }

  free(entry->field);

  switch(entry->field_type) {
    case GD_LINCOM_ENTRY:
      for (i = 0; i < entry->n_fields; ++i)
        free(entry->in_fields[i]);
      break;
    case GD_LINTERP_ENTRY:
      free(entry->in_fields[0]);
      free(entry->table);
      break;
    case GD_MULTIPLY_ENTRY:
      free(entry->in_fields[1]);
      /* fall through */
    case GD_BIT_ENTRY:
    case GD_PHASE_ENTRY:
      free(entry->in_fields[0]);
      break;
    case GD_STRING_ENTRY:
      if (priv)
        free(entry->e->string);
      break;
    case GD_CONST_ENTRY:
      if (priv)
        free(entry->e->client);
      break;
    case GD_RAW_ENTRY:
      if (priv) {
        free(entry->e->file[0].name);
        free(entry->e->file[1].name);
      }
      break;
    case GD_INDEX_ENTRY:
    case GD_NO_ENTRY:
      break;
  }

  if (priv) {
    for (i = 0; i < GD_MAX_LINCOM * 2; ++i)
      free(entry->e->scalar[i]);
    free(entry->e->field_list);
    free(entry->e->vector_list);
    free(entry->e->string_value_list);
    for (i = 0; i < GD_N_ENTYPES; ++i)
      free(entry->e->type_list[i]);
    free(entry->e->const_value_list);
    free(entry->e);
    free(entry);
  }

  dreturnvoid();
  return;
}

gd_entry_t* dirfile_free_entry_strings(gd_entry_t* entry)
{
  dtrace("%p", entry);

  _GD_FreeE(entry, 0);

  dreturn("%p", entry);
  return entry;
}

static void _GD_GetScalar(DIRFILE* D, gd_entry_t* E, const char* scalar,
    int type, void* data)
{
  int32_t i32;
  uint32_t u32;
  gd_entry_t* C;

  dtrace("%p, %p, \"%s\", %i, %p", D, E, scalar, type, data);

  if (scalar != NULL) {
    if ((C = _GD_FindField(D, scalar, NULL)) == NULL)
      _GD_SetError(D, GD_E_BAD_SCALAR, GD_E_SCALAR_CODE, E->field, 0,
          E->e->scalar[0]);
    else if (C->field_type != GD_CONST_ENTRY)
      _GD_SetError(D, GD_E_BAD_SCALAR, GD_E_SCALAR_TYPE, E->field, 0,
          E->e->scalar[0]);
    else {
      if (type == GD_IEEE754)
        _GD_DoConst(D, C, GD_FLOAT64, data);
      else if (type == GD_SIGNED) {
        _GD_DoConst(D, C, GD_INT32, &i32);
        *(int*)data = (unsigned int)i32;
      } else {
        _GD_DoConst(D, C, GD_UINT32, &u32);
        *(unsigned int*)data = (unsigned int)u32;
      }

      if ((D->flags & GD_ACCMODE) == GD_RDWR) {
        C->e->client = realloc(C->e->client, (C->e->n_client + 1) *
            sizeof(gd_entry_t*));
        C->e->client[C->e->n_client++] = E;
      }
    }
  }

  dreturnvoid();
}

int _GD_CalculateEntry(DIRFILE* D, gd_entry_t* E)
{
  int i;

  dtrace("%p, %p", D, E);

  switch(E->field_type) {
    case GD_RAW_ENTRY:
      _GD_GetScalar(D, E, E->e->scalar[0], 0, &E->spf);
      break;
    case GD_LINCOM_ENTRY:
      for (i = 0; i < E->n_fields; ++i) {
        _GD_GetScalar(D, E, E->e->scalar[i * 2], GD_IEEE754, &E->m[i]);
        _GD_GetScalar(D, E, E->e->scalar[i * 2 + 1], GD_IEEE754, &E->b[i]);

        if (D->error)
          break;
      }
      break;
    case GD_BIT_ENTRY:
      _GD_GetScalar(D, E, E->e->scalar[0], GD_SIGNED, &E->bitnum);
      _GD_GetScalar(D, E, E->e->scalar[1], GD_SIGNED, &E->numbits);
      break;
    case GD_PHASE_ENTRY:
      _GD_GetScalar(D, E, E->e->scalar[0], GD_SIGNED, &E->shift);
      break;
    case GD_NO_ENTRY:
    case GD_LINTERP_ENTRY:
    case GD_MULTIPLY_ENTRY:
    case GD_STRING_ENTRY:
    case GD_CONST_ENTRY:
    case GD_INDEX_ENTRY:
      break;
  }

  if (!D->error)
    E->e->calculated = 1;

  dreturn("%i", E->e->calculated);
  return E->e->calculated;
}

const char* get_raw_filename(DIRFILE* D, const char* field_code)
{
  dtrace("%p, \"%s\"", D, field_code);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  /* Check field */
  gd_entry_t *E = _GD_FindField(D, field_code, NULL);

  if (E == NULL) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
    dreturn("%p", NULL);
    return NULL;
  }

  if (E->field_type != GD_RAW_ENTRY) {
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
    dreturn("%p", NULL);
    return NULL;
  }

  if (E->e->file[0].name == NULL) {
    /* ensure encoding sybtype is known */
    if (!_GD_Supports(D, E, 0)) {
      dreturn("%p", NULL);
      return NULL;
    }

    if (E->e->file[0].encoding == GD_ENC_UNKNOWN) {
      _GD_SetError(D, GD_E_UNKNOWN_ENCODING, 0, NULL, 0, NULL);
      dreturn("%p", NULL);
      return NULL;
    }

    E->e->file[0].name = malloc(FILENAME_MAX);

    if (E->e->file[0].name == NULL) {
      _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      dreturn("%p", NULL);
      return NULL;
    }

    snprintf(E->e->file[0].name, FILENAME_MAX, "%s%s", E->e->filebase,
        ef[E->e->file[0].encoding].ext);
  }

  dreturn("%p", E->e->file[0].name);
  return E->e->file[0].name;
}

int get_entry(DIRFILE* D, const char* field_code, gd_entry_t* entry)
{
  int i;
  gd_entry_t *E;

  dtrace("%p, \"%s\", %p", D, field_code, entry);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", 1);
    return -1;
  }

  _GD_ClearError(D);

  E = _GD_FindField(D, field_code, NULL);

  if (E == NULL) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
    dreturn("%i", -1);
    return -1;
  }

  /* Calculate the entry, if necessary */
  if (!E->e->calculated)
    _GD_CalculateEntry(D, E);

  if (D->error) {
    dreturn("%i", -1);
    return -1;
  }

  /* now copy to the user supplied buffer */
  memcpy(entry, E, sizeof(gd_entry_t));
  entry->e = NULL;

  /* duplicate strings */
  entry->field = strdup(E->field);

  switch(E->field_type) {
    case GD_LINCOM_ENTRY:
      for (i = 0; i < E->n_fields; ++i)
        entry->in_fields[i] = strdup(E->in_fields[i]);
      break;
    case GD_LINTERP_ENTRY:
      entry->in_fields[0] = strdup(E->in_fields[0]);
      entry->table = strdup(E->table);
      break;
    case GD_MULTIPLY_ENTRY:
      entry->in_fields[1] = strdup(E->in_fields[1]);
      /* fall through */
    case GD_BIT_ENTRY:
    case GD_PHASE_ENTRY:
      entry->in_fields[0] = strdup(E->in_fields[0]);
      /* fall through */
    case GD_RAW_ENTRY:
    case GD_INDEX_ENTRY:
    case GD_CONST_ENTRY:
    case GD_STRING_ENTRY:
    case GD_NO_ENTRY:
      break;
  }

  dreturn("%i", 0);
  return 0;
}
