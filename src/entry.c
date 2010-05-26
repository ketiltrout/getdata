/* (C) 2008-2010 D. V. Wiebe
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
      for (i = 0; i < entry->n_fields; ++i) {
        free(entry->in_fields[i]);
        free(entry->scalar[i]);
        free(entry->scalar[i + GD_MAX_LINCOM]);
      }
      break;
    case GD_LINTERP_ENTRY:
      free(entry->in_fields[0]);
      free(entry->table);
      if (priv)
        free(entry->e->table_path);
      break;
    case GD_MULTIPLY_ENTRY:
      free(entry->in_fields[1]);
      free(entry->in_fields[0]);
      break;
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
      free(entry->scalar[1]);
      /* fallthrough */
    case GD_PHASE_ENTRY:
      free(entry->scalar[0]);
      free(entry->in_fields[0]);
      break;
    case GD_POLYNOM_ENTRY:
      free(entry->in_fields[0]);
      for (i = 0; i <= entry->poly_ord; ++i)
        free(entry->scalar[i]);
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
      free(entry->scalar[0]);
      if (priv) {
        free(entry->e->filebase);
        free(entry->e->file[0].name);
        free(entry->e->file[1].name);
      }
      break;
    case GD_INDEX_ENTRY:
    case GD_NO_ENTRY:
      break;
  }

  if (priv) {
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

gd_entry_t* gd_free_entry_strings(gd_entry_t* entry)
{
  dtrace("%p", entry);

  _GD_FreeE(entry, 0);

  dreturn("%p", entry);
  return entry;
}

static void _GD_GetScalar(DIRFILE* D, gd_entry_t* E, const char* scalar,
    int type, void* data)
{
  void *ptr = NULL;
  gd_entry_t* C;
  int repr;
  char* field_code;

  dtrace("%p, %p, \"%s\", %i, %p", D, E, scalar, type, data);

  if (scalar != NULL) {
    C = _GD_FindFieldAndRepr(D, scalar, &field_code, &repr, NULL, 0);

    if (D->error) {
      dreturnvoid();
      return;
    }

    if (C == NULL)
      _GD_SetError(D, GD_E_BAD_SCALAR, GD_E_SCALAR_CODE, E->field, 0,
          field_code);
    else if (C->field_type != GD_CONST_ENTRY)
      _GD_SetError(D, GD_E_BAD_SCALAR, GD_E_SCALAR_TYPE, E->field, 0,
          field_code);
    else {
      if ((D->flags & GD_ACCMODE) == GD_RDWR) {
        ptr = realloc(C->e->client, (C->e->n_client + 1) *
            sizeof(gd_entry_t*));
        if (ptr == NULL)
          _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      }

      _GD_DoField(D, C, repr, 0, 1, type, data);

      if (ptr) {
        C->e->client = ptr;
        C->e->client[C->e->n_client++] = E;
      }
    }

    if (field_code != scalar)
      free(field_code);
  }

  dreturnvoid();
}

/* resolve non-literal scalars */
int _GD_CalculateEntry(DIRFILE* D, gd_entry_t* E)
{
  int i;

  dtrace("%p, %p", D, E);

  switch(E->field_type) {
    case GD_RAW_ENTRY:
      _GD_GetScalar(D, E, E->scalar[0], GD_UINT16, &E->spf);
      break;
    case GD_POLYNOM_ENTRY:
      E->comp_scal = 0;
      for (i = 0; i <= E->poly_ord; ++i) {
        _GD_GetScalar(D, E, E->scalar[i], GD_COMPLEX128, &E->ca[i]);
        E->a[i] = creal(E->ca[i]);

        if (cimag(E->ca[i]))
          E->comp_scal = 1;

        if (D->error)
          break;
      }
      break;
    case GD_LINCOM_ENTRY:
      E->comp_scal = 0;
      for (i = 0; i < E->n_fields; ++i) {
        _GD_GetScalar(D, E, E->scalar[i], GD_COMPLEX128, &E->cm[i]);
        E->m[i] = creal(E->cm[i]);

        if (cimag(E->cm[i]))
          E->comp_scal = 1;

        _GD_GetScalar(D, E, E->scalar[i + GD_MAX_LINCOM], GD_COMPLEX128,
            &E->cb[i]);
        E->b[i] = creal(E->cb[i]);

        if (cimag(E->cb[i]))
          E->comp_scal = 1;

        if (D->error)
          break;
      }
      break;
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
      _GD_GetScalar(D, E, E->scalar[0], GD_INT16, &E->bitnum);
      _GD_GetScalar(D, E, E->scalar[1], GD_INT16, &E->numbits);
      break;
    case GD_PHASE_ENTRY:
      _GD_GetScalar(D, E, E->scalar[0], GD_INT64, &E->shift);
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

const char* gd_get_raw_filename(DIRFILE* D, const char* field_code_in)
{
  int repr;
  char* field_code;

  dtrace("%p, \"%s\"", D, field_code_in);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  /* Check field */
  gd_entry_t *E = _GD_FindFieldAndRepr(D, field_code_in, &field_code, &repr,
      NULL, 1);

  if (D->error) {
    dreturn("%p", NULL);
    return NULL;
  }

  if (E->field_type != GD_RAW_ENTRY) {
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
    dreturn("%p", NULL);
    return NULL;
  }

  if (field_code != field_code_in)
    free(field_code);

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
    } else if (_GD_SetEncodedName(D, E->e->file, E->e->filebase, 0)) {
      dreturn("%p", NULL);
      return NULL;
    }
  }

  dreturn("%p", E->e->file[0].name);
  return E->e->file[0].name;
}

int gd_get_entry(DIRFILE* D, const char* field_code_in, gd_entry_t* entry)
{
  int i, repr;
  gd_entry_t *E;
  char* field_code;

  dtrace("%p, \"%s\", %p", D, field_code_in, entry);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", 1);
    return -1;
  }

  _GD_ClearError(D);

  /* get rid of the represenation, if any */
  E = _GD_FindFieldAndRepr(D, field_code_in, &field_code, &repr, NULL, 1);

  if (D->error) {
    dreturn("%i", -1);
    return -1;
  }

  if (field_code != field_code_in)
    free(field_code);

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
      for (i = 0; i < E->n_fields; ++i) {
        entry->in_fields[i] = strdup(E->in_fields[i]);
        if (E->scalar[i])
          entry->scalar[i] = strdup(E->scalar[i]);
        if (E->scalar[i + GD_MAX_LINCOM])
          entry->scalar[i + GD_MAX_LINCOM] = strdup(E->scalar[i +
              GD_MAX_LINCOM]);
      }
      break;
    case GD_LINTERP_ENTRY:
      entry->in_fields[0] = strdup(E->in_fields[0]);
      entry->table = strdup(E->table);
      break;
    case GD_POLYNOM_ENTRY:
      entry->in_fields[0] = strdup(E->in_fields[0]);
      for (i = 0; i <= E->poly_ord; ++i)
        if (E->scalar[i])
          entry->scalar[i] = strdup(E->scalar[i]);
      break;
    case GD_MULTIPLY_ENTRY:
      entry->in_fields[0] = strdup(E->in_fields[0]);
      entry->in_fields[1] = strdup(E->in_fields[1]);
      break;
    case GD_PHASE_ENTRY:
      entry->in_fields[0] = strdup(E->in_fields[0]);
      if (E->scalar[0])
        entry->scalar[0] = strdup(E->scalar[0]);
      break;
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
      entry->in_fields[0] = strdup(E->in_fields[0]);
      if (E->scalar[0])
        entry->scalar[0] = strdup(E->scalar[0]);
      if (E->scalar[1])
        entry->scalar[1] = strdup(E->scalar[1]);
      break;
    case GD_RAW_ENTRY:
      if (E->scalar[0])
        entry->scalar[0] = strdup(E->scalar[0]);
      break;
    case GD_INDEX_ENTRY:
    case GD_CONST_ENTRY:
    case GD_STRING_ENTRY:
    case GD_NO_ENTRY:
      break;
  }

  dreturn("%i", 0);
  return 0;
}

gd_entype_t gd_get_entry_type(DIRFILE* D, const char* field_code_in)
{
  gd_entry_t* E;
  char* field_code;
  int repr;

  dtrace("%p, \"%s\"", D, field_code_in);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", GD_NO_ENTRY);
    return GD_NO_ENTRY;
  }

  _GD_ClearError(D);

  /* get rid of the represenation, if any */
  E = _GD_FindFieldAndRepr(D, field_code_in, &field_code, &repr, NULL, 1);

  if (D->error) {
    dreturn("%i", GD_NO_ENTRY);
    return GD_NO_ENTRY;
  }

  if (field_code != field_code_in)
    free(field_code);

  dreturn("%i", E->field_type);
  return E->field_type;
}

int gd_get_fragment_index(DIRFILE* D, const char* field_code_in)
{
  gd_entry_t* E;
  char* field_code;
  int repr;

  dtrace("%p, \"%s\"", D, field_code_in);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ClearError(D);

  /* get rid of the represenation, if any */
  E = _GD_FindFieldAndRepr(D, field_code_in, &field_code, &repr, NULL, 1);

  if (D->error) {
    dreturn("%i", -1);
    return -1;
  }

  if (field_code != field_code_in)
    free(field_code);

  dreturn("%i", E->fragment_index);
  return E->fragment_index;
}

int gd_validate(DIRFILE *D, const char *field_code_in)
{
  int i, repr;
  gd_entry_t* E;
  char *field_code;

  dtrace("%p, \"%s\"", D, field_code_in);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ClearError(D);

  /* get rid of the representation, if any */
  E = _GD_FindFieldAndRepr(D, field_code_in, &field_code, &repr, NULL, 1);

  if (D->error) {
    dreturn("%i", -1);
    return -1;
  }

  if (field_code != field_code_in)
    free(field_code);

  /* calculate scalars */
  if (!E->e->calculated)
    _GD_CalculateEntry(D, E);

  /* check input fields */
  switch (E->field_type) {
    case GD_LINCOM_ENTRY:
      for (i = 0; i < E->n_fields; ++i)
        _GD_BadInput(D, E, i);
      break;
    case GD_MULTIPLY_ENTRY:
      _GD_BadInput(D, E, 1);
    case GD_LINTERP_ENTRY:
    case GD_BIT_ENTRY:
    case GD_PHASE_ENTRY:
    case GD_POLYNOM_ENTRY:
    case GD_SBIT_ENTRY:
      _GD_BadInput(D, E, 0);
      break;
    case GD_RAW_ENTRY:
    case GD_CONST_ENTRY:
    case GD_STRING_ENTRY:
    case GD_INDEX_ENTRY:
    case GD_NO_ENTRY:
      break;
  }

  if (D->error) {
    dreturn("%i", -1);
    return -1;
  }

  dreturn("%i", 0);
  return 0;
}
