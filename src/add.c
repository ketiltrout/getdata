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
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef STDC_HEADERS
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#endif

#include "internal.h"

/* add an entry */
int dirfile_add(DIRFILE* D, const gd_entry_t* entry)
{
  dtrace("%p, %p", D, entry);

  int i;

  _GD_ClearError(D);

  /* check for duplicate field */
  gd_entry_t* E = _GD_FindField(D, entry->field); 

  if (E != NULL) { /* matched */
    _GD_SetError(D, GD_E_DUPLICATE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* check for bad field type */
  if (entry->field_type == GD_NO_ENTRY) {
    _GD_SetError(D, GD_E_BAD_ENTRY, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* check for include index out of range */
  if (entry->format_file < 0 || entry->format_file >= D->n_include) {
    _GD_SetError(D, GD_E_BAD_ENTRY, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* New entry */
  E = malloc(sizeof(gd_entry_t));
  if (E == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }
  memset(E, 0, sizeof(gd_entry_t));
  E->format_file = entry->format_file;

  /* Validate field code */
  E->field_type = entry->field_type;
  E->field = _GD_ValidateField(entry->field);
  if (E->field == entry->field) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, NULL);
    free(E);
    dreturn("%i", -1);
    return -1;
  } else if (E->field == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    free(E);
    dreturn("%i", -1);
    return -1;
  }

  /* Validate entry and add auxiliary data */
  switch(entry->field_type)
  {
    case GD_RAW_ENTRY:
      E->data_type = entry->data_type;
      
      if ((E->file = malloc(FILENAME_MAX)) == NULL) {
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
        break;
      }

      snprintf(E->file, FILENAME_MAX, "./%s", E->field);

      if ((E->spf = entry->spf) < 0)
        _GD_SetError(D, GD_E_BAD_ENTRY, 0, NULL, 0, NULL);
      else if ((E->size = GD_SIZE(E->data_type)) == 0)
        _GD_SetError(D, GD_E_BAD_ENTRY, 0, NULL, 0, NULL);
      break;
    case GD_LINCOM_ENTRY:
      E->n_fields = entry->n_fields;
      memcpy(E->m, entry->m, sizeof(double) * E->n_fields);
      memcpy(E->b, entry->b, sizeof(double) * E->n_fields);

      if (E->n_fields < 1 || E->n_fields > GD_MAX_LINCOM)
        _GD_SetError(D, GD_E_BAD_ENTRY, 0, NULL, 0, NULL);
      else
        for (i = 0; i < E->n_fields; ++i)
          if ((E->in_fields[i] = strdup(entry->in_fields[i])) == NULL)
            _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      break;
    case GD_LINTERP_ENTRY:
      E->table_len = -1;

      if ((E->in_fields[0] = strdup(entry->in_fields[0])) == NULL)
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      else if ((E->table = strdup(entry->table)) == NULL)
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      break;
    case GD_MULTIPLY_ENTRY:
      if ((E->in_fields[0] = strdup(entry->in_fields[0])) == NULL)
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      else if ((E->in_fields[1] = strdup(entry->in_fields[1])) == NULL)
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      break;
    case GD_BIT_ENTRY:
      E->numbits = entry->numbits;
      E->bitnum = entry->bitnum;

      if ((E->in_fields[0] = strdup(entry->in_fields[0])) == NULL)
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      else if (E->numbits < 1)
        _GD_SetError(D, GD_E_BAD_ENTRY, 0, NULL, 0, NULL);
      else if (E->bitnum < 0)
        _GD_SetError(D, GD_E_BAD_ENTRY, 0, NULL, 0, NULL);
      else if (E->bitnum + E->numbits - 1 > 63)
        _GD_SetError(D, GD_E_BAD_ENTRY, 0, NULL, 0, NULL);
      break;
    case GD_PHASE_ENTRY:
      E->shift = entry->shift;

      if ((E->in_fields[0] = strdup(entry->in_fields[0])) == NULL)
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      break;
    case GD_NO_ENTRY:
      _GD_InternalError(D); /* We've already verrified field_type is valid */
      break;
  }

  if (D->error != GD_E_OK) {
    dirfile_free_entry_strings(E);
    free(E);
    dreturn("%i", -1);
    return -1;
  }

  /* add the entry and resort the entry list */
  D->entry = realloc(D->entry, (D->n_entries + 1) * sizeof(gd_entry_t**));
  D->entry[D->n_entries++] = E;

  qsort(D->entry, D->n_entries, sizeof(gd_entry_t*), _GD_EntryCmp);

  dreturn("%i", 0);
  return 0;
}

/* add a RAW entry */
int dirfile_add_raw(DIRFILE* D, const char* field_code, unsigned int spf,
    gd_type_t data_type)
{
  dtrace("%p, \"%s\", %i, %x", D, field_code, spf, data_type);

  gd_entry_t R;
  R.field = (char*)field_code;
  R.field_type = GD_RAW_ENTRY;
  R.spf = spf;
  R.data_type = data_type;
  int error = dirfile_add(D, &R);

  dreturn("%i", error);
  return error;
}

/* add a LINCOM entry -- this function is variadic */
int dirfile_add_lincom(DIRFILE* D, const char* field_code, int n_fields, ...)
{
  dtrace("%p, \"%s\", %i, ...", D, field_code, n_fields);

  int i;
  va_list va;
  gd_entry_t L;
  L.field = (char*)field_code;
  L.field_type = GD_LINCOM_ENTRY;
  L.n_fields = n_fields;

  if (n_fields > 0) {
    va_start(va, n_fields);

    L.in_fields[0] = va_arg(va, char*);
    L.m[0] = va_arg(va, double);
    L.b[0] = va_arg(va, double);

    for (i = 1; i < GD_MAX_LINCOM; ++i)
      if (n_fields > i) {
        L.in_fields[i] = va_arg(va, char*);
        L.m[i] = va_arg(va, double);
        L.b[i] = va_arg(va, double);
      }

    va_end(va);
  }
  int error = dirfile_add(D, &L);

  dreturn("%i", error);
  return error;
}

/* add a LINTERP entry */
int dirfile_add_linterp(DIRFILE* D, const char* field_code,
    const char* in_field, const char* table)
{
  dtrace("%p, \"%s\", \"%s\", \"%s\"", D, field_code, in_field, table);

  gd_entry_t L;
  L.field = (char*)field_code;
  L.field_type = GD_LINTERP_ENTRY;
  L.in_fields[0] = (char*)in_field;
  L.table = (char*)table;
  int error = dirfile_add(D, &L);

  dreturn("%i", error);
  return error;
}

/* add a BIT entry */
int dirfile_add_bit(DIRFILE* D, const char* field_code, const char* in_field,
    int bitnum, int numbits)
{
  dtrace("%p, \"%s\", \"%s\", %i, %i\n", D, field_code, in_field, bitnum,
      numbits);

  gd_entry_t B;
  B.field = (char*)field_code;
  B.field_type = GD_BIT_ENTRY;
  B.in_fields[0] = (char*)in_field;
  B.bitnum = bitnum;
  B.numbits = numbits;
  int error = dirfile_add(D, &B);

  dreturn("%i", error);
  return error;
}

/* add a MULTIPLY entry */
int dirfile_add_multiply(DIRFILE* D, const char* field_code,
    const char* in_field1, const char* in_field2)
{
  dtrace("%p, \"%s\", \"%s\", \"%s\"", D, field_code, in_field1, in_field2);

  gd_entry_t M;
  M.field = (char*)field_code;
  M.field_type = GD_MULTIPLY_ENTRY;
  M.in_fields[0] = (char*)in_field1;
  M.in_fields[1] = (char*)in_field2;
  int error = dirfile_add(D, &M);

  dreturn("%i", error);
  return error;
}

/* add a PHASE entry */
int dirfile_add_phase(DIRFILE* D, const char* field_code, const char* in_field,
    int shift)
{
  dtrace("%p, \"%s\", \"%s\", %i", D, field_code, in_field, shift);

  gd_entry_t P;
  P.field = (char*)field_code;
  P.field_type = GD_PHASE_ENTRY;
  P.in_fields[0] = (char*)in_field;
  P.shift = shift;
  int error = dirfile_add(D, &P);

  dreturn("%i", error);
  return error;
}
