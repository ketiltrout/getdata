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
#include <string.h>
#endif

#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#endif

#include "internal.h"

/* add an entry */
int dirfile_add(DIRFILE* D, const gd_entry_t* entry)
{
  dtrace("%p, %p", D, entry);

  char temp_buffer[FILENAME_MAX];
  int i;

  _GD_ClearError(D);

  /* check access mode */
  if ((D->flags & GD_ACCMODE) == GD_RDONLY) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* check for duplicate field */
  gd_entry_t* E = _GD_FindField(D, entry->field); 

  if (E != NULL) { /* matched */
    _GD_SetError(D, GD_E_DUPLICATE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* check for bad field type */
  if (entry->field_type != GD_RAW_ENTRY &&
      entry->field_type != GD_LINCOM_ENTRY &&
      entry->field_type != GD_LINTERP_ENTRY &&
      entry->field_type != GD_BIT_ENTRY &&
      entry->field_type != GD_MULTIPLY_ENTRY &&
      entry->field_type != GD_MULTIPLY_ENTRY &&
      entry->field_type != GD_PHASE_ENTRY &&
      entry->field_type != GD_CONST_ENTRY &&
      entry->field_type != GD_STRING_ENTRY)
  {
    _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_TYPE, NULL,
        entry->field_type, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* check for include index out of range */
  if (entry->format_file < 0 || entry->format_file >= D->n_include) {
    _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_FORMAT, NULL,
        entry->format_file, NULL);
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

  E->e = malloc(sizeof(union _gd_private_entry));
  if (E->e == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    free(E);
    dreturn("%i", -1);
    return -1;
  }

  /* Validate field code */
  E->field_type = entry->field_type;
  E->field = _GD_ValidateField(entry->field);
  if (E->field == entry->field) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, NULL);
    E->field = NULL;
    dirfile_free_entry_strings(E);
    free(E);
    dreturn("%i", -1);
    return -1;
  } else if (E->field == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dirfile_free_entry_strings(E);
    free(E);
    dreturn("%i", -1);
    return -1;
  }

  /* Validate entry and add auxiliary data */
  switch(entry->field_type)
  {
    case GD_RAW_ENTRY:
      E->data_type = entry->data_type;
      E->e->fp = -1;
      E->e->stream = NULL;
      E->e->first = 0;
      
      if ((E->e->file = malloc(FILENAME_MAX)) == NULL) {
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
        break;
      }

      strcpy(temp_buffer, D->include_list[E->format_file].cname);
      snprintf(E->e->file, FILENAME_MAX, "%s/%s", dirname(temp_buffer),
          E->field);

      if ((E->spf = entry->spf) <= 0)
        _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_SPF, NULL, entry->spf,
            NULL);
      else if (E->data_type & 0x40 || (E->size = GD_SIZE(E->data_type)) == 0)
        _GD_SetError(D, GD_E_BAD_TYPE, 0, NULL, entry->data_type, NULL);
      else if (D->first_field == NULL) {
        E->e->first = 1; /* This is the first raw field? */
        D->first_field = malloc(sizeof(gd_entry_t));
        if (D->first_field == NULL) {
          _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
          break;
        }

        memcpy(D->first_field, E, sizeof(gd_entry_t));
        /* Tag the include list */
        for (i = E->format_file; i != -1; i = D->include_list[i].parent)
          D->include_list[i].first = D->include_list[i].modified = 1;
      }
      break;
    case GD_LINCOM_ENTRY:
      E->n_fields = entry->n_fields;
      memcpy(E->m, entry->m, sizeof(double) * E->n_fields);
      memcpy(E->b, entry->b, sizeof(double) * E->n_fields);

      if (E->n_fields < 1 || E->n_fields > GD_MAX_LINCOM)
        _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_NFIELDS, NULL,
            E->n_fields, NULL);
      else
        for (i = 0; i < E->n_fields; ++i)
          if ((E->in_fields[i] = strdup(entry->in_fields[i])) == NULL)
            _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      break;
    case GD_LINTERP_ENTRY:
      E->e->table_len = -1;
      E->e->x = E->e->y = NULL;

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
        _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_NUMBITS, NULL, 
            entry->numbits, NULL);
      else if (E->bitnum < 0)
        _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_BITNUM, NULL, 
            entry->bitnum, NULL);
      else if (E->bitnum + E->numbits - 1 > 63)
        _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_BITSIZE, NULL, 
            E->bitnum + E->numbits - 1, NULL);
      break;
    case GD_PHASE_ENTRY:
      E->shift = entry->shift;

      if ((E->in_fields[0] = strdup(entry->in_fields[0])) == NULL)
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      break;
    case GD_CONST_ENTRY:
      if (E->data_type & 0x40 || (E->size = GD_SIZE(E->data_type)) == 0)
        _GD_SetError(D, GD_E_BAD_TYPE, 0, NULL, entry->data_type, NULL);
      break;
    case GD_STRING_ENTRY:
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
  D->include_list[E->format_file].modified = 1;

  qsort(D->entry, D->n_entries, sizeof(gd_entry_t*), _GD_EntryCmp);

  dreturn("%i", 0);
  return 0;
}

/* add a RAW entry */
int dirfile_add_raw(DIRFILE* D, const char* field_code, gd_type_t data_type,
    unsigned int spf, int format_file)
{
  dtrace("%p, \"%s\", %i, %x %i", D, field_code, spf, data_type, format_file);

  gd_entry_t R;
  R.field = (char*)field_code;
  R.field_type = GD_RAW_ENTRY;
  R.spf = spf;
  R.data_type = data_type;
  R.format_file = format_file;
  int error = dirfile_add(D, &R);

  dreturn("%i", error);
  return error;
}

/* add a LINCOM entry -- this function is variadic */
int dirfile_add_lincom(DIRFILE* D, const char* field_code, int n_fields,
    const char** in_fields, const double* m, const double* b, int format_file)
{
  dtrace("%p, \"%s\", %i, %p, %p, %p, %i", D, field_code, n_fields, in_fields,
      m, b, format_file);

  int i;
  gd_entry_t L;
  L.field = (char*)field_code;
  L.field_type = GD_LINCOM_ENTRY;
  L.n_fields = n_fields;
  L.format_file = format_file;

  for (i = 0; i < n_fields; ++i) {
    L.in_fields[i] = (char*)in_fields[i];
    L.m[i] = m[i];
    L.b[i] = b[i];
  }
  int error = dirfile_add(D, &L);

  dreturn("%i", error);
  return error;
}

/* add a LINTERP entry */
int dirfile_add_linterp(DIRFILE* D, const char* field_code,
    const char* in_field, const char* table, int format_file)
{
  dtrace("%p, \"%s\", \"%s\", \"%s\", %i", D, field_code, in_field, table,
      format_file);

  gd_entry_t L;
  L.field = (char*)field_code;
  L.field_type = GD_LINTERP_ENTRY;
  L.in_fields[0] = (char*)in_field;
  L.table = (char*)table;
  L.format_file = format_file;
  int error = dirfile_add(D, &L);

  dreturn("%i", error);
  return error;
}

/* add a BIT entry */
int dirfile_add_bit(DIRFILE* D, const char* field_code, const char* in_field,
    int bitnum, int numbits, int format_file)
{
  dtrace("%p, \"%s\", \"%s\", %i, %i, %i\n", D, field_code, in_field, bitnum,
      numbits, format_file);

  gd_entry_t B;
  B.field = (char*)field_code;
  B.field_type = GD_BIT_ENTRY;
  B.in_fields[0] = (char*)in_field;
  B.bitnum = bitnum;
  B.numbits = numbits;
  B.format_file = format_file;
  int error = dirfile_add(D, &B);

  dreturn("%i", error);
  return error;
}

/* add a MULTIPLY entry */
int dirfile_add_multiply(DIRFILE* D, const char* field_code,
    const char* in_field1, const char* in_field2, int format_file)
{
  dtrace("%p, \"%s\", \"%s\", \"%s\", %i", D, field_code, in_field1, in_field2,
      format_file);

  gd_entry_t M;
  M.field = (char*)field_code;
  M.field_type = GD_MULTIPLY_ENTRY;
  M.in_fields[0] = (char*)in_field1;
  M.in_fields[1] = (char*)in_field2;
  M.format_file = format_file;
  int error = dirfile_add(D, &M);

  dreturn("%i", error);
  return error;
}

/* add a PHASE entry */
int dirfile_add_phase(DIRFILE* D, const char* field_code, const char* in_field,
    int shift, int format_file)
{
  dtrace("%p, \"%s\", \"%s\", %i, %i", D, field_code, in_field, shift,
      format_file);

  gd_entry_t P;
  P.field = (char*)field_code;
  P.field_type = GD_PHASE_ENTRY;
  P.in_fields[0] = (char*)in_field;
  P.shift = shift;
  P.format_file = format_file;
  int error = dirfile_add(D, &P);

  dreturn("%i", error);
  return error;
}
