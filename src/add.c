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
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#endif

#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#endif

#include "internal.h"

/* add an entry */
static int _GD_Add(DIRFILE* D, const gd_entry_t* entry, const char* parent)
{
  dtrace("%p, %p", D, entry);

  char temp_buffer[FILENAME_MAX];
  int i;
  gd_entry_t* E;
  gd_entry_t* P = NULL;

  _GD_ClearError(D);

  /* check access mode */
  if ((D->flags & GD_ACCMODE) == GD_RDONLY) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* check parent */
  if (parent != NULL) {
    /* make sure it's not a meta field already */
    if (strchr(parent, '/') != NULL) {
      _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, parent);
      dreturn("%i", -1);
      return -1;
    }

    P = _GD_GetEntry(D, parent);
    if (D->error) {
      dreturn("%i", -1);
      return -1;
    }

    snprintf(temp_buffer, FILENAME_MAX, "%s/%s", parent, entry->field);
  } else
    snprintf(temp_buffer, FILENAME_MAX, "%s", entry->field);

  /* check for duplicate field */
  E = _GD_GetEntry(D, temp_buffer);

  if (D->error == GD_E_OK) { /* matched */
    _GD_SetError(D, GD_E_DUPLICATE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ClearError(D);

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
  if (entry->format_file >= D->n_include &&
      (entry->format_file != GD_FORMAT_AUTO || P == NULL))
  {
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
  if (entry->format_file == GD_FORMAT_AUTO)
    E->format_file = P->format_file;
  else
    E->format_file = entry->format_file;

  E->e = malloc(sizeof(struct _gd_private_entry));
  if (E->e == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    free(E);
    dreturn("%i", -1);
    return -1;
  }

  /* Validate field code */
  E->field_type = entry->field_type;
  if (parent == NULL)
    temp_buffer[0] = 0;
  else 
    snprintf(temp_buffer, FILENAME_MAX, "%s/", parent);
  E->field = _GD_ValidateField(temp_buffer, entry->field);
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
      else {
        /* create an empty file */
        close(open(E->e->file, O_CREAT | O_TRUNC, 0666));

        if (D->first_field == NULL) {
          /* This is the first raw field */
          E->e->first = 1;
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

  /* Invalidate the field lists */
  D->list_validity = 0;

  qsort(D->entry, D->n_entries, sizeof(gd_entry_t*), _GD_EntryCmp);

  dreturn("%i", 0);
  return 0;
}

int dirfile_add(DIRFILE* D, const gd_entry_t* entry)
{
  return _GD_Add(D, entry, NULL);
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
  int error = _GD_Add(D, &R, NULL);

  dreturn("%i", error);
  return error;
}

/* add a LINCOM entry */
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
  int error = _GD_Add(D, &L, NULL);

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
  int error = _GD_Add(D, &L, NULL);

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
  int error = _GD_Add(D, &B, NULL);

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
  int error = _GD_Add(D, &M, NULL);

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
  int error = _GD_Add(D, &P, NULL);

  dreturn("%i", error);
  return error;
}

/* add a STRING entry */
int dirfile_add_string(DIRFILE* D, const char* field_code, const char* value,
    int format_file)
{
  dtrace("%p, \"%s\", \"%s\", %i", D, field_code, value, format_file);

  gd_entry_t *entry;
  gd_entry_t S;
  S.field = (char*)field_code;
  S.field_type = GD_STRING_ENTRY;
  S.format_file = format_file;
  int error = _GD_Add(D, &S, NULL);

  /* Actually store the string, now */
  if (!error) {
    entry = _GD_GetEntry(D, field_code);

    if (D->error == GD_E_OK)
      _GD_DoFieldOut(D, entry, field_code, 0, 0, 0, 0, GD_NULL, value);

    if (D->error)
      error = -1;
  }

  dreturn("%i", error);
  return error;
}

/* add a CONST entry */
int dirfile_add_const(DIRFILE* D, const char* field_code, gd_type_t const_type,
    gd_type_t data_type, const void* value, int format_file)
{
  dtrace("%p, \"%s\", 0x%x, 0x%x, %p, %i", D, field_code, const_type, data_type,
      value, format_file);

  gd_entry_t *entry;
  gd_entry_t C;
  C.field = (char*)field_code;
  C.field_type = GD_CONST_ENTRY;
  C.const_type = const_type;
  C.format_file = format_file;
  int error = _GD_Add(D, &C, NULL);

  /* Actually store the constant, now */
  if (!error) {
    entry = _GD_GetEntry(D, field_code);

    if (D->error == GD_E_OK)
      _GD_DoFieldOut(D, entry, field_code, 0, 0, 0, 0, data_type, value);

    if (D->error)
      error = -1;
  }

  dreturn("%i", error);
  return error;
}

int dirfile_add_meta(DIRFILE* D, gd_entry_t* entry, const char* parent)
{
  return _GD_Add(D, entry, parent);
}

/* add a META RAW entry */
int dirfile_add_metaraw(DIRFILE* D, const char* field_code, const char* parent,
    gd_type_t data_type, unsigned int spf)
{
  dtrace("%p, \"%s\", \"%s\", %i, %x", D, field_code, parent, spf,
      data_type);

  gd_entry_t R;
  R.field = (char*)field_code;
  R.field_type = GD_RAW_ENTRY;
  R.spf = spf;
  R.data_type = data_type;
  R.format_file = GD_FORMAT_AUTO;
  int error = _GD_Add(D, &R, parent);

  dreturn("%i", error);
  return error;
}

/* add a META LINCOM entry */
int dirfile_add_metalincom(DIRFILE* D, const char* field_code,
    const char* parent, int n_fields, const char** in_fields, const double* m,
    const double* b)
{
  dtrace("%p, \"%s\", \"%s\", %i, %p, %p, %p", D, field_code, parent,
      n_fields, in_fields, m, b);

  int i;
  gd_entry_t L;
  L.field = (char*)field_code;
  L.field_type = GD_LINCOM_ENTRY;
  L.n_fields = n_fields;
  L.format_file = GD_FORMAT_AUTO;

  for (i = 0; i < n_fields; ++i) {
    L.in_fields[i] = (char*)in_fields[i];
    L.m[i] = m[i];
    L.b[i] = b[i];
  }
  int error = _GD_Add(D, &L, parent);

  dreturn("%i", error);
  return error;
}

/* add a META LINTERP entry */
int dirfile_add_metalinterp(DIRFILE* D, const char* field_code,
    const char* parent, const char* in_field, const char* table)
{
  dtrace("%p, \"%s\", \"%s\", \"%s\", \"%s\"", D, field_code, parent, in_field,
      table);

  gd_entry_t L;
  L.field = (char*)field_code;
  L.field_type = GD_LINTERP_ENTRY;
  L.in_fields[0] = (char*)in_field;
  L.table = (char*)table;
  L.format_file = GD_FORMAT_AUTO;
  int error = _GD_Add(D, &L, parent);

  dreturn("%i", error);
  return error;
}

/* add a META BIT entry */
int dirfile_add_metabit(DIRFILE* D, const char* field_code, const char* parent,
    const char* in_field, int bitnum, int numbits)
{
  dtrace("%p, \"%s\", \"%s\", \"%s\", %i, %in", D, field_code, parent, in_field,
      bitnum, numbits);

  gd_entry_t B;
  B.field = (char*)field_code;
  B.field_type = GD_BIT_ENTRY;
  B.in_fields[0] = (char*)in_field;
  B.bitnum = bitnum;
  B.numbits = numbits;
  B.format_file = GD_FORMAT_AUTO;
  int error = _GD_Add(D, &B, parent);

  dreturn("%i", error);
  return error;
}

/* add a META MULTIPLY entry */
int dirfile_add_metamultiply(DIRFILE* D, const char* field_code,
    const char* parent, const char* in_field1, const char* in_field2)
{
  dtrace("%p, \"%s\", \"%s\", \"%s\", \"%s\"", D, field_code, parent,
      in_field1, in_field2);

  gd_entry_t M;
  M.field = (char*)field_code;
  M.field_type = GD_MULTIPLY_ENTRY;
  M.in_fields[0] = (char*)in_field1;
  M.in_fields[1] = (char*)in_field2;
  M.format_file = GD_FORMAT_AUTO;
  int error = _GD_Add(D, &M, parent);

  dreturn("%i", error);
  return error;
}

/* add a META PHASE entry */
int dirfile_add_metaphase(DIRFILE* D, const char* field_code,
    const char* parent, const char* in_field, int shift)
{
  dtrace("%p, \"%s\", \"%s\", \"%s\", %i", D, field_code, parent, in_field,
      shift);

  gd_entry_t P;
  P.field = (char*)field_code;
  P.field_type = GD_PHASE_ENTRY;
  P.in_fields[0] = (char*)in_field;
  P.shift = shift;
  P.format_file = GD_FORMAT_AUTO;
  int error = _GD_Add(D, &P, parent);

  dreturn("%i", error);
  return error;
}

/* add a META STRING entry */
int dirfile_add_metastring(DIRFILE* D, const char* field_code,
    const char* parent, const char* value)
{
  dtrace("%p, \"%s\", \"%s\", \"%s\"", D, field_code, parent, value);

  gd_entry_t *entry;
  gd_entry_t S;
  S.field = (char*)field_code;
  S.field_type = GD_STRING_ENTRY;
  S.format_file = GD_FORMAT_AUTO;
  int error = _GD_Add(D, &S, parent);

  /* Actually store the string, now */
  if (!error) {
    entry = _GD_GetEntry(D, field_code);

    if (D->error == GD_E_OK)
      _GD_DoFieldOut(D, entry, field_code, 0, 0, 0, 0, GD_NULL, value);

    if (D->error)
      error = -1;
  }

  dreturn("%i", error);
  return error;
}

/* add a META CONST entry */
int dirfile_add_metaconst(DIRFILE* D, const char* field_code,
    const char* parent, gd_type_t const_type, gd_type_t data_type,
    const void* value)
{
  dtrace("%p, \"%s\", \"%s\", 0x%x, 0x%x, %p", D, field_code, parent,
      const_type, data_type, value);

  gd_entry_t *entry;
  gd_entry_t C;
  C.field = (char*)field_code;
  C.field_type = GD_CONST_ENTRY;
  C.const_type = const_type;
  C.format_file = GD_FORMAT_AUTO;
  int error = _GD_Add(D, &C, parent);

  /* Actually store the constant, now */
  if (!error) {
    entry = _GD_GetEntry(D, field_code);

    if (D->error == GD_E_OK)
      _GD_DoFieldOut(D, entry, field_code, 0, 0, 0, 0, data_type, value);

    if (D->error)
      error = -1;
  }

  dreturn("%i", error);
  return error;
}
