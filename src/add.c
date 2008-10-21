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
#include <errno.h>
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
  dtrace("%p, %p, \"%s\"", D, entry, parent);

  char temp_buffer[FILENAME_MAX];
  int i, u;
  gd_entry_t* E;
  gd_entry_t* P = NULL;

  _GD_ClearError(D);

  /* check access mode */
  if ((D->flags & GD_ACCMODE) == GD_RDONLY) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* check for include index out of range */
  if (P == NULL && (entry->fragment_index < 0 ||
        entry->fragment_index >= D->n_fragment))
  {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, entry->fragment_index, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* check protection */
  if (D->fragment[entry->fragment_index].protection & GD_PROTECT_FORMAT ||
      (entry->field_type == GD_RAW_ENTRY &&
       D->fragment[entry->fragment_index].protection & GD_PROTECT_DATA))
  {
    _GD_SetError(D, GD_E_PROTECTED, 0, NULL, 0,
        D->fragment[entry->fragment_index].cname);
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

    P = _GD_FindField(D, parent, NULL);
    if (P == NULL) {
      _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, parent);
      dreturn("%i", -1);
      return -1;
    }

    snprintf(temp_buffer, FILENAME_MAX, "%s/%s", parent, entry->field);
  } else
    snprintf(temp_buffer, FILENAME_MAX, "%s", entry->field);

  /* check for duplicate field */
  E = _GD_FindField(D, temp_buffer, &u);

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
      entry->field_type != GD_PHASE_ENTRY &&
      entry->field_type != GD_CONST_ENTRY &&
      entry->field_type != GD_STRING_ENTRY)
  {
    _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_TYPE, NULL,
        entry->field_type, NULL);
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
  if (P)
    E->fragment_index = P->fragment_index;
  else
    E->fragment_index = entry->fragment_index;

  E->e = malloc(sizeof(struct _gd_private_entry));
  if (E->e == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    free(E);
    dreturn("%i", -1);
    return -1;
  }
  memset(E->e, 0, sizeof(struct _gd_private_entry));

  /* Validate field code */
  E->field_type = entry->field_type;
  E->field = _GD_ValidateField(P, entry->field);

  if (E->field == entry->field) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, entry->field);
    E->field = NULL;
    _GD_FreeE(E, 1);
    dreturn("%i", -1);
    return -1;
  } else if (E->field == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    _GD_FreeE(E, 1);
    dreturn("%i", -1);
    return -1;
  }

  /* Set meta indicies */
  if (parent != NULL)
    E->e->n_meta = -1;

  /* Validate entry and add auxiliary data */
  switch(entry->field_type)
  {
    case GD_RAW_ENTRY:
      /* no METARAW fields allowed */
      if (parent != NULL) {
        _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_METARAW, NULL,
            entry->field_type, NULL);
        break;
      }

      /* If the encoding scheme is unknown, we can't add the field */
      if ((D->fragment[E->fragment_index].flags & GD_ENCODING) ==
          GD_AUTO_ENCODED)
      {
        _GD_SetError(D, GD_E_UNKNOWN_ENCODING, 0, NULL, 0, NULL);
        break;
      }

      /* If the encoding scheme is unsupported, we can't add the field */
      if ((D->fragment[E->fragment_index].flags & GD_ENCODING) ==
          GD_ENC_UNSUPPORTED)
      {
        _GD_SetError(D, GD_E_UNSUPPORTED, 0, NULL, 0, NULL);
        break;
      }

      E->data_type = entry->data_type;
      E->e->fp = -1;
      E->e->encoding = GD_ENC_UNKNOWN;

      if ((E->e->file = malloc(FILENAME_MAX)) == NULL) {
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
        break;
      }

      snprintf(E->e->file, FILENAME_MAX, "%s/%s/%s", D->name,
          D->fragment[E->fragment_index].sname, E->field);

      /* Set the subencoding subscheme */
      _GD_ResolveEncoding(E->e->file, D->fragment[E->fragment_index].flags,
          E->e);

      if ((E->spf = entry->spf) <= 0)
        _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_SPF, NULL, entry->spf,
            NULL);
      else if (E->data_type & 0x40 || (E->size = GD_SIZE(E->data_type)) == 0)
        _GD_SetError(D, GD_E_BAD_TYPE, 0, NULL, entry->data_type, NULL);
      else if (encode[E->e->encoding].touch == NULL) 
        _GD_SetError(D, GD_E_UNSUPPORTED, 0, NULL, 0, NULL);
      else if ((*encode[E->e->encoding].touch)(E->e->file))
        _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file, errno, NULL);
      else if (D->fragment[E->fragment_index].ref_name == NULL) {
        /* This is the first raw field in this fragment; propagate it upwards */
        for (i = E->fragment_index; i != -1; i = D->fragment[i].parent)
          if (D->fragment[i].ref_name == NULL) {
            D->fragment[E->fragment_index].ref_name = strdup(E->field);
            D->fragment[i].modified = 1;
          } else
            break;
        /* Is this the first raw field ever defined? */
        if (D->reference_field == NULL)
          D->reference_field = E;
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
      E->const_type = entry->const_type;

      if (E->const_type & 0x40 || GD_SIZE(E->const_type) == 0)
        _GD_SetError(D, GD_E_BAD_TYPE, 0, NULL, E->const_type, NULL);
      else if (P == NULL)
        D->n_const++;
      else
        P->e->n_meta_const++;
      break;
    case GD_STRING_ENTRY:
      E->e->string = strdup("");
      if (P == NULL)
        D->n_string++;
      else
        P->e->n_meta_string++;
      break;
    case GD_INDEX_ENTRY:
    case GD_NO_ENTRY:
      _GD_InternalError(D); /* We've already verrified field_type is valid */
      break;
  }

  if (D->error != GD_E_OK) {
    _GD_FreeE(E, 1);
    dreturn("%i", -1);
    return -1;
  }

  if (P) {
    P->e->meta_entry = realloc(P->e->meta_entry, (P->e->n_meta + 1) *
        sizeof(gd_entry_t*));
    P->e->meta_entry[P->e->n_meta] = E;
    P->e->n_meta++;
    D->n_meta++;
  }

  /* add the entry and resort the entry list */
  D->entry = realloc(D->entry, (D->n_entries + 1) * sizeof(gd_entry_t*));
  _GD_InsertSort(D, E, u);
  D->n_entries++;
  D->fragment[E->fragment_index].modified = 1;

  /* Invalidate the field lists */
  D->list_validity = 0;
  D->type_list_validity = 0;

  dreturn("%i", 0);
  return 0;
}

/* add a META field by parsing a field spec */
int dirfile_madd_spec(DIRFILE* D, const char* line, const char* parent)
{
  char instring[MAX_LINE_LENGTH];
  char outstring[MAX_LINE_LENGTH];
  const char *in_cols[MAX_IN_COLS];
  int n_cols;
  int me;
  gd_entry_t* E = NULL;

  dtrace("%p, \"%s\", \"%s\"", D, line, parent);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* check access mode */
  if ((D->flags & GD_ACCMODE) == GD_RDONLY) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ClearError(D);

  E = _GD_FindField(D, parent, NULL);
  if (E == NULL) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, parent);
    dreturn("%i", -1);
    return -1;
  }

  me = E->fragment_index;

  /* check protection */
  if (D->fragment[me].protection & GD_PROTECT_FORMAT) {
    _GD_SetError(D, GD_E_PROTECTED, 0, NULL, 0, D->fragment[me].cname);
    dreturn("%i", -1);
    return -1;
  }

  /* we do this to ensure line is not too long */
  strncpy(instring, line, MAX_LINE_LENGTH - 1);
  instring[MAX_LINE_LENGTH - 2] = '\0';

  /* start parsing */
  n_cols = _GD_Tokenise(D, instring, outstring, in_cols, "dirfile_madd_spec()",
      0);

  if (D->error) {
    dreturn("%i", -1); /* tokeniser threw an error */
    return -1;
  }

  /* Directive parsing is skipped -- The Field Spec parser will add the field */
  _GD_ParseFieldSpec(D, n_cols, in_cols, E, "dirfile_madd_spec()", 0, me,
    DIRFILE_STANDARDS_VERSION, 1, 1);

  if (D->error) {
    dreturn("%i", -1); /* field spec parser threw an error */
    return -1;
  }

  dreturn("%i", 0);
  return 0;
}

/* add a field by parsing a field spec */
int dirfile_add_spec(DIRFILE* D, const char* line, int fragment_index)
{
  char instring[MAX_LINE_LENGTH];
  char outstring[MAX_LINE_LENGTH];
  const char *in_cols[MAX_IN_COLS];
  int n_cols;

  dtrace("%p, \"%s\", %i", D, line, fragment_index);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* check access mode */
  if ((D->flags & GD_ACCMODE) == GD_RDONLY) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* check for include index out of range */
  if (fragment_index < 0 || fragment_index >= D->n_fragment) {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, fragment_index, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* check protection */
  if (D->fragment[fragment_index].protection & GD_PROTECT_FORMAT) {
    _GD_SetError(D, GD_E_PROTECTED, 0, NULL, 0,
        D->fragment[fragment_index].cname);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ClearError(D);

  /* we do this to ensure line is not too long */
  strncpy(instring, line, MAX_LINE_LENGTH - 1);
  instring[MAX_LINE_LENGTH - 2] = '\0';

  /* start parsing */
  n_cols = _GD_Tokenise(D, instring, outstring, in_cols, "dirfile_add_spec()",
      0);

  if (D->error) {
    dreturn("%i", -1); /* tokeniser threw an error */
    return -1;
  }

  /* Directive parsing is skipped -- The Field Spec parser will add the field */
  _GD_ParseFieldSpec(D, n_cols, in_cols, NULL, "dirfile_add_spec()", 0, 
      fragment_index, DIRFILE_STANDARDS_VERSION, 1, 1);

  if (D->error) {
    dreturn("%i", -1); /* field spec parser threw an error */
    return -1;
  }

  dreturn("%i", 0);
  return 0;
}

int dirfile_add(DIRFILE* D, const gd_entry_t* entry)
{
  int ret;

  dtrace("%p, %p", D, entry);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  ret = _GD_Add(D, entry, NULL);

  dreturn("%i", ret);
  return ret;
}

/* add a RAW entry */
int dirfile_add_raw(DIRFILE* D, const char* field_code, gd_type_t data_type,
    unsigned int spf, int fragment_index)
{
  dtrace("%p, \"%s\", %x, %i %i", D, field_code, data_type, spf, fragment_index);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t R;
  R.field = (char*)field_code;
  R.field_type = GD_RAW_ENTRY;
  R.spf = spf;
  R.data_type = data_type;
  R.fragment_index = fragment_index;
  int error = _GD_Add(D, &R, NULL);

  dreturn("%i", error);
  return error;
}

/* add a LINCOM entry */
int dirfile_add_lincom(DIRFILE* D, const char* field_code, int n_fields,
    const char** in_fields, const double* m, const double* b, int fragment_index)
{
  dtrace("%p, \"%s\", %i, %p, %p, %p, %i", D, field_code, n_fields, in_fields,
      m, b, fragment_index);

  int i;

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t L;
  L.field = (char*)field_code;
  L.field_type = GD_LINCOM_ENTRY;
  L.n_fields = n_fields;
  L.fragment_index = fragment_index;

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
    const char* in_field, const char* table, int fragment_index)
{
  dtrace("%p, \"%s\", \"%s\", \"%s\", %i", D, field_code, in_field, table,
      fragment_index);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t L;
  L.field = (char*)field_code;
  L.field_type = GD_LINTERP_ENTRY;
  L.in_fields[0] = (char*)in_field;
  L.table = (char*)table;
  L.fragment_index = fragment_index;
  int error = _GD_Add(D, &L, NULL);

  dreturn("%i", error);
  return error;
}

/* add a BIT entry */
int dirfile_add_bit(DIRFILE* D, const char* field_code, const char* in_field,
    int bitnum, int numbits, int fragment_index)
{
  dtrace("%p, \"%s\", \"%s\", %i, %i, %i\n", D, field_code, in_field, bitnum,
      numbits, fragment_index);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t B;
  B.field = (char*)field_code;
  B.field_type = GD_BIT_ENTRY;
  B.in_fields[0] = (char*)in_field;
  B.bitnum = bitnum;
  B.numbits = numbits;
  B.fragment_index = fragment_index;
  int error = _GD_Add(D, &B, NULL);

  dreturn("%i", error);
  return error;
}

/* add a MULTIPLY entry */
int dirfile_add_multiply(DIRFILE* D, const char* field_code,
    const char* in_field1, const char* in_field2, int fragment_index)
{
  dtrace("%p, \"%s\", \"%s\", \"%s\", %i", D, field_code, in_field1, in_field2,
      fragment_index);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t M;
  M.field = (char*)field_code;
  M.field_type = GD_MULTIPLY_ENTRY;
  M.in_fields[0] = (char*)in_field1;
  M.in_fields[1] = (char*)in_field2;
  M.fragment_index = fragment_index;
  int error = _GD_Add(D, &M, NULL);

  dreturn("%i", error);
  return error;
}

/* add a PHASE entry */
int dirfile_add_phase(DIRFILE* D, const char* field_code, const char* in_field,
    int shift, int fragment_index)
{
  dtrace("%p, \"%s\", \"%s\", %i, %i", D, field_code, in_field, shift,
      fragment_index);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t P;
  P.field = (char*)field_code;
  P.field_type = GD_PHASE_ENTRY;
  P.in_fields[0] = (char*)in_field;
  P.shift = shift;
  P.fragment_index = fragment_index;
  int error = _GD_Add(D, &P, NULL);

  dreturn("%i", error);
  return error;
}

/* add a STRING entry */
int dirfile_add_string(DIRFILE* D, const char* field_code, const char* value,
    int fragment_index)
{
  dtrace("%p, \"%s\", \"%s\", %i", D, field_code, value, fragment_index);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t *entry;
  gd_entry_t S;
  S.field = (char*)field_code;
  S.field_type = GD_STRING_ENTRY;
  S.fragment_index = fragment_index;
  int error = _GD_Add(D, &S, NULL);

  /* Actually store the string, now */
  if (!error) {
    entry = _GD_FindField(D, field_code, NULL);

    if (entry == NULL)
      _GD_InternalError(D); /* We should be able to find it: we just added it */
    else
      _GD_DoFieldOut(D, entry, field_code, 0, 0, 0, 0, GD_NULL, value);

    if (D->error)
      error = -1;
  }

  dreturn("%i", error);
  return error;
}

/* add a CONST entry */
int dirfile_add_const(DIRFILE* D, const char* field_code, gd_type_t const_type,
    gd_type_t data_type, const void* value, int fragment_index)
{
  dtrace("%p, \"%s\", 0x%x, 0x%x, %p, %i", D, field_code, const_type, data_type,
      value, fragment_index);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t *entry;
  gd_entry_t C;
  C.field = (char*)field_code;
  C.field_type = GD_CONST_ENTRY;
  C.const_type = const_type;
  C.fragment_index = fragment_index;
  int error = _GD_Add(D, &C, NULL);

  /* Actually store the constant, now */
  if (!error) {
    entry = _GD_FindField(D, field_code, NULL);

    if (entry == NULL)
      _GD_InternalError(D); /* We should be able to find it: we just added it */
    else
      _GD_DoFieldOut(D, entry, field_code, 0, 0, 0, 0, data_type, value);

    if (D->error)
      error = -1;
  }

  dreturn("%i", error);
  return error;
}

int dirfile_madd(DIRFILE* D, const gd_entry_t* entry, const char* parent)
{
  int ret;

  dtrace("%p, %p, \"%s\"", D, entry, parent);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  ret = _GD_Add(D, entry, parent);

  dreturn("%i", ret);
  return ret;
}

/* add a META LINCOM entry */
int dirfile_madd_lincom(DIRFILE* D, const char* parent, const char* field_code,
    int n_fields, const char** in_fields, const double* m, const double* b)
{
  dtrace("%p, \"%s\", \"%s\", %i, %p, %p, %p", D, field_code, parent,
      n_fields, in_fields, m, b);

  int i;

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t L;
  L.field = (char*)field_code;
  L.field_type = GD_LINCOM_ENTRY;
  L.n_fields = n_fields;
  L.fragment_index = 0;

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
int dirfile_madd_linterp(DIRFILE* D, const char* parent, const char* field_code,
    const char* in_field, const char* table)
{
  dtrace("%p, \"%s\", \"%s\", \"%s\", \"%s\"", D, field_code, parent, in_field,
      table);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t L;
  L.field = (char*)field_code;
  L.field_type = GD_LINTERP_ENTRY;
  L.in_fields[0] = (char*)in_field;
  L.table = (char*)table;
  L.fragment_index = 0;
  int error = _GD_Add(D, &L, parent);

  dreturn("%i", error);
  return error;
}

/* add a META BIT entry */
int dirfile_madd_bit(DIRFILE* D, const char* parent, const char* field_code,
    const char* in_field, int bitnum, int numbits)
{
  dtrace("%p, \"%s\", \"%s\", \"%s\", %i, %in", D, field_code, parent, in_field,
      bitnum, numbits);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t B;
  B.field = (char*)field_code;
  B.field_type = GD_BIT_ENTRY;
  B.in_fields[0] = (char*)in_field;
  B.bitnum = bitnum;
  B.numbits = numbits;
  B.fragment_index = 0;
  int error = _GD_Add(D, &B, parent);

  dreturn("%i", error);
  return error;
}

/* add a META MULTIPLY entry */
int dirfile_madd_multiply(DIRFILE* D, const char* parent,
    const char* field_code, const char* in_field1, const char* in_field2)
{
  dtrace("%p, \"%s\", \"%s\", \"%s\", \"%s\"", D, field_code, parent,
      in_field1, in_field2);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t M;
  M.field = (char*)field_code;
  M.field_type = GD_MULTIPLY_ENTRY;
  M.in_fields[0] = (char*)in_field1;
  M.in_fields[1] = (char*)in_field2;
  M.fragment_index = 0;
  int error = _GD_Add(D, &M, parent);

  dreturn("%i", error);
  return error;
}

/* add a META PHASE entry */
int dirfile_madd_phase(DIRFILE* D, const char* parent, const char* field_code,
    const char* in_field, int shift)
{
  dtrace("%p, \"%s\", \"%s\", \"%s\", %i", D, field_code, parent, in_field,
      shift);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t P;
  P.field = (char*)field_code;
  P.field_type = GD_PHASE_ENTRY;
  P.in_fields[0] = (char*)in_field;
  P.shift = shift;
  P.fragment_index = 0;
  int error = _GD_Add(D, &P, parent);

  dreturn("%i", error);
  return error;
}

/* add a META STRING entry */
int dirfile_madd_string(DIRFILE* D, const char* parent, const char* field_code,
    const char* value)
{
  dtrace("%p, \"%s\", \"%s\", \"%s\"", D, field_code, parent, value);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t *entry;
  gd_entry_t S;
  S.field = (char*)field_code;
  S.field_type = GD_STRING_ENTRY;
  S.fragment_index = 0;
  int error = _GD_Add(D, &S, parent);

  /* Actually store the string, now */
  if (!error) {
    entry = _GD_FindField(D, field_code, NULL);

    if (entry == NULL)
      _GD_InternalError(D); /* We should be able to find it: we just added it */
    else
      _GD_DoFieldOut(D, entry, field_code, 0, 0, 0, 0, GD_NULL, value);

    if (D->error)
      error = -1;
  }

  dreturn("%i", error);
  return error;
}

/* add a META CONST entry */
int dirfile_madd_const(DIRFILE* D, const char* parent, const char* field_code,
    gd_type_t const_type, gd_type_t data_type, const void* value)
{
  dtrace("%p, \"%s\", \"%s\", 0x%x, 0x%x, %p", D, field_code, parent,
      const_type, data_type, value);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  gd_entry_t *entry;
  gd_entry_t C;
  C.field = (char*)field_code;
  C.field_type = GD_CONST_ENTRY;
  C.const_type = const_type;
  C.fragment_index = 0;
  int error = _GD_Add(D, &C, parent);

  /* Actually store the constant, now */
  if (!error) {
    entry = _GD_FindField(D, field_code, NULL);

    if (entry == NULL)
      _GD_InternalError(D); /* We should be able to find it: we just added it */
    else
      _GD_DoFieldOut(D, entry, field_code, 0, 0, 0, 0, data_type, value);

    if (D->error)
      error = -1;
  }

  dreturn("%i", error);
  return error;
}
