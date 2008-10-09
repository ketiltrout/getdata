/* (C) 2002-2005 C. Barth Netterfield
 * (C) 2005-2008 D. V. Wiebe
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
#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#endif

#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif

#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#endif

#define MAX_IN_COLS (3 * GD_MAX_LINCOM + 5) /* for META lincom */

static gd_type_t _GD_RawType(const char* type)
{
  /* for backwards compatibility */
  if (strlen(type) == 1) 
    return _GD_LegacyType(type[0]);

  if (strcmp(type, "NULL") == 0)
    return GD_NULL;
  if (strcmp(type, "INT8") == 0)
    return GD_INT8;
  if (strcmp(type, "UINT8") == 0)
    return GD_UINT8;
  if (strcmp(type, "INT16") == 0)
    return GD_INT16;
  if (strcmp(type, "INT32") == 0)
    return GD_INT32;
  if (strcmp(type, "UINT32") == 0)
    return GD_UINT32;
  if (strcmp(type, "UINT64") == 0)
    return GD_UINT64;
  if (strcmp(type, "INT64") == 0)
    return GD_INT64;
  if (strcmp(type, "UINT16") == 0)
    return GD_UINT16;
  if (strcmp(type, "FLOAT32") == 0)
    return GD_FLOAT32;
  if (strcmp(type, "FLOAT") == 0)
    return GD_FLOAT32;
  if (strcmp(type, "FLOAT64") == 0)
    return GD_FLOAT64;
  if (strcmp(type, "DOUBLE") == 0)
    return GD_FLOAT64;

  return GD_UNKNOWN;
}

/* Check for a valid field name -- returns input on error */
char* _GD_ValidateField(const gd_entry_t* parent, const char* field_code)
{
  size_t len = strlen(field_code);
  size_t i;
  char* ptr;

  dtrace("%p, \"%s\"", parent, field_code);

  for (i = 0; i < len; ++i)
    if (strchr("/\\<>;|&.", field_code[i]) != NULL) {
      dreturn("%p", field_code);
      return (char*)field_code;
    }

  if (parent != NULL) {
    ptr = malloc(strlen(parent->field) + strlen(field_code) + 2);
    sprintf(ptr, "%s/%s", parent->field, field_code);
  } else
    ptr = strdup(field_code);

  dreturn("\"%s\"", ptr);
  return ptr;
}

/* _GD_ParseRaw: parse a RAW data type in the format file
*/
static gd_entry_t* _GD_ParseRaw(DIRFILE* D, const char* in_cols[MAX_IN_COLS],
    int n_cols, const gd_entry_t* parent, const char* subdir,
    const char* format_file, int line)
{
  dtrace("%p, %p, %i, %p, \"%s\", \"%s\", %i", D, in_cols, n_cols, parent,
      subdir, format_file, line);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS, format_file, line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  /* META RAW fields are prohibited */
  if (parent != NULL) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_METARAW, format_file, line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  gd_entry_t* R = malloc(sizeof(gd_entry_t));
  if (R == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  R->e = malloc(sizeof(struct _gd_private_entry));
  if (R->e == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    free(R);
    dreturn("%p", NULL);
    return NULL;
  }

  R->field_type = GD_RAW_ENTRY;
  R->e->file = NULL;
  R->e->fp = -1; /* file not opened yet */
  R->e->stream = NULL; /* file not opened yet */
  R->e->n_meta = -1;
  R->e->encoding = GD_ENC_UNKNOWN; /* don't know the encoding subscheme yet */
  R->format_file = D->n_include - 1;

  R->field = _GD_ValidateField(NULL, in_cols[0]);
  if (R->field == in_cols[0]) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    R->field = NULL;
    dirfile_free_entry_strings(R);
    free(R);
    dreturn("%p", NULL);
    return NULL;
  }

  R->e->file = malloc(FILENAME_MAX);
  if (R->e->file == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    R->field = NULL;
    dirfile_free_entry_strings(R);
    free(R);
    dreturn("%p", NULL);
    return NULL;
  }

  snprintf((char*)R->e->file, FILENAME_MAX, "%s/%s/%s", D->name, subdir,
      in_cols[0]);
  R->data_type = _GD_RawType(in_cols[2]);
  R->size = GD_SIZE(R->data_type);

  if (R->size == 0 || R->data_type & 0x40)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_TYPE, format_file, line,
        in_cols[2]);
  else if ((R->spf = atoi(in_cols[3])) <= 0)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_SPF, format_file, line,
        in_cols[3]);

  if (D->error != GD_E_OK) {
    dirfile_free_entry_strings(R);
    free(R);
    R = NULL;
  }

  dreturn("%p", R);
  return R;
}

/* _GD_ParseLincom: parse a LINCOM data type in the format file.
*/
static gd_entry_t* _GD_ParseLincom(DIRFILE* D, const char* in_cols[MAX_IN_COLS],
    int n_cols, const gd_entry_t* parent, const char* format_file, int line)
{
  int i;

  dtrace("%p, %p, %i, %p, \"%s\", %i", D, in_cols, n_cols, parent, format_file,
      line);

  if (n_cols < 3) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS, format_file, line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  gd_entry_t* L = malloc(sizeof(gd_entry_t));
  if (L == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  L->e = malloc(sizeof(struct _gd_private_entry));
  if (L->e == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    free(L);
    dreturn("%p", NULL);
    return NULL;
  }

  L->field_type = GD_LINCOM_ENTRY;
  for (i = 0; i < GD_MAX_LINCOM; ++i) {
    L->in_fields[i] = NULL;
    L->e->entry[i] = NULL;
  }
  L->format_file = D->n_include - 1;

  if (parent == NULL)
    L->e->n_meta = -1;
  else {
    L->e->n_meta = L->e->n_meta_string = L->e->n_meta_const = 0;
    L->e->meta_entry = NULL;
  }

  L->field = _GD_ValidateField(parent, in_cols[0]);
  if (L->field == in_cols[0]) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    L->field = NULL;
    dirfile_free_entry_strings(L);
    free(L);
    dreturn("%p", NULL);
    return NULL;
  }

  L->n_fields = atoi(in_cols[2]);

  if (L->field == NULL)
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
  else if ((L->n_fields < 1) || (L->n_fields > GD_MAX_LINCOM))
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_FIELDS, format_file, line,
        in_cols[2]);
  else if (n_cols < L->n_fields * 3 + 3)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS, format_file, line, NULL);
  else
    for (i = 0; i < L->n_fields; i++) {
      L->in_fields[i] = strdup(in_cols[i * 3 + 3]);
      if (L->in_fields[i] == NULL)
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      L->m[i] = atof(in_cols[i * 3 + 4]);
      L->b[i] = atof(in_cols[i * 3 + 5]);
    }

  if (D->error != GD_E_OK) {
    dirfile_free_entry_strings(L);
    free(L);
    L = NULL;
  }

  dreturn("%p", L);
  return L;
}

/* _GD_ParseLinterp: parse a LINTERP data type in the format file.
*/
static gd_entry_t* _GD_ParseLinterp(DIRFILE* D,
    const char* in_cols[MAX_IN_COLS], int n_cols, const gd_entry_t* parent,
    const char* format_file, int line)
{
  char temp_buffer[FILENAME_MAX];

  dtrace("%p, %p, %i, %p, \"%s\", %i", D, in_cols, n_cols, parent, format_file,
      line);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS, format_file, line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  gd_entry_t* L = malloc(sizeof(gd_entry_t));
  if (L == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  L->e = malloc(sizeof(struct _gd_private_entry));
  if (L->e == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    free(L);
    dreturn("%p", NULL);
    return NULL;
  }

  L->field_type = GD_LINTERP_ENTRY;
  L->in_fields[0] = NULL;
  L->e->entry[0] = NULL;
  L->table = NULL;
  L->format_file = D->n_include - 1;

  if (parent == NULL)
    L->e->n_meta = -1;
  else {
    L->e->n_meta = L->e->n_meta_string = L->e->n_meta_const = 0;
    L->e->meta_entry = NULL;
  }

  L->field = _GD_ValidateField(parent, in_cols[0]);
  if (L->field == in_cols[0]) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    L->field = NULL;
    dirfile_free_entry_strings(L);
    free(L);
    dreturn("%p", NULL);
    return NULL;
  }

  L->in_fields[0] = strdup(in_cols[2]);
  L->e->table_len = -1; /* linterp file not read yet */


  if (in_cols[3][0] == '/')
    L->table = strdup(in_cols[3]);
  else {
    /* non-absolute paths are relative to the format file's directory */
    L->table = malloc(FILENAME_MAX);
    if (L->table != NULL) {
      strcpy(temp_buffer, format_file);
      strcpy(L->table, dirname(temp_buffer));
      strcat(L->table, "/");
      strcat(L->table, in_cols[3]);
    }
  }

  if (L->field == NULL || L->in_fields[0] == NULL || L->table == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dirfile_free_entry_strings(L);
    free(L);
    L = NULL;
  }

  dreturn("%p", L);
  return L;
}

/* _GD_ParseMultiply: parse MULTIPLY data type entry in format file.
*/
static gd_entry_t* _GD_ParseMultiply(DIRFILE* D,
    const char* in_cols[MAX_IN_COLS], int n_cols, const gd_entry_t* parent,
    const char* format_file, int line)
{
  dtrace("%p, %p, %i, %p, \"%s\", %i", D, in_cols, n_cols, parent, format_file,
      line);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS, format_file, line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  gd_entry_t* M = malloc(sizeof(gd_entry_t));
  if (M == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  M->e = malloc(sizeof(struct _gd_private_entry));
  if (M->e == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    free(M);
    dreturn("%p", NULL);
    return NULL;
  }

  M->field_type = GD_MULTIPLY_ENTRY;
  M->in_fields[0] = M->in_fields[1] = NULL;
  M->e->entry[0] = M->e->entry[1] = NULL;
  M->format_file = D->n_include - 1;

  if (parent != NULL)
    M->e->n_meta = -1;
  else {
    M->e->n_meta = M->e->n_meta_string = M->e->n_meta_const = 0;
    M->e->meta_entry = NULL;
  }

  M->field = _GD_ValidateField(parent, in_cols[0]);
  if (M->field == in_cols[0]) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    M->field = NULL;
    dirfile_free_entry_strings(M);
    free(M);
    dreturn("%p", NULL);
    return NULL;
  }

  M->in_fields[0] = strdup(in_cols[2]);
  M->in_fields[1] = strdup(in_cols[3]);

  if (M->field == NULL || M->in_fields[0] == NULL || M->in_fields[1] == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dirfile_free_entry_strings(M);
    free(M);
    M = NULL;
  }

  dreturn("%p", M);
  return M;
}

/* _GD_ParseBit: parse BIT data type entry in format file.
*/
static gd_entry_t* _GD_ParseBit(DIRFILE* D, const char* in_cols[MAX_IN_COLS],
    int n_cols, const gd_entry_t* parent, const char* format_file, int line)
{
  dtrace("%p, %p, %i, %p, \"%s\", %i", D, in_cols, n_cols, parent, format_file,
      line);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS, format_file, line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  gd_entry_t* B = malloc(sizeof(gd_entry_t));
  if (B == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  B->e = malloc(sizeof(struct _gd_private_entry));
  if (B->e == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    free(B);
    dreturn("%p", NULL);
    return NULL;
  }

  B->field_type = GD_BIT_ENTRY;
  B->in_fields[0] = NULL;
  B->e->entry[0] = NULL;
  B->format_file = D->n_include - 1;

  if (parent == NULL)
    B->e->n_meta = -1;
  else {
    B->e->n_meta = B->e->n_meta_string = B->e->n_meta_const = 0;
    B->e->meta_entry = NULL;
  }

  B->field = _GD_ValidateField(parent, in_cols[0]);
  if (B->field == in_cols[0]) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    B->field = NULL;
    dirfile_free_entry_strings(B);
    free(B);
    dreturn("%p", NULL);
    return NULL;
  }

  B->in_fields[0] = strdup(in_cols[2]);

  if (B->field == NULL || B->in_fields[0] == NULL)
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);

  B->bitnum = atoi(in_cols[3]);
  if (n_cols > 4)
    B->numbits = atoi(in_cols[4]);
  else
    B->numbits = 1;

  if (B->numbits < 1)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_NUMBITS, format_file, line, NULL);
  else if (B->bitnum < 0)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BITNUM, format_file, line, NULL);
  else if (B->bitnum + B->numbits - 1 > 63)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BITSIZE, format_file, line, NULL);

  if (D->error != GD_E_OK) {
    dirfile_free_entry_strings(B);
    free(B);
    B = NULL;
  }

  dreturn("%p", B);
  return B;
}

/* _GD_ParsePhase: parse PHASE data type entry in formats file.
*/
static gd_entry_t* _GD_ParsePhase(DIRFILE* D, const char* in_cols[MAX_IN_COLS],
    int n_cols, const gd_entry_t* parent, const char* format_file, int line)
{
  dtrace("%p, %p, %i, %p, \"%s\", %i", D, in_cols, n_cols, parent, format_file,
      line);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS, format_file, line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  gd_entry_t* P = malloc(sizeof(gd_entry_t));
  if (P == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  P->e = malloc(sizeof(struct _gd_private_entry));
  if (P->e == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    free(P);
    dreturn("%p", NULL);
    return NULL;
  }

  P->field_type = GD_PHASE_ENTRY;
  P->in_fields[0] = NULL;
  P->e->entry[0] = NULL;
  P->format_file = D->n_include - 1;

  if (parent == NULL)
    P->e->n_meta = -1;
  else {
    P->e->n_meta = P->e->n_meta_string = P->e->n_meta_const = 0;
    P->e->meta_entry = NULL;
  }

  P->field = _GD_ValidateField(parent, in_cols[0]);
  if (P->field == in_cols[0]) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    P->field = NULL;
    dirfile_free_entry_strings(P);
    free(P);
    dreturn("%p", NULL);
    return NULL;
  }

  P->in_fields[0] = strdup(in_cols[2]); /* field */
  P->shift = atoi(in_cols[3]); /*shift*/

  if (P->field == NULL || P->in_fields[0] == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dirfile_free_entry_strings(P);
    free(P);
    P = NULL;
  }

  dreturn("%p", P);
  return P;
}

/* _GD_ParseConst: parse CONST data type entry in formats file.
*/
static gd_entry_t* _GD_ParseConst(DIRFILE* D, const char* in_cols[MAX_IN_COLS],
    int n_cols, const gd_entry_t* parent, const char* format_file, int line)
{
  char fmt[50];

  dtrace("%p, %p, %i, %p, \"%s\", %i", D, in_cols, n_cols, parent, format_file,
      line);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS, format_file, line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  gd_entry_t* C = malloc(sizeof(gd_entry_t));
  if (C == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  C->e = malloc(sizeof(struct _gd_private_entry));
  if (C->e == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    free(C);
    dreturn("%p", NULL);
    return NULL;
  }

  C->field_type = GD_CONST_ENTRY;
  C->format_file = D->n_include - 1;

  if (parent != NULL)
    C->e->n_meta = -1;
  else {
    C->e->n_meta = C->e->n_meta_string = C->e->n_meta_const = 0;
    C->e->meta_entry = NULL;
  }

  C->field = _GD_ValidateField(parent, in_cols[0]);
  if (C->field == in_cols[0]) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    C->field = NULL;
    dirfile_free_entry_strings(C);
    free(C);
    dreturn("%p", NULL);
    return NULL;
  }

  if (C->field == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dirfile_free_entry_strings(C);
    free(C);
    C = NULL;
    dreturn("%p", NULL);
    return NULL;
  }

  C->const_type = _GD_RawType(in_cols[2]);

  if (GD_SIZE(C->const_type) == 0 || C->data_type & 0x40) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_TYPE, format_file, line,
        in_cols[2]);
    dirfile_free_entry_strings(C);
    free(C);
    C = NULL;
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_ScanFormat(fmt, C->const_type);
  if (C->const_type & GD_SIGNED)
    sscanf(in_cols[3], fmt, C->e->iconst);
  else if (C->const_type & GD_IEEE754)
    sscanf(in_cols[3], fmt, C->e->dconst);
  else
    sscanf(in_cols[3], fmt, C->e->uconst);

  dreturn("%p", C);
  return C;
}

/* _GD_ParseString: parse STRING data type entry in formats file.
*/
static gd_entry_t* _GD_ParseString(DIRFILE* D, const char *in_cols[MAX_IN_COLS],
    int n_cols, const gd_entry_t* parent, const char* format_file, int line)
{
  dtrace("%p, %p, %i, %p, \"%s\", %i", D, in_cols, n_cols, parent, format_file,
      line);

  if (n_cols < 3) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS, format_file, line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  gd_entry_t* S = malloc(sizeof(gd_entry_t));
  if (S == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  S->e = malloc(sizeof(struct _gd_private_entry));
  if (S->e == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    free(S);
    dreturn("%p", NULL);
    return NULL;
  }

  S->field_type = GD_STRING_ENTRY;
  S->format_file = D->n_include - 1;
  S->e->string = strdup(in_cols[2]);

  if (parent != NULL)
    S->e->n_meta = -1;
  else {
    S->e->n_meta = S->e->n_meta_string = S->e->n_meta_const = 0;
    S->e->meta_entry = NULL;
  }

  S->field = _GD_ValidateField(parent, in_cols[0]);
  if (S->field == in_cols[0]) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    S->field = NULL;
    dirfile_free_entry_strings(S);
    free(S);
    dreturn("%p", NULL);
    return NULL;
  }

  if (S->field == NULL || S->e->string == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dirfile_free_entry_strings(S);
    free(S);
    S = NULL;
    dreturn("%p", NULL);
    return NULL;
  }

  dreturn("%p", S);
  return S;
}

/* _GD_EntryCmp: Comparison functions for the entries
*/
int _GD_EntryCmp(const void *A, const void *B)
{
  return (strcmp((*(gd_entry_t **)A)->field,
        (*(gd_entry_t **)B)->field));
}

static int utf8encode (DIRFILE* D, const char* format_file, int linenum,
    char** op, uint32_t value)
{
  dtrace("%p, %p, %llx", D, op, (long long)value);

  if (value > 0x10FFFF || value == 0) {
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_CHARACTER, format_file, linenum,
          NULL);
      dreturn("%i", 1);
      return 1;
  }

  if (value <= 0x7F)
    *((*op)++) = value;
  else if (value <= 0x7FF) {
    *((*op)++) = 0xC0 + (value >> 6);
    *((*op)++) = 0x80 + (value & 0x3F);
  } else if (value <= 0xFFFF) {
    *((*op)++) = 0xE0 + (value >> 12);
    *((*op)++) = 0x80 + ((value >> 6) & 0x3F);
    *((*op)++) = 0x80 + (value & 0x3F);
  } else {
    *((*op)++) = 0xF0 + (value >> 18);
    *((*op)++) = 0x80 + ((value >> 12) & 0x3F);
    *((*op)++) = 0x80 + ((value >> 6) & 0x3F);
    *((*op)++) = 0x80 + (value & 0x3F);
  }

  dreturn("%i", 0);
  return 0;
}

/* _GD_ParseFieldSpec: Parse a format file line fragment containing a field
 * specification */
static void _GD_ParseFieldSpec(DIRFILE* D, int n_cols,
    const char* in_cols[MAX_IN_COLS], const gd_entry_t* parent,
    const char* subdir, const char* format_file, int linenum, int have_first,
    int me, int standards)
{
  gd_entry_t* E = NULL;

  dtrace("%p, %i, %p, %p, \"%s\", \"%s\", %i, %i, %i, %i", D, n_cols, in_cols,
      parent, subdir, format_file, linenum, have_first, me, standards);

  D->entry = realloc(D->entry, (D->n_entries + 1) * sizeof(gd_entry_t*));

  if (strcmp(in_cols[1], "RAW") == 0) {
    E = _GD_ParseRaw(D, in_cols, n_cols, parent, subdir, format_file, linenum);
    if (!D->error && D->first_field == NULL) {
      /* set the first field */
      D->first_field = malloc(sizeof(gd_entry_t));
      if (D->first_field == NULL)
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      else {
        memcpy(D->first_field, E, sizeof(gd_entry_t));
        have_first = 1;
        D->include_list[me].first = 1;
      }
    }
  } else if (strcmp(in_cols[1], "LINCOM") == 0)
    E = _GD_ParseLincom(D, in_cols, n_cols, parent, format_file, linenum);
  else if (strcmp(in_cols[1], "LINTERP") == 0)
    E = _GD_ParseLinterp(D, in_cols, n_cols, parent, format_file, linenum);
  else if (strcmp(in_cols[1], "MULTIPLY") == 0)
    E = _GD_ParseMultiply(D, in_cols, n_cols, parent, format_file, linenum);
  else if (strcmp(in_cols[1], "BIT") == 0)
    E = _GD_ParseBit(D, in_cols, n_cols, parent, format_file, linenum);
  else if (strcmp(in_cols[1], "PHASE") == 0)
    E = _GD_ParsePhase(D, in_cols, n_cols, parent, format_file, linenum);
  else if (strcmp(in_cols[1], "CONST") == 0) {
    E = _GD_ParseConst(D, in_cols, n_cols, parent, format_file, linenum);
    if (D->error == GD_E_OK)
      D->n_const++;
  } else if (strcmp(in_cols[1], "STRING") == 0) {
    E = _GD_ParseString(D, in_cols, n_cols, parent, format_file, linenum);
    if (D->error == GD_E_OK)
      D->n_string++;
  } else if (standards <= DIRFILE_STANDARDS_VERSION ||
      (D->flags & GD_PEDANTIC))
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_LINE, format_file, linenum,
        NULL);

  if (D->error == GD_E_OK && E != NULL) {
    /* Check for duplicate */
    int u;
    const gd_entry_t* Q = _GD_GetEntry(D, E->field, &u);

    if (D == GD_E_OK)
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_DUPLICATE, format_file, linenum,
          D->include_list[Q->format_file].cname);
    else {
      _GD_ClearError(D);

      /* sort */
      _GD_InsertSort(D, E, u);
      D->n_entries++;
    }
  }

  dreturnvoid();
}

/* _GD_ParseFormatFile: Perform the actual parsing of the format file.  This
 *       function is called from GetFormat once for the main format file and
 *       once for each included file.
 *
 *       Returns 0 unless this format file contains the first raw field.
 */
#define ACC_MODE_NONE  0
#define ACC_MODE_OCTAL 1
#define ACC_MODE_HEX   2
#define ACC_MODE_UTF8  3
static int _GD_ParseFormatFile(FILE* fp, DIRFILE *D, const char* filedir,
    const char* subdir, const char* format_file, int format_parent,
    int standards)
{
  char instring[MAX_LINE_LENGTH];
  char outstring[MAX_LINE_LENGTH];
  const char *in_cols[MAX_IN_COLS];
  char* ip;
  char* op = outstring;
  int n_cols = 0;
  int linenum = 0;
  int escaped_char = 0;
  int quotated = 0;
  int ws = 1;
  uint32_t accumulator = 0;
  int n_acc = 0;
  int acc_mode = ACC_MODE_NONE;
  int me = D->n_include - 1;
  int have_first = 0;

  dtrace("%p, %p, \"%s\", \"%s\", \"%s\", %i", fp, D, filedir, subdir,
      format_file, standards);

  /* start parsing */
  while (_GD_GetLine(fp, instring, &linenum)) {
    n_cols = 0;
    outstring[0] = '\0';

    /* tokenise the line */
    for (ip = instring; *ip != '\0'; ++ip) {
      if (escaped_char) {
        if (ws) {
          if (n_cols >= MAX_IN_COLS)
            break; /* Ignore trailing data on the line */
          in_cols[n_cols++] = op;
        }

        if (acc_mode == ACC_MODE_OCTAL) {
          if (*ip >= '0' && *ip <= '7') {
            accumulator = accumulator * 8 + *ip - '0';
            n_acc++;
          }

          if (n_acc == 3 || accumulator > 037 || *ip < '0' || *ip > '7') {
            if (accumulator == 0) {
              _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_CHARACTER, format_file,
                  linenum, NULL);
              break;
            }

            *(op++) = accumulator;

            if (*ip < '0' || *ip > '7')
              ip--; /* rewind */
            escaped_char = 0;
          }
        } else if (acc_mode != ACC_MODE_NONE) {
          if (isxdigit(*ip)) {
            n_acc++;
            if (*ip >= '0' && *ip <= '9')
              accumulator = accumulator * 16 + *ip - '0';
            else if (*ip >= 'A' && *ip <= 'F')
              accumulator = accumulator * 16 + *ip - 'A';
            else
              accumulator = accumulator * 16 + *ip - 'a';
          }

          if (acc_mode == ACC_MODE_HEX && (n_acc == 2 || !isxdigit(*ip))) {
            if (accumulator == 0) {
              _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_CHARACTER, format_file,
                  linenum, NULL);
              break;
            }

            *(op++) = accumulator;

            if (!isxdigit(*ip))
              ip--; /* rewind */
            escaped_char = 0;
          } else if (acc_mode == ACC_MODE_UTF8 && (n_acc == 7 ||
                accumulator > 0x10FF || !isxdigit(*ip)))
          {
            if (utf8encode(D, format_file, linenum, &op, accumulator))
              break; /* syntax error */

            if (!isxdigit(*ip))
              ip--; /* rewind */
            escaped_char = 0;
          }
        } else {
          switch(*ip) {
            case 'a':
              *(op++) = '\a';
              escaped_char = 0;
              break;
            case 'b':
              *(op++) = '\b';
              escaped_char = 0;
              break;
            case 'e':
              *(op++) = '\x1B';
              escaped_char = 0;
              break;
            case 'f':
              *(op++) = '\b';
              escaped_char = 0;
              break;
            case 'n':
              *(op++) = '\n';
              escaped_char = 0;
              break;
            case 'r':
              *(op++) = '\r';
              escaped_char = 0;
              break;
            case 't':
              *(op++) = '\t';
              escaped_char = 0;
              break;
            case 'v':
              *(op++) = '\v';
              escaped_char = 0;
              break;
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
              n_acc = 1;
              accumulator = *ip - '0';
              acc_mode = ACC_MODE_OCTAL;
              break;
            case 'u':
              n_acc = 0;
              accumulator = 0;
              acc_mode = ACC_MODE_UTF8;
              break;
            case 'x':
              n_acc = 0;
              accumulator = 0;
              acc_mode = ACC_MODE_HEX;
              break;
            default:
              *(op++) = *ip;
              escaped_char = 0;
          }
        }
      } else {
        if (*ip == '\\')
          escaped_char = 1;
        else if (*ip == '"')
          quotated = !quotated;
        else if (!quotated && isspace(*ip)) {
          if (!ws)
            *(op++) = '\0';
          ws = 1;
        } else if (!quotated && *ip == '#') {
          *op = '\0';
          break;
        } else {
          if (ws) {
            if (n_cols >= MAX_IN_COLS)
              break; /* Ignore trailing data on the line */
            in_cols[n_cols++] = op;
          }
          ws = 0;
          *(op++) = *ip;
        }
      }
    }

    /* set up for possibly slashed reserved words */
    ip = (char*)in_cols[0] + ((in_cols[0][0] == '/') ? 1 : 0);

    if (D->error) {
      ; /* tokeniser threw an error */
    } else if (quotated || escaped_char) { /* Unterminated token */
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_UNTERM, format_file, linenum,
          NULL);
    } else if (n_cols < 2) {
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS, format_file, linenum,
          NULL);

      /* Directives */

    } else if (strcmp(ip, "FRAMEOFFSET") == 0) {
      D->frame_offset = atoll(in_cols[1]);
    } else if (strcmp(ip, "INCLUDE") == 0) {
      unsigned int i;
      int found = 0;
      char temp_buffer[FILENAME_MAX];
      char new_format_file[FILENAME_MAX];
      char new_subdir[FILENAME_MAX];
      FILE* new_fp = NULL;

      /* create the format filename */
      snprintf(new_format_file, FILENAME_MAX, "%s/%s/%s", filedir,
          subdir, in_cols[1]);

      /* Run through the include list to see if we've already included this
       * file */
      for (i = 0; i < D->n_include; ++i)
        if (strcmp(new_format_file, D->include_list[i].cname) == 0) {
          found = 1;
          break;
        }

      /* If we found the file, we won't reopen it.  Continue parsing. */
      if (found)
        continue;

      /* Otherwise, try to open the file */
      new_fp = fopen(new_format_file, "r");

      /* If opening the file failed, set the error code and abort parsing. */
      if (new_fp == NULL) {
        _GD_SetError(D, GD_E_OPEN_INCLUDE, errno, format_file, linenum,
            new_format_file);
        break;
      }

      /* If we got here, we managed to open the included file; parse it */
      D->include_list = realloc(D->include_list, ++(D->n_include) *
          sizeof(struct gd_include_t));
      if (D->include_list == NULL) {
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
        dreturn("%i", have_first);
        return have_first;
      }
      D->include_list[D->n_include - 1].cname = strdup(new_format_file);
      D->include_list[D->n_include - 1].ename = strdup(in_cols[1]);
      D->include_list[D->n_include - 1].modified = 0;
      D->include_list[D->n_include - 1].parent = format_parent;
      D->include_list[D->n_include - 1].first = 0;

      if (D->include_list[D->n_include - 1].cname == NULL ||
          D->include_list[D->n_include - 1].ename == NULL)
      {
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
        dreturn("%i", have_first);
        return have_first;
      }

      /* extract the subdirectory name - dirname both returns a volatile string
       * and modifies its argument, ergo strcpy */
      strcpy(temp_buffer, in_cols[1]);
      if (strcmp(subdir, ".") == 0)
        strcpy(new_subdir, dirname(temp_buffer));
      else
        snprintf(new_subdir, FILENAME_MAX, "%s/%s", subdir,
            dirname(temp_buffer));

      if (_GD_ParseFormatFile(new_fp, D, filedir, new_subdir, new_format_file,
            me, standards))
        D->include_list[me].first = 1;
      fclose(new_fp);
    } else if (strcmp(ip, "ENCODING") == 0) {
      if (!(D->flags & GD_FORCE_ENCODING)) {
        if (strcmp(in_cols[1], "none") == 0)
          D->flags = (D->flags & ~GD_ENCODING) | GD_UNENCODED;
        else if (strcmp(in_cols[1], "slim") == 0)
          D->flags = (D->flags & ~GD_ENCODING) | GD_SLIM_ENCODED;
        else if (strcmp(in_cols[1], "text") == 0)
          D->flags = (D->flags & ~GD_ENCODING) | GD_TEXT_ENCODED;
        else
          D->flags = (D->flags & ~GD_ENCODING) | GD_ENC_UNSUPPORTED;
      }
    } else if (strcmp(ip, "ENDIAN") == 0) {
      if (!(D->flags & GD_FORCE_ENDIAN)) {
        if (strcmp(in_cols[1], "big") == 0) {
          D->flags |= GD_BIG_ENDIAN;
          D->flags &= ~GD_LITTLE_ENDIAN;
        } else if (strcmp(in_cols[1], "little") == 0) {
          D->flags |= GD_LITTLE_ENDIAN;
          D->flags &= ~GD_BIG_ENDIAN;
        } else 
          _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_ENDIAN, format_file, linenum,
              NULL);
      }
    } else if (strcmp(ip, "META") == 0) {
      const gd_entry_t* P =  _GD_GetEntry(D, in_cols[1], NULL);
      if (P == NULL)
        _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_NO_PARENT, format_file,
            linenum, in_cols[1]);
      else {
        if (n_cols < 4)
          _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS, format_file, linenum,
              NULL);
        else {
          _GD_ParseFieldSpec(D, n_cols - 2, in_cols + 2, P, subdir,
              format_file, linenum, have_first, me, standards);
          if (!D->error)
            D->n_meta++;
        }
      }
    } else if (strcmp(ip, "VERSION") == 0) {
      standards = atoi(in_cols[1]);

      /* Field Types -- here we go back to in_cols */

    } else if ((strcmp(in_cols[0], "INDEX") == 0) ||
        (strcmp(in_cols[0], "FILEFRAM") == 0))
    { /* reserved field names */
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_RES_NAME, format_file, linenum,
          NULL);
    } else
      _GD_ParseFieldSpec(D, n_cols, in_cols, NULL, subdir, format_file, linenum,
          have_first, me, standards);

    /* break out of loop (so we can return) if we've encountered an error */
    if (D->error != GD_E_OK)
      break;
  }

  dreturn("%i", have_first);
  return have_first;
}

/* attempt to open or create a new dirfile - set error appropriately */
static FILE* _GD_CreateDirfile(DIRFILE* D, const char* format_file,
    const char* filedir)
{
  struct stat statbuf;
  char fullname[FILENAME_MAX];
  DIR* dir;
  char* dirfile_end;
  struct dirent* lamb;
  int dir_error = 0;
  int format_error = 0;
  FILE* fp = NULL;

  dtrace("%p, \"%s\", \"%s\"", D, format_file, filedir);

  /* naively try to open the format file */
  if ((fp = fopen(format_file, "r")) == NULL) {
    format_error = errno;

    /* open failed, try to stat the directory itself */
    if (stat(filedir, &statbuf))
      dir_error = errno;
    else if (!S_ISDIR(statbuf.st_mode))
      dir_error = ENOTDIR;
  }

  /* First, cast out our four failure modes */

  /* unable to read the format file */
  if (format_error == EACCES || dir_error == EACCES) {
    _GD_SetError(D, GD_E_OPEN, GD_E_OPEN_NO_ACCESS, format_file, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  /* the directory exists, but it's not a dirfile, do nothing else -- even if we
   * were asked to truncate it */
  if (!dir_error && format_error) {
    _GD_SetError(D, GD_E_OPEN, GD_E_OPEN_NOT_DIRFILE, filedir, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  /* Couldn't open the file, and we weren't asked to create it */
  if (format_error && !(D->flags & GD_CREAT)) {
    _GD_SetError(D, GD_E_OPEN, GD_E_OPEN_NOT_EXIST, filedir, format_error,
        NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  /* It does exist, but we were asked to exclusively create it */
  if (!format_error && (D->flags & GD_CREAT) && (D->flags & GD_EXCL)) {
    _GD_SetError(D, GD_E_CREAT, GD_E_CREAT_EXCL, filedir, 0, NULL);
    fclose(fp);
    dreturn("%p", NULL);
    return NULL;
  }

  /* If we made it here we either:
   * 1) have no such directory, but plan to create it, or
   * 2) have a dirfile, which means the directory supplied contains a readable
   *   file called format */

  /* Truncate, if needed -- dangerous!  Truncating a dirfile deletes every
   * regular file in the specified directory.  It does not touch subdirectories.
   * Note that the rather lame definition of a dirfile at this point
   * (specifically, we haven't bothered to see if the format file is parsable)
   * could be problematic if users use GD_TRUNC cavalierly. */
  if (D->flags & GD_TRUNC && !format_error) {
    /* This file isn't going to be around much longer */
    fclose(fp);

    /* can't truncate a read-only dirfile */
    if ((D->flags & GD_ACCMODE) == GD_RDONLY) {
      _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
      dreturn("%p", NULL);
      return NULL;
    }

    /* This code is from defile */
    if ((dir = opendir(filedir)) == NULL) {
      _GD_SetError(D, GD_E_TRUNC, GD_E_TRUNC_DIR, filedir, errno, NULL);
      dreturn("%p", NULL);
      return NULL;
    }

    strcpy(fullname, filedir);
    dirfile_end = fullname + strlen(fullname);
    if (*(dirfile_end - 1) != '/') {
      strcat(fullname, "/");
      dirfile_end++;
    }

    while ((lamb = readdir(dir)) != NULL) {
      strcpy(dirfile_end, lamb->d_name);

      if (stat(fullname, &statbuf)) {
        _GD_SetError(D, GD_E_TRUNC, GD_E_TRUNC_STAT, fullname, errno, NULL);
        dreturn("%p", NULL);
        return NULL;
      }

      /* only delete regular files */
      if (S_ISREG(statbuf.st_mode)) {
        if (unlink(fullname)) {
          _GD_SetError(D, GD_E_TRUNC, GD_E_TRUNC_UNLINK, fullname, errno, NULL);
          dreturn("%p", NULL);
          return NULL;
        }
      }
    }
  }

  /* Create, if needed */
  if ((D->flags & GD_CREAT && format_error) || (D->flags & GD_TRUNC))
  {
    /* can't create a read-only dirfile */
    if ((D->flags & GD_ACCMODE) == GD_RDONLY) {
      _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
      dreturn("%p", NULL);
      return NULL;
    }

    /* attempt to create the dirfile directory, if not present */
    if (dir_error) 
      if (mkdir(filedir, 00777) < 0) {
        _GD_SetError(D, GD_E_CREAT, GD_E_CREAT_DIR, filedir, errno, NULL);
        dreturn("%p", NULL);
        return NULL;
      }

    /* create a new, empty format file */
    if ((fp = fopen(format_file, "wt")) == NULL) {
      _GD_SetError(D, GD_E_CREAT, GD_E_CREAT_FORMAT, format_file, errno, NULL);
      dreturn("%p", NULL);
      return NULL;
    }

    /* set GD_UNENCODED if GD_AUTO_ENCODED was specified */
    if ((D->flags & GD_ENCODING) == GD_AUTO_ENCODED)
      D->flags = (D->flags & ~GD_ENCODING) | GD_UNENCODED;
  }

  /* open succeeds */
  dreturn("%p", fp);
  return fp;
}

/* dirfile_open: open (or, perhaps, create) and parse the specified dirfile
*/
DIRFILE* dirfile_open(const char* filedir, unsigned int flags)
{
  FILE *fp;
  DIRFILE* D;
  char format_file[FILENAME_MAX];

  dtrace("\"%s\", 0%o", filedir, flags);

  D = malloc(sizeof(DIRFILE));
  _GD_ClearError(D);
  D->recurse_level = 0;

  D->entry = NULL;
  D->error_string = malloc(FILENAME_MAX);
  D->error_file = malloc(FILENAME_MAX);
  D->name = strdup(filedir);
  D->frame_offset = 0;
  D->n_entries = D->n_string = D->n_const = D->n_meta = 0;
  D->first_field = NULL;
  D->field_list = NULL;
  D->vector_list = NULL;
  D->const_list = NULL;
  D->string_list = NULL;
  D->string_value_list = NULL;
  D->const_value_list = NULL;
  D->list_validity = 0;
  D->flags = flags | GD_INVALID;
  D->n_include = 0;

  snprintf(format_file, FILENAME_MAX, "%s%sformat", filedir,
      (filedir[strlen(filedir) - 1] == '/') ? "" : "/");

  /* open the format file (or create it) */
  if ((fp = _GD_CreateDirfile(D, format_file, filedir)) == NULL) {
    dreturn("%p", D);
    return D; /* errors have already been set */
  }

  /* Parse the file.  This will take care of any necessary inclusions */
  D->n_include = 1;

  D->include_list = malloc(sizeof(struct gd_include_t));
  if (D->include_list == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", D);
    return D;
  }
  D->include_list[0].cname = strdup(format_file);
  /* The root format file needs no external name */
  D->include_list[0].ename = NULL;
  D->include_list[0].modified = 0;
  D->include_list[0].parent = -1;

  _GD_ParseFormatFile(fp, D, filedir, ".", format_file, 0,
      DIRFILE_STANDARDS_VERSION);
  fclose(fp);

  if (D->error != GD_E_OK) {
    dreturn("%p", D);
    return D;
  }

  /* Success! Clear invalid bit */
  if (D->error == GD_E_OK)
    D->flags &= ~GD_INVALID;

  dreturn("%p", D);
  return D;
}
/* vim: ts=2 sw=2 et tw=80
*/
