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
#include <math.h>
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

  if (field_code[0] == '\0' || len >= MAX_LINE_LENGTH) {
    dreturn("%p", field_code);
    return (char*)field_code;
  }

  for (i = 0; i < len; ++i)
    if (strchr("/<>;|&.", field_code[i]) != NULL) {
      dreturn("%p", field_code);
      return (char*)field_code;
    }

  if (strcmp("FRAMEOFFSET", field_code) == 0 ||
      strcmp("ENCODING", field_code) == 0 ||
      strcmp("ENDIAN", field_code) == 0 ||
      strcmp("INCLUDE", field_code) == 0 ||
      strcmp("META", field_code) == 0 ||
      strcmp("VERSION", field_code) == 0)
  {
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

static char* _GD_SetScalar(const char* token, void* data, int type)
{
  char* ptr;
  double d;

  dtrace("\"%s\", %p, %i", token, data, type);

  /* try to parse token as a double */
  d = strtod(token, &ptr); 

  /* there were trailing characters */
  if (*ptr != '\0') {
    ptr = strdup(token);
    dreturn("\"%s\"", ptr);
    return ptr;
  }

  if (type == GD_IEEE754)
    *(double*)data = d;
  else if (type == GD_SIGNED)
    *(int*)data = (int)round(d);
  else
    *(unsigned int*)data = (unsigned int)round(d);

  dreturn("%p", NULL);
  return NULL;
}

/* _GD_ParseRaw: parse a RAW data type in the format file
*/
static gd_entry_t* _GD_ParseRaw(DIRFILE* D, const char* in_cols[MAX_IN_COLS],
    int n_cols, const gd_entry_t* parent, int me, const char* format_file,
    int line)
{
  dtrace("%p, %p, %i, %p, %i, \"%s\", %i", D, in_cols, n_cols, parent, me,
      format_file, line);

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

  gd_entry_t* E = malloc(sizeof(gd_entry_t));
  if (E == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = malloc(sizeof(struct _gd_private_entry));
  if (E->e == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct _gd_private_entry));

  E->field_type = GD_RAW_ENTRY;
  E->e->file = NULL;
  E->e->fp = -1; /* file not opened yet */
  E->e->stream = NULL; /* file not opened yet */
  E->e->encoding = GD_ENC_UNKNOWN; /* don't know the encoding subscheme yet */

  E->field = _GD_ValidateField(NULL, in_cols[0]);
  if (E->field == in_cols[0]) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    E->field = NULL;
    _GD_FreeE(E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  E->e->file = malloc(FILENAME_MAX);
  if (E->e->file == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    E->field = NULL;
    _GD_FreeE(E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  snprintf((char*)E->e->file, FILENAME_MAX, "%s/%s/%s", D->name,
      D->include_list[me].sname, in_cols[0]);
  E->data_type = _GD_RawType(in_cols[2]);
  E->size = GD_SIZE(E->data_type);

  if (E->size == 0 || E->data_type & 0x40)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_TYPE, format_file, line,
        in_cols[2]);
  else if ((E->e->scalar[0] = _GD_SetScalar(in_cols[3], &E->spf, 0)) == NULL) {
    E->e->calculated = 1;
    if (E->spf <= 0)
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_SPF, format_file, line,
          in_cols[3]);
  }

  if (D->error != GD_E_OK) {
    _GD_FreeE(E, 1);
    E = NULL;
  }

  dreturn("%p", E);
  return E;
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

  gd_entry_t* E = malloc(sizeof(gd_entry_t));
  if (E == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = malloc(sizeof(struct _gd_private_entry));
  if (E->e == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct _gd_private_entry));

  E->field_type = GD_LINCOM_ENTRY;
  for (i = 0; i < GD_MAX_LINCOM; ++i) {
    E->in_fields[i] = NULL;
    E->e->entry[i] = NULL;
  }

  E->field = _GD_ValidateField(parent, in_cols[0]);
  if (E->field == in_cols[0]) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    E->field = NULL;
    _GD_FreeE(E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  E->n_fields = atoi(in_cols[2]);
  E->e->calculated = 1;

  if (E->field == NULL)
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
  else if ((E->n_fields < 1) || (E->n_fields > GD_MAX_LINCOM))
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_FIELDS, format_file, line,
        in_cols[2]);
  else if (n_cols < E->n_fields * 3 + 3)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS, format_file, line, NULL);
  else
    for (i = 0; i < E->n_fields; i++) {
      E->in_fields[i] = strdup(in_cols[i * 3 + 3]);
      if (E->in_fields[i] == NULL)
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);

      E->e->scalar[i * 2] = _GD_SetScalar(in_cols[i * 3 + 4], &E->m[i],
          GD_IEEE754);
      E->e->scalar[i * 2 + 1] = _GD_SetScalar(in_cols[i * 3 + 5], &E->b[i],
          GD_IEEE754);

      if (E->e->scalar[i * 2] != NULL || E->e->scalar[i * 2 + 1] != NULL)
        E->e->calculated = 0;
    }

  if (D->error != GD_E_OK) {
    _GD_FreeE(E, 1);
    E = NULL;
  }

  dreturn("%p", E);
  return E;
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

  gd_entry_t* E = malloc(sizeof(gd_entry_t));
  if (E == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = malloc(sizeof(struct _gd_private_entry));
  if (E->e == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct _gd_private_entry));

  E->field_type = GD_LINTERP_ENTRY;
  E->in_fields[0] = NULL;
  E->e->entry[0] = NULL;
  E->e->calculated = 1;
  E->table = NULL;

  E->field = _GD_ValidateField(parent, in_cols[0]);
  if (E->field == in_cols[0]) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    E->field = NULL;
    _GD_FreeE(E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  E->in_fields[0] = strdup(in_cols[2]);
  E->e->table_len = -1; /* linterp file not read yet */


  if (in_cols[3][0] == '/')
    E->table = strdup(in_cols[3]);
  else {
    /* non-absolute paths are relative to the format file's directory */
    E->table = malloc(FILENAME_MAX);
    if (E->table != NULL) {
      strcpy(temp_buffer, format_file);
      strcpy(E->table, dirname(temp_buffer));
      strcat(E->table, "/");
      strcat(E->table, in_cols[3]);
    }
  }

  if (E->field == NULL || E->in_fields[0] == NULL || E->table == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    _GD_FreeE(E, 1);
    E = NULL;
  }

  dreturn("%p", E);
  return E;
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

  gd_entry_t* E = malloc(sizeof(gd_entry_t));
  if (E == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = malloc(sizeof(struct _gd_private_entry));
  if (E->e == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct _gd_private_entry));

  E->field_type = GD_MULTIPLY_ENTRY;
  E->in_fields[0] = E->in_fields[1] = NULL;
  E->e->entry[0] = E->e->entry[1] = NULL;
  E->e->calculated = 1;

  E->field = _GD_ValidateField(parent, in_cols[0]);
  if (E->field == in_cols[0]) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    E->field = NULL;
    _GD_FreeE(E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  E->in_fields[0] = strdup(in_cols[2]);
  E->in_fields[1] = strdup(in_cols[3]);

  if (E->field == NULL || E->in_fields[0] == NULL || E->in_fields[1] == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    _GD_FreeE(E, 1);
    E = NULL;
  }

  dreturn("%p", E);
  return E;
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

  gd_entry_t* E = malloc(sizeof(gd_entry_t));
  if (E == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = malloc(sizeof(struct _gd_private_entry));
  if (E->e == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct _gd_private_entry));

  E->field_type = GD_BIT_ENTRY;
  E->in_fields[0] = NULL;
  E->e->entry[0] = NULL;
  E->e->calculated = 1;

  E->field = _GD_ValidateField(parent, in_cols[0]);
  if (E->field == in_cols[0]) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    E->field = NULL;
    _GD_FreeE(E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  E->in_fields[0] = strdup(in_cols[2]);

  if (E->field == NULL || E->in_fields[0] == NULL)
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);

  
  E->e->scalar[0] = _GD_SetScalar(in_cols[3], &E->bitnum, GD_SIGNED);
  if (n_cols > 4)
    E->e->scalar[1] = _GD_SetScalar(in_cols[4], &E->numbits, GD_SIGNED);
  else
    E->numbits = 1;

  if (E->e->scalar[0] != NULL || E->e->scalar[1] != NULL)
    E->e->calculated = 0;

  if (E->e->scalar[0] == NULL && E->numbits < 1)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_NUMBITS, format_file, line, NULL);
  else if (E->e->scalar[1] == NULL && E->bitnum < 0)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BITNUM, format_file, line, NULL);
  else if (E->e->calculated && E->bitnum + E->numbits - 1 > 63)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BITSIZE, format_file, line, NULL);

  if (D->error != GD_E_OK) {
    _GD_FreeE(E, 1);
    E = NULL;
  }

  dreturn("%p", E);
  return E;
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

  gd_entry_t* E = malloc(sizeof(gd_entry_t));
  if (E == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = malloc(sizeof(struct _gd_private_entry));
  if (E->e == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct _gd_private_entry));

  E->field_type = GD_PHASE_ENTRY;
  E->in_fields[0] = NULL;
  E->e->entry[0] = NULL;

  E->field = _GD_ValidateField(parent, in_cols[0]);
  if (E->field == in_cols[0]) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    E->field = NULL;
    _GD_FreeE(E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  E->in_fields[0] = strdup(in_cols[2]); /* field */

  if (E->field == NULL || E->in_fields[0] == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    _GD_FreeE(E, 1);
    E = NULL;
  }

  if ((E->e->scalar[0] = _GD_SetScalar(in_cols[3], &E->shift, GD_SIGNED)) ==
      NULL)
    E->e->calculated = 1;

  dreturn("%p", E);
  return E;
}

/* _GD_ParseConst: parse CONST data type entry in formats file.
*/
static gd_entry_t* _GD_ParseConst(DIRFILE* D, const char* in_cols[MAX_IN_COLS],
    int n_cols, const gd_entry_t* parent, const char* format_file, int line)
{
  dtrace("%p, %p, %i, %p, \"%s\", %i", D, in_cols, n_cols, parent, format_file,
      line);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS, format_file, line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  gd_entry_t* E = malloc(sizeof(gd_entry_t));
  if (E == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = malloc(sizeof(struct _gd_private_entry));
  if (E->e == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct _gd_private_entry));

  E->field_type = GD_CONST_ENTRY;
  E->e->calculated = 1;

  E->field = _GD_ValidateField(parent, in_cols[0]);
  if (E->field == in_cols[0]) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    E->field = NULL;
    _GD_FreeE(E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  if (E->field == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    _GD_FreeE(E, 1);
    E = NULL;
    dreturn("%p", NULL);
    return NULL;
  }

  E->const_type = _GD_RawType(in_cols[2]);

  if (GD_SIZE(E->const_type) == 0 || E->data_type & 0x40) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_TYPE, format_file, line,
        in_cols[2]);
    _GD_FreeE(E, 1);
    E = NULL;
    dreturn("%p", NULL);
    return NULL;
  }

  if (E->const_type & GD_SIGNED)
    sscanf(in_cols[3], "%" SCNi64, &E->e->iconst);
  else if (E->const_type & GD_IEEE754)
    sscanf(in_cols[3], "%lg", &E->e->dconst);
  else
    sscanf(in_cols[3], "%" SCNu64, &E->e->uconst);

  dreturn("%p", E);
  return E;
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

  gd_entry_t* E = malloc(sizeof(gd_entry_t));
  if (E == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = malloc(sizeof(struct _gd_private_entry));
  if (E->e == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct _gd_private_entry));

  E->field_type = GD_STRING_ENTRY;
  E->e->string = strdup(in_cols[2]);
  E->e->calculated = 1;

  E->field = _GD_ValidateField(parent, in_cols[0]);
  if (E->field == in_cols[0]) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    E->field = NULL;
    _GD_FreeE(E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  if (E->field == NULL || E->e->string == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    _GD_FreeE(E, 1);
    E = NULL;
    dreturn("%p", NULL);
    return NULL;
  }

  dreturn("%p", E);
  return E;
}

static int utf8encode(DIRFILE* D, const char* format_file, int linenum,
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
void _GD_ParseFieldSpec(DIRFILE* D, int n_cols, const char** in_cols,
    const gd_entry_t* P, const char* format_file, int linenum, int* have_first,
    unsigned int me, int standards, int creat, int pedantic)
{
  gd_entry_t* E = NULL;

  dtrace("%p, %i, %p, %p, \"%s\", %i, %p, %u, %i, %i, %i", D, n_cols, in_cols,
      P, format_file, linenum, have_first, me, standards, creat, pedantic);

  D->entry = realloc(D->entry, (D->n_entries + 1) * sizeof(gd_entry_t*));

  if (n_cols < 2)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS, format_file, linenum,
        NULL);
  else if (strcmp(in_cols[0], "INDEX") == 0) /* reserved field name */
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_RES_NAME, format_file, linenum,
        NULL);
  else if (strcmp(in_cols[1], "RAW") == 0) {
    E = _GD_ParseRaw(D, in_cols, n_cols, P, me, format_file, linenum);
    if (!D->error && D->first_field == NULL) {
      /* set the first field */
      D->first_field = malloc(sizeof(gd_entry_t));
      if (D->first_field == NULL)
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      else {
        memcpy(D->first_field, E, sizeof(gd_entry_t));
        *have_first = 1;
        D->include_list[me].first = 1;
      }
    }

    /* Create the binary file, if requested */
    if (!D->error && creat) {
      /* If the encoding scheme is unknown, we can't add the field */
      if ((D->include_list[me].flags & GD_ENCODING) == GD_AUTO_ENCODED) {
        _GD_SetError(D, GD_E_UNKNOWN_ENCODING, 0, NULL, 0, NULL);
      } else if ((D->include_list[me].flags & GD_ENCODING) ==
          GD_ENC_UNSUPPORTED)
      {
        /* If the encoding scheme is unsupported, we can't add the field */
        _GD_SetError(D, GD_E_UNSUPPORTED, 0, NULL, 0, NULL);
      } else {
        /* Set the subencoding subscheme */
        _GD_ResolveEncoding(E->e->file,
            D->include_list[me].flags, E->e);

        if (encode[E->e->encoding].touch == NULL) 
          _GD_SetError(D, GD_E_UNSUPPORTED, 0, NULL, 0, NULL);
        else if ((*encode[E->e->encoding].touch)(E->e->file))
          _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file, errno, NULL);
      }
    }
  } else if (strcmp(in_cols[1], "LINCOM") == 0)
    E = _GD_ParseLincom(D, in_cols, n_cols, P, format_file, linenum);
  else if (strcmp(in_cols[1], "LINTERP") == 0)
    E = _GD_ParseLinterp(D, in_cols, n_cols, P, format_file, linenum);
  else if (strcmp(in_cols[1], "MULTIPLY") == 0)
    E = _GD_ParseMultiply(D, in_cols, n_cols, P, format_file, linenum);
  else if (strcmp(in_cols[1], "BIT") == 0)
    E = _GD_ParseBit(D, in_cols, n_cols, P, format_file, linenum);
  else if (strcmp(in_cols[1], "PHASE") == 0)
    E = _GD_ParsePhase(D, in_cols, n_cols, P, format_file, linenum);
  else if (strcmp(in_cols[1], "CONST") == 0) {
    E = _GD_ParseConst(D, in_cols, n_cols, P, format_file, linenum);
    if (D->error == GD_E_OK && P == NULL)
      D->n_const++;
  } else if (strcmp(in_cols[1], "STRING") == 0) {
    E = _GD_ParseString(D, in_cols, n_cols, P, format_file, linenum);
    if (D->error == GD_E_OK && P == NULL)
      D->n_string++;
  } else if (standards <= DIRFILE_STANDARDS_VERSION || pedantic)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_LINE, format_file, linenum,
        NULL);

  if (D->error == GD_E_OK && E != NULL) {
    /* the Format file fragment index */
    E->fragment_index = me;

    /* Check for duplicate */
    int u;
    const gd_entry_t* Q = _GD_FindField(D, E->field, &u);

    if (Q) {
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_DUPLICATE, format_file, linenum,
          D->include_list[Q->fragment_index].cname);
      dreturnvoid();
      return;
    } 

    /* sort */
    _GD_InsertSort(D, E, u);
    D->n_entries++;

    /* Initialse the meta counts */
    if (P != NULL) {
      E->e->n_meta = -1;
      /* there is no need to sort this list */
      P->e->meta_entry = realloc(P->e->meta_entry, (P->e->n_meta + 1) *
          sizeof(gd_entry_t*));
      P->e->meta_entry[P->e->n_meta++] = E;

      D->n_meta++;
      if (E->field_type == GD_CONST_ENTRY)
        P->e->n_meta_const++;
      else if (E->field_type == GD_STRING_ENTRY)
        P->e->n_meta_string++;
    }
  }

  dreturnvoid();
}

/* _GD_Tokenise: Tokenise a line.  Returns n_cols. */
#define ACC_MODE_NONE  0
#define ACC_MODE_OCTAL 1
#define ACC_MODE_HEX   2
#define ACC_MODE_UTF8  3
int _GD_Tokenise(DIRFILE *D, const char* instring, char* outstring,
    const char** in_cols, const char* format_file, int linenum)
{
  const char* ip;
  char* op = outstring;
  int n_cols = 0;
  int escaped_char = 0;
  int quotated = 0;
  int ws = 1;
  uint32_t accumulator = 0;
  int n_acc = 0;
  int acc_mode = ACC_MODE_NONE;

  dtrace("%p, \"%s\", %p, %p, \"%s\", %i", D, instring, outstring, in_cols,
      format_file, linenum);

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
            *(op++) = '\f';
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
      else if (*ip == '"') {
        if ((quotated = !quotated)) {
          if (ws) {
            if (n_cols >= MAX_IN_COLS)
              break; /* Ignore trailing data on the line */
            in_cols[n_cols++] = op;
            ws = 0;
          }
        }
      } else if (!quotated && isspace(*ip)) {
        if (!ws) {
          *(op++) = '\0';
          ws = 1;
        }
      } else if (!quotated && *ip == '#') {
        *op = '\0';
        break;
      } else {
        if (ws) {
          if (n_cols >= MAX_IN_COLS)
            break; /* Ignore trailing data on the line */
          in_cols[n_cols++] = op;
          ws = 0;
        }
        *(op++) = *ip;
      }
    }
  }

  if (quotated || escaped_char) /* Unterminated token */
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_UNTERM, format_file, linenum,
        NULL);

  dreturn("%i", n_cols);
  return n_cols;
}

/* _GD_ParseDirective: Actually parse a single format file line.
 *       Returns 1 if a match was made.
 */
static int _GD_ParseDirective(DIRFILE *D, const char** in_cols, int n_cols,
    int me, int* standards, int linenum, int* have_first, unsigned int flags)
{
  const char* ptr;

  dtrace("%p, %p, %i, %u, %p, %i, %p", D, in_cols, n_cols, me, standards,
      linenum, have_first);

  /* set up for possibly slashed reserved words */
  ptr = (char*)in_cols[0] + ((in_cols[0][0] == '/') ? 1 : 0);

  if (n_cols < 2) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS, D->include_list[me].cname,
        linenum, NULL);
  } else if (strcmp(ptr, "FRAMEOFFSET") == 0) {
    D->frame_offset = atoll(in_cols[1]);
  } else if (strcmp(ptr, "INCLUDE") == 0)
    _GD_Include(D, in_cols[1], D->include_list[me].cname, linenum, me,
        standards, D->include_list[me].flags);
  else if (strcmp(ptr, "ENCODING") == 0) {
    if (!(flags & GD_FORCE_ENCODING)) {
      if (strcmp(in_cols[1], "none") == 0)
        D->include_list[me].flags = (D->include_list[me].flags & ~GD_ENCODING) |
          GD_UNENCODED;
      else if (strcmp(in_cols[1], "slim") == 0)
        D->include_list[me].flags = (D->include_list[me].flags & ~GD_ENCODING) |
          GD_SLIM_ENCODED;
      else if (strcmp(in_cols[1], "text") == 0)
        D->include_list[me].flags = (D->include_list[me].flags & ~GD_ENCODING) |
          GD_TEXT_ENCODED;
      else
        D->include_list[me].flags = (D->include_list[me].flags & ~GD_ENCODING) |
          GD_ENC_UNSUPPORTED;
    }
  } else if (strcmp(ptr, "ENDIAN") == 0) {
    if (!(flags & GD_FORCE_ENDIAN)) {
      if (strcmp(in_cols[1], "big") == 0) {
        D->include_list[me].flags |= GD_BIG_ENDIAN;
        D->include_list[me].flags &= ~GD_LITTLE_ENDIAN;
      } else if (strcmp(in_cols[1], "little") == 0) {
        D->include_list[me].flags |= GD_LITTLE_ENDIAN;
        D->include_list[me].flags &= ~GD_BIG_ENDIAN;
      } else 
        _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_ENDIAN,
            D->include_list[me].cname, linenum, NULL);
    }
  } else if (strcmp(ptr, "META") == 0) {
    const gd_entry_t* P =  _GD_FindField(D, in_cols[1], NULL);
    if (P == NULL)
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_NO_PARENT,
          D->include_list[me].cname, linenum, in_cols[1]);
    else if (P->fragment_index != me)
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_LOCATION,
          D->include_list[me].cname, linenum, in_cols[1]);
    else if (n_cols < 4)
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_COLS,
          D->include_list[me].cname, linenum, NULL);
    else
      _GD_ParseFieldSpec(D, n_cols - 2, in_cols + 2, P,
          D->include_list[me].cname, linenum, have_first, me, *standards, 0,
          flags & GD_PEDANTIC);
  } else if (strcmp(ptr, "VERSION") == 0)
    *standards = atoi(in_cols[1]);
  else {
    dreturn("%i", 0);
    return 0;
  }

  dreturn("%i", 1);
  return 1;
}

/* _GD_ParseFormatFile: Parse each line of the format file.  This
 *       function is called from GetFormat once for the main format file and
 *       once for each included file.
 *
 *       Returns 0 unless this format file contains the first raw field.
 */
int _GD_ParseFormatFile(FILE* fp, DIRFILE *D, int me, int* standards,
    unsigned int flags)
{
  char instring[MAX_LINE_LENGTH];
  char outstring[MAX_LINE_LENGTH];
  const char *in_cols[MAX_IN_COLS];
  int linenum = 0;
  int have_first = 0;
  int n_cols;
  int match;

  dtrace("%p, %p, %i, %p", fp, D, me, standards);

  /* start parsing */
  while (_GD_GetLine(fp, instring, &linenum)) {
    n_cols = _GD_Tokenise(D, instring, outstring, in_cols,
        D->include_list[me].cname, linenum);

    if (D->error)
      break; /* tokeniser threw an error */

    match = _GD_ParseDirective(D, in_cols, n_cols, me, standards, linenum,
        &have_first, flags);

    if (D->error)
      break; /* directive parser threw an error */

    if (!match)
      _GD_ParseFieldSpec(D, n_cols, in_cols, NULL, D->include_list[me].cname,
          linenum, &have_first, me, *standards, 0, flags & GD_PEDANTIC);

    if (D->error)
      break; /* field spec parser threw an error */
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
  int standards = DIRFILE_STANDARDS_VERSION;

  dtrace("\"%s\", 0x%x", filedir, flags);

  D = malloc(sizeof(DIRFILE));
  memset(D, 0, sizeof(DIRFILE));

  D->error_string = malloc(FILENAME_MAX);
  D->error_file = malloc(FILENAME_MAX);
  D->name = strdup(filedir);
  D->flags = flags | GD_INVALID;

  /* Add the INDEX entry */
  D->n_entries = 1;

  D->entry = malloc(sizeof(gd_entry_t*));
  if (D->entry == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", D);
    return D;
  }

  D->entry[0] = malloc(sizeof(gd_entry_t));
  if (D->entry == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", D);
    return D;
  }
  memset(D->entry[0], 0, sizeof(gd_entry_t));

  D->entry[0]->field_type = GD_INDEX_ENTRY;
  D->entry[0]->e = malloc(sizeof(struct _gd_private_entry));
  D->entry[0]->field = strdup("INDEX");
  if (D->entry[0]->field == NULL || D->entry[0]->e == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", D);
    return D;
  }
  memset(D->entry[0]->e, 0, sizeof(struct _gd_private_entry));
  D->entry[0]->e->calculated = 1;

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
  D->include_list[0].sname = strdup(".");
  /* The root format file needs no external name */
  D->include_list[0].ename = NULL;
  D->include_list[0].modified = 0;
  D->include_list[0].parent = -1;
  D->include_list[0].flags = D->flags & (GD_ENCODING | GD_LITTLE_ENDIAN |
      GD_BIG_ENDIAN);

  _GD_ParseFormatFile(fp, D, 0, &standards, D->flags);
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
