/* (C) 2002-2005 C. Barth Netterfield
 * (C) 2005-2009 D. V. Wiebe
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
  if (strcmp(type, "COMPLEX64") == 0)
    return GD_COMPLEX64;
  if (strcmp(type, "COMPLEX128") == 0)
    return GD_COMPLEX128;

  return GD_UNKNOWN;
}

static char* _GD_SetScalar(DIRFILE* D, const char* token, void* data, int type,
    const char* format_file, int line, int *comp_scal)
{
  char *ptr;
  const char *semicolon;
  double d;
  double i = 0;

  dtrace("%p, \"%s\", %p, %i, \"%s\", %i, %p", D, token, data, type,
      format_file, line, comp_scal);

  /* try to convert to double */
  d = strtod(token, &ptr); 

  /* check for a complex value -- look for the semicolon */
  for (semicolon = token; *semicolon; ++semicolon)
    if (*semicolon == ';')
      break;

  /* there were trailing characters in the double or real part of complex */
  if (ptr != semicolon) {
    ptr = strdup(token);
    dreturn("\"%s\"", ptr);
    return ptr;
  }

  /* If there was a semicolon, try to extract the imaginary part */
  if (*semicolon == ';') {
    i = strtod(semicolon + 1, &ptr);

    /* there were trailing characters in the imaginary part of complex */
    if (*ptr != '\0') {
      ptr = strdup(token);
      dreturn("\"%s\"", ptr);
      return ptr;
    }

    /* if a complex value is not permitted, complain */
    if (type != GD_COMPLEX) {
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_LITTERAL, format_file, line,
          token);
      dreturn("%p", NULL);
      return NULL;
    }

    *comp_scal = 1;
  }

  if (type == GD_INT64)
    *(int64_t*)data = (int64_t)round(d);
  else if (type == GD_COMPLEX)
    *(double complex*)data = d + _Complex_I * i;
  else if (type == GD_IEEE754)
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
static gd_entry_t* _GD_ParseRaw(DIRFILE* D, char* in_cols[MAX_IN_COLS],
    int n_cols, const gd_entry_t* parent, int me, const char* format_file,
    int line, int pedantic)
{
  dtrace("%p, %p, %i, %p, %i, \"%s\", %i, %i", D, in_cols, n_cols, parent, me,
      format_file, line, pedantic);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, format_file, line, NULL);
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
  E->e->file[0].fp = E->e->file[1].fp = -1; /* file not opened yet */
  E->e->file[0].encoding = GD_ENC_UNKNOWN; /* don't know the encoding
                                              subscheme yet */

  E->field = _GD_ValidateField(NULL, in_cols[0], pedantic);
  if (E->field == in_cols[0]) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    E->field = NULL;
    _GD_FreeE(E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  E->e->filebase = malloc(FILENAME_MAX);
  if (E->e->filebase == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    _GD_FreeE(E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  if (D->fragment[me].sname)
    snprintf(E->e->filebase, FILENAME_MAX, "%s/%s/%s", D->name,
        D->fragment[me].sname, in_cols[0]);
  else
    snprintf(E->e->filebase, FILENAME_MAX, "%s/%s", D->name, in_cols[0]);

  E->data_type = _GD_RawType(in_cols[2]);
  E->e->size = GD_SIZE(E->data_type);

  if (E->e->size == 0 || E->data_type & 0x40)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_TYPE, format_file, line,
        in_cols[2]);
  else if ((E->scalar[0] = _GD_SetScalar(D, in_cols[3], &E->spf, 0, format_file,
          line, NULL)) == NULL)
  {
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
static gd_entry_t* _GD_ParseLincom(DIRFILE* D, char* in_cols[MAX_IN_COLS],
    int n_cols, const gd_entry_t* parent, const char* format_file, int line,
    int pedantic)
{
  int i;
  char* ptr = NULL;

  dtrace("%p, %p, %i, %p, \"%s\", %i, %i", D, in_cols, n_cols, parent,
      format_file, line, pedantic);

  if (n_cols < 3) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, format_file, line, NULL);
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

  E->field = _GD_ValidateField(parent, in_cols[0], pedantic);
  if (E->field == in_cols[0]) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    E->field = NULL;
    _GD_FreeE(E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  E->e->calculated = 1;
  E->n_fields = (int)(strtol(in_cols[2], &ptr, 10));
  if (*ptr != '\0') {
    E->n_fields = (n_cols - 2) / 3;
    /* assume <n> has been omitted */
    if (n_cols % 3 != 2 || E->n_fields < 1 || E->n_fields > GD_MAX_LINCOM) {
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, format_file, line, NULL);
      _GD_FreeE(E, 1);
      dreturn("%p", NULL);
      return NULL;
    }
    /* the following two statements are somewhat hacky.... */
    n_cols++;
    in_cols--;
  }

  if (E->field == NULL)
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
  else if ((E->n_fields < 1) || (E->n_fields > GD_MAX_LINCOM))
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_FIELDS, format_file, line,
        in_cols[2]);
  else if (n_cols < E->n_fields * 3 + 3)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, format_file, line, NULL);
  else
    for (i = 0; i < E->n_fields; i++) {
      E->in_fields[i] = strdup(in_cols[i * 3 + 3]);
      if (E->in_fields[i] == NULL)
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);

      E->scalar[i] = _GD_SetScalar(D, in_cols[i * 3 + 4], &E->cm[i],
          GD_COMPLEX, format_file, line, &E->comp_scal);
      E->m[i] = creal(E->cm[i]);
      E->scalar[i + GD_MAX_LINCOM] = _GD_SetScalar(D, in_cols[i * 3 + 5],
          &E->cb[i], GD_COMPLEX, format_file, line, &E->comp_scal);
      E->b[i] = creal(E->cb[i]);

      if (E->scalar[i] != NULL || E->scalar[i + GD_MAX_LINCOM] != NULL)
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
    char* in_cols[MAX_IN_COLS], int n_cols, const gd_entry_t* parent,
    const char* format_file, int line, int pedantic)
{
  dtrace("%p, %p, %i, %p, \"%s\", %i, %i", D, in_cols, n_cols, parent,
      format_file, line, pedantic);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, format_file, line, NULL);
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

  E->field = _GD_ValidateField(parent, in_cols[0], pedantic);
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


  E->table = strdup(in_cols[3]);

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
    char* in_cols[MAX_IN_COLS], int n_cols, const gd_entry_t* parent,
    const char* format_file, int line, int pedantic)
{
  dtrace("%p, %p, %i, %p, \"%s\", %i, %i", D, in_cols, n_cols, parent,
      format_file, line, pedantic);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, format_file, line, NULL);
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

  E->field = _GD_ValidateField(parent, in_cols[0], pedantic);
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
static gd_entry_t* _GD_ParseBit(DIRFILE* D, int is_signed,
    char* in_cols[MAX_IN_COLS], int n_cols, const gd_entry_t* parent,
    const char* format_file, int line, int pedantic)
{
  dtrace("%p, %i, %p, %i, %p, \"%s\", %i, %i", D, is_signed, in_cols, n_cols,
      parent, format_file, line, pedantic);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, format_file, line, NULL);
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

  E->field_type = (is_signed) ? GD_SBIT_ENTRY : GD_BIT_ENTRY;
  E->in_fields[0] = NULL;
  E->e->entry[0] = NULL;
  E->e->calculated = 1;

  E->field = _GD_ValidateField(parent, in_cols[0], pedantic);
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


  E->scalar[0] = _GD_SetScalar(D, in_cols[3], &E->bitnum, GD_SIGNED,
      format_file, line, NULL);
  if (n_cols > 4)
    E->scalar[1] = _GD_SetScalar(D, in_cols[4], &E->numbits, GD_SIGNED,
        format_file, line, NULL);
  else
    E->numbits = 1;

  if (E->scalar[0] != NULL || E->scalar[1] != NULL)
    E->e->calculated = 0;

  if (E->scalar[1] == NULL && E->numbits < 1)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_NUMBITS, format_file, line, NULL);
  else if (E->scalar[0] == NULL && E->bitnum < 0)
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
static gd_entry_t* _GD_ParsePhase(DIRFILE* D, char* in_cols[MAX_IN_COLS],
    int n_cols, const gd_entry_t* parent, const char* format_file, int line,
    int pedantic)
{
  dtrace("%p, %p, %i, %p, \"%s\", %i, %i", D, in_cols, n_cols, parent,
      format_file, line, pedantic);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, format_file, line, NULL);
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

  E->field = _GD_ValidateField(parent, in_cols[0], pedantic);
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

  if ((E->scalar[0] = _GD_SetScalar(D, in_cols[3], &E->shift, GD_INT64,
          format_file, line, NULL)) == NULL)
    E->e->calculated = 1;

  dreturn("%p", E);
  return E;
}

/* _GD_ParsePolynom: parse a POLYNOM data type in the format file.
*/
static gd_entry_t* _GD_ParsePolynom(DIRFILE* D,
    char* in_cols[MAX_IN_COLS], int n_cols, const gd_entry_t* parent,
    const char* format_file, int line, int pedantic)
{
  int i;

  dtrace("%p, %p, %i, %p, \"%s\", %i, %i", D, in_cols, n_cols, parent,
      format_file, line, pedantic);

  if (n_cols < 5) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, format_file, line, NULL);
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

  E->field_type = GD_POLYNOM_ENTRY;
  E->field = _GD_ValidateField(parent, in_cols[0], pedantic);
  if (E->field == in_cols[0]) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    E->field = NULL;
    _GD_FreeE(E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  E->poly_ord = n_cols - 4;

  /* the legacy ignore-trailing-tokens "feature" */
  if (E->poly_ord > GD_MAX_POLYORD)
    E->poly_ord = GD_MAX_POLYORD;

  E->e->calculated = 1;

  if (E->field == NULL)
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
  else {
    E->in_fields[0] = strdup(in_cols[2]);
    if (E->in_fields[0] == NULL)
      _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    else
      for (i = 0; i <= E->poly_ord; i++) {
        E->scalar[i] = _GD_SetScalar(D, in_cols[i + 3], &E->ca[i],
            GD_COMPLEX, format_file, line, &E->comp_scal);
        E->a[i] = creal(E->ca[i]);

        if (E->scalar[i] != NULL)
          E->e->calculated = 0;
      }
  }

  if (D->error != GD_E_OK) {
    _GD_FreeE(E, 1);
    E = NULL;
  }

  dreturn("%p", E);
  return E;
}

/* _GD_ParseConst: parse CONST data type entry in formats file.
*/
static gd_entry_t* _GD_ParseConst(DIRFILE* D, char* in_cols[MAX_IN_COLS],
    int n_cols, const gd_entry_t* parent, const char* format_file, int line,
    int pedantic)
{
  int dummy;
  char* ptr;

  dtrace("%p, %p, %i, %p, \"%s\", %i, %i", D, in_cols, n_cols, parent,
      format_file, line, pedantic);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, format_file, line, NULL);
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

  E->field = _GD_ValidateField(parent, in_cols[0], pedantic);
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

  if (E->const_type & GD_COMPLEX)
    ptr = _GD_SetScalar(D, in_cols[3], &E->e->cconst, GD_COMPLEX, format_file,
        line, &dummy);
  else if (E->const_type & GD_IEEE754)
    ptr = _GD_SetScalar(D, in_cols[3], &E->e->dconst, GD_IEEE754, format_file,
        line, NULL);
  else if (E->const_type & GD_SIGNED)
    ptr = _GD_SetScalar(D, in_cols[3], &E->e->iconst, GD_SIGNED, format_file,
        line, NULL);
  else
    ptr = _GD_SetScalar(D, in_cols[3], &E->e->uconst, 0, format_file, line,
        NULL);

  if (ptr)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_LITTERAL, format_file, line,
        in_cols[3]);

  if (D->error) {
    _GD_FreeE(E, 1);
    E = NULL;
  }

  dreturn("%p", E);
  return E;
}

/* _GD_ParseString: parse STRING data type entry in formats file.
*/
static gd_entry_t* _GD_ParseString(DIRFILE* D, char *in_cols[MAX_IN_COLS],
    int n_cols, const gd_entry_t* parent, const char* format_file, int line,
    int pedantic)
{
  dtrace("%p, %p, %i, %p, \"%s\", %i, %i", D, in_cols, n_cols, parent,
      format_file, line, pedantic);

  if (n_cols < 3) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, format_file, line, NULL);
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

  E->field = _GD_ValidateField(parent, in_cols[0], pedantic);
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
    *((*op)++) = (char)value;
  else if (value <= 0x7FF) {
    *((*op)++) = (char)(0xC0 + (value >> 6));
    *((*op)++) = (char)(0x80 + (value & 0x3F));
  } else if (value <= 0xFFFF) {
    *((*op)++) = (char)(0xE0 + (value >> 12));
    *((*op)++) = (char)(0x80 + ((value >> 6) & 0x3F));
    *((*op)++) = (char)(0x80 + (value & 0x3F));
  } else {
    *((*op)++) = (char)(0xF0 + (value >> 18));
    *((*op)++) = (char)(0x80 + ((value >> 12) & 0x3F));
    *((*op)++) = (char)(0x80 + ((value >> 6) & 0x3F));
    *((*op)++) = (char)(0x80 + (value & 0x3F));
  }

  dreturn("%i", 0);
  return 0;
}

/* _GD_ParseFieldSpec: Parse a format file line fragment containing a field
 * specification */
gd_entry_t* _GD_ParseFieldSpec(DIRFILE* D, int n_cols, char** in_cols,
    const gd_entry_t* P, const char* format_file, int linenum, int me,
    int standards, int creat, int pedantic, int insert)
{
  gd_entry_t* E = NULL;
  void *ptr;
  char *cptr;

  dtrace("%p, %i, %p, %p, \"%s\", %i, %u, %i, %i, %i, %i", D, n_cols, in_cols,
      P, format_file, linenum, me, standards, creat, pedantic, insert);

  /* Check for barth-style metafield definition */
  if (P == NULL)
    for (cptr = in_cols[0] + 1; *cptr != '\0'; ++cptr)
      if (*cptr == '/') {
        *cptr = '\0';
        const gd_entry_t* P = _GD_FindField(D, in_cols[0], NULL);
        if (P == NULL)
          _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_NO_PARENT,
              D->fragment[me].cname, linenum, in_cols[0]);
        else if (P->fragment_index != me)
          _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_LOCATION,
              D->fragment[me].cname, linenum, in_cols[0]);
        else if (n_cols < 2)
          _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, D->fragment[me].cname,
              linenum, NULL);
        else {
          /* temporarily point in_cols[0] to the metafield name */
          ptr = in_cols[0];
          in_cols[0] = cptr + 1;
          E = _GD_ParseFieldSpec(D, n_cols, in_cols, P, D->fragment[me].cname,
              linenum, me, standards, creat, pedantic, insert);
          /* restore in_cols[0] */
          in_cols[0] = ptr;
        }
        dreturn("%p", (!insert) ? E : NULL);
        return (!insert) ? E : NULL;
      }


  ptr = realloc(D->entry, (D->n_entries + 1) * sizeof(gd_entry_t*));
  if (ptr == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn ("%p", NULL);
    return NULL;
  }
  D->entry = ptr;

  if (n_cols < 2)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, format_file, linenum,
        NULL);
  else if (P == NULL && strcmp(in_cols[0], "INDEX") == 0)
    /* reserved field name */
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_RES_NAME, format_file, linenum,
        NULL);
  else if (strcmp(in_cols[1], "RAW") == 0) {
    E = _GD_ParseRaw(D, in_cols, n_cols, P, me, format_file, linenum, pedantic);

    /* Create the binary file, if requested */
    if (!D->error && creat) {
      /* If this fragment is protected, we can't do anything */
      if (D->fragment[me].protection != GD_PROTECT_NONE)
        _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_DATA, NULL, 0,
            D->fragment[me].cname);
      /* If the encoding scheme is unknown, we can't add the field */
      if (D->fragment[me].encoding == GD_AUTO_ENCODED)
        _GD_SetError(D, GD_E_UNKNOWN_ENCODING, 0, NULL, 0, NULL);
      else if (D->fragment[me].encoding == GD_ENC_UNSUPPORTED)
        /* If the encoding scheme is unsupported, we can't add the field */
        _GD_SetError(D, GD_E_UNSUPPORTED, 0, NULL, 0, NULL);
      else if (!_GD_Supports(D, E, GD_EF_TOUCH))
        ; /* error already set */
      else if (_GD_SetEncodedName(D, E->e->file, E->e->filebase, 0))
        ; /* error already set */
      else if ((*_gd_ef[E->e->file[0].encoding].touch)(E->e->file))
        _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[0].name, errno, NULL);
    }

    /* Is this the first raw field ever defined? */
    if (!D->error && D->fragment[E->fragment_index].ref_name == NULL)
      if (D->reference_field == NULL)
        D->reference_field = E;
  } else if (strcmp(in_cols[1], "LINCOM") == 0)
    E = _GD_ParseLincom(D, in_cols, n_cols, P, format_file, linenum, pedantic);
  else if (strcmp(in_cols[1], "LINTERP") == 0)
    E = _GD_ParseLinterp(D, in_cols, n_cols, P, format_file, linenum, pedantic);
  else if (strcmp(in_cols[1], "MULTIPLY") == 0)
    E = _GD_ParseMultiply(D, in_cols, n_cols, P, format_file, linenum,
        pedantic);
  else if (strcmp(in_cols[1], "BIT") == 0)
    E = _GD_ParseBit(D, 0, in_cols, n_cols, P, format_file, linenum, pedantic);
  else if (strcmp(in_cols[1], "PHASE") == 0)
    E = _GD_ParsePhase(D, in_cols, n_cols, P, format_file, linenum, pedantic);
  else if (strcmp(in_cols[1], "POLYNOM") == 0)
    E = _GD_ParsePolynom(D, in_cols, n_cols, P, format_file, linenum, pedantic);
  else if (strcmp(in_cols[1], "SBIT") == 0)
    E = _GD_ParseBit(D, 1, in_cols, n_cols, P, format_file, linenum, pedantic);
  else if (strcmp(in_cols[1], "CONST") == 0) {
    E = _GD_ParseConst(D, in_cols, n_cols, P, format_file, linenum, pedantic);
    if (D->error == GD_E_OK && P == NULL)
      D->n_const++;
  } else if (strcmp(in_cols[1], "STRING") == 0) {
    E = _GD_ParseString(D, in_cols, n_cols, P, format_file, linenum, pedantic);
    if (D->error == GD_E_OK && P == NULL)
      D->n_string++;
  } else if (standards <= DIRFILE_STANDARDS_VERSION || pedantic)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_LINE, format_file, linenum,
        NULL);

  if (insert && D->error == GD_E_OK && E != NULL) {
    /* the Format file fragment index */
    E->fragment_index = me;

    /* Check for duplicate */
    unsigned int u;
    const gd_entry_t* Q = _GD_FindField(D, E->field, &u);

    if (Q) {
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_DUPLICATE, format_file, linenum,
          D->fragment[Q->fragment_index].cname);
      dreturn("%p", NULL);
      return NULL;
    } 

    /* Initialse the meta counts */
    if (P != NULL) {
      E->e->n_meta = -1;
      E->e->parent = P;
      /* there is no need to sort this list */
      ptr = realloc(P->e->meta_entry, (P->e->n_meta + 1) * sizeof(gd_entry_t*));
      if (ptr == NULL) {
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
        dreturn ("%p", NULL);
        return NULL;
      }
      P->e->meta_entry = ptr;
      P->e->meta_entry[P->e->n_meta++] = E;

      D->n_meta++;
      if (E->field_type == GD_CONST_ENTRY)
        P->e->n_meta_const++;
      else if (E->field_type == GD_STRING_ENTRY)
        P->e->n_meta_string++;
    }

    /* sort */
    _GD_InsertSort(D, E, u);
    D->n_entries++;
  }

  /* return the entry object if we either:
   * - didn't insert the object into the list of fields (ie. we were called
   *   by [m]alter_spec
   * - found a RAW field (which might be the reference field)
   */
  dreturn("%p", (!insert || (E && E->field_type == GD_RAW_ENTRY)) ? E : NULL);
  return (!insert || (E && E->field_type == GD_RAW_ENTRY)) ? E : NULL;
}

/* _GD_Tokenise: Tokenise a line.  Returns n_cols.  This is a simple state
 * machine. */
#define ACC_MODE_NONE  0
#define ACC_MODE_OCTAL 1
#define ACC_MODE_HEX   2
#define ACC_MODE_UTF8  3
int _GD_Tokenise(DIRFILE *D, const char* instring, char* outstring,
    char** in_cols, const char* format_file, int linenum)
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

          *(op++) = (char)accumulator;

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

          *(op++) = (char)accumulator;

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
          case '\n': /* this is a backslach at the end of a line -- we don't
                        support line splicing in this manner */
            break;
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
      } else if (!quotated && *ip <= ' ' && (*ip == ' ' || *ip == '\n' ||
            *ip == '\t' || *ip == '\r' || *ip == '\f' || *ip == '\v'))
      {
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
  *op = '\0';

  if (quotated || escaped_char) /* Unterminated token */
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_UNTERM, format_file, linenum,
        NULL);

  dreturn("%i", n_cols);
  return n_cols;
}

/* _GD_ParseDirective: Actually parse a single format file line.
 *       Returns 1 if a match was made.
 */
static int _GD_ParseDirective(DIRFILE *D, char** in_cols, int n_cols,
    int me, int* standards, int linenum, char** ref_name, int* first_fragment,
    unsigned long flags)
{
  const char* ptr;
  int i;

  dtrace("%p, %p, %i, %u, %p, %i, %p, %p, %lx", D, in_cols, n_cols, me,
      standards, linenum, ref_name, first_fragment, flags);

  /* set up for possibly slashed reserved words */
  ptr = in_cols[0] + ((in_cols[0][0] == '/') ? 1 : 0);

  if (n_cols < 2)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, D->fragment[me].cname,
        linenum, NULL);
  else if (strcmp(ptr, "ENCODING") == 0) {
    if (!(flags & GD_FORCE_ENCODING)) {
      D->fragment[me].encoding = GD_ENC_UNSUPPORTED;
      for (i = 0; i < GD_N_SUBENCODINGS - 1; ++i)
        if (strcmp(in_cols[1], _gd_ef[i].ffname) == 0) {
          D->fragment[me].encoding = _gd_ef[i].scheme;
          break;
        }
    }
  } else if (strcmp(ptr, "ENDIAN") == 0) {
    if (!(flags & GD_FORCE_ENDIAN)) {
      if (strcmp(in_cols[1], "big") == 0)
        D->fragment[me].byte_sex = GD_BIG_ENDIAN;
      else if (strcmp(in_cols[1], "little") == 0)
        D->fragment[me].byte_sex = GD_LITTLE_ENDIAN;
      else 
        _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_ENDIAN,
            D->fragment[me].cname, linenum, NULL);
    }
  } else if (strcmp(ptr, "FRAMEOFFSET") == 0)
    D->fragment[me].frame_offset = atoll(in_cols[1]);
  else if (strcmp(ptr, "INCLUDE") == 0) {
    *first_fragment = _GD_Include(D, in_cols[1], D->fragment[me].cname, linenum,
        ref_name, me, standards, D->fragment[me].encoding |
        D->fragment[me].byte_sex);
  } else if (strcmp(ptr, "META") == 0) {
    const gd_entry_t* P =  _GD_FindField(D, in_cols[1], NULL);
    if (P == NULL)
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_NO_PARENT,
          D->fragment[me].cname, linenum, in_cols[1]);
    else if (P->fragment_index != me)
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_LOCATION,
          D->fragment[me].cname, linenum, in_cols[1]);
    else if (n_cols < 4)
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK,
          D->fragment[me].cname, linenum, NULL);
    else
      _GD_ParseFieldSpec(D, n_cols - 2, in_cols + 2, P,
          D->fragment[me].cname, linenum, me, *standards, 0,
          flags & GD_PEDANTIC, 1);
  } else if (strcmp(ptr, "PROTECT") == 0) {
    if (strcmp(in_cols[1], "none") == 0)
      D->fragment[me].protection = GD_PROTECT_NONE;
    else if (strcmp(in_cols[1], "format") == 0)
      D->fragment[me].protection = GD_PROTECT_FORMAT;
    else if (strcmp(in_cols[1], "data") == 0)
      D->fragment[me].protection = GD_PROTECT_DATA;
    else if (strcmp(in_cols[1], "all") == 0)
      D->fragment[me].protection = GD_PROTECT_ALL;
    else
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_PROTECT, D->fragment[me].cname,
          linenum, in_cols[1]);
  } else if (strcmp(ptr, "REFERENCE") == 0) {
    free(*ref_name);
    *ref_name = strdup(in_cols[1]);
  } else if (strcmp(ptr, "VERSION") == 0)
    *standards = atoi(in_cols[1]);
  else {
    dreturn("%i", 0);
    return 0;
  }

  dreturn("%i", 1);
  return 1;
}

/* _GD_ParseFragment: Parse each line of the fragment.
 *
 *       Returns NULL unless this fragment contains a REFERENCE directive.
 */
char* _GD_ParseFragment(FILE* fp, DIRFILE *D, int me, int* standards,
    unsigned long flags)
{
  char instring[GD_MAX_LINE_LENGTH];
  char outstring[GD_MAX_LINE_LENGTH];
  char *in_cols[MAX_IN_COLS];
  int linenum = 0;
  char* ref_name = NULL;
  int n_cols;
  int match = 0;
  int rescan = 0;
  int se_action = GD_SYNTAX_ABORT;
  int first_fragment = -1;
  gd_entry_t* first_raw = NULL;
  gd_parser_data_t pdata;

  int saved_error = 0;
  int saved_suberror = 0;
  int saved_line = 0;
  char* saved_token = NULL;

  dtrace("%p, %p, %i, %p", fp, D, me, standards);

  /* start parsing */
  while (rescan || _GD_GetLine(fp, instring, &linenum)) {
    rescan = 0;
    n_cols = _GD_Tokenise(D, instring, outstring, in_cols,
        D->fragment[me].cname, linenum);

    if (D->error == GD_E_OK)
      match = _GD_ParseDirective(D, in_cols, n_cols, me, standards, linenum,
          &ref_name, &first_fragment, flags);

    if (D->error == GD_E_OK && !match)
      first_raw = _GD_ParseFieldSpec(D, n_cols, in_cols, NULL,
          D->fragment[me].cname, linenum, me, *standards, 0,
          flags & GD_PEDANTIC, 1);

    if (D->flags & GD_IGNORE_DUPS && D->error == GD_E_FORMAT &&
        D->suberror == GD_E_FORMAT_DUPLICATE)
      _GD_ClearError(D); /* ignore this line, continue parsing */
    else if (D->error == GD_E_FORMAT) {
      /* call the callback for this error */
      if (D->sehandler != NULL) {
        pdata.dirfile = D;
        pdata.suberror = D->suberror;
        pdata.linenum = linenum;
        pdata.filename = D->fragment[me].cname;
        pdata.line = instring;
        se_action = (*D->sehandler)(&pdata, D->sehandler_extra);
      }

      switch(se_action) {
        case GD_SYNTAX_ABORT:
          break; /* abort parsing */
        case GD_SYNTAX_CONTINUE:
          if (!saved_error) { /* remember this error ... */
            saved_suberror = D->suberror;
            saved_line = D->error_line;
            if (D->error_string != NULL)
              saved_token = strdup(D->error_string);
            saved_error = 1;
          }
          /* ... and continue parsing (fallthrough) */
        case GD_SYNTAX_IGNORE:
          _GD_ClearError(D); /* ignore this line, continue parsing */
          break;
        case GD_SYNTAX_RESCAN:
          _GD_ClearError(D);
          rescan = 1; /* rescan the modified instring */
          break;
        default:
          /* improper callback response */
          _GD_SetError(D, GD_E_CALLBACK, 0, NULL, se_action, NULL);
          break;
      }
    }

    if (D->error)
      break; /* abort in the event of a non-syntax error */
  }

  /* restore a saved error, if we have one */
  if (!D->error && saved_error) {
    _GD_SetError(D, GD_E_FORMAT, saved_suberror, D->fragment[me].cname,
        saved_line, saved_token);
    free(saved_token);
  }

  /* Set reference */
  if (!D->error) {
    if (ref_name != NULL)
      D->fragment[me].ref_name = strdup(ref_name);
    else if (first_raw != NULL)
      D->fragment[me].ref_name = strdup(first_raw->field);
  }

  dreturn("%p", ref_name);
  return ref_name;
}
/* vim: ts=2 sw=2 et tw=80
*/
