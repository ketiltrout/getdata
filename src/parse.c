/* Copyright (C) 2002-2005 C. Barth Netterfield
 * Copyright (C) 2005-2012 D. V. Wiebe
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

static gd_type_t _GD_RawType(const char* type, int standards, int pedantic)
{
  gd_type_t t = GD_UNKNOWN;;
  dtrace("\"%s\", %i, %i", type, standards, pedantic);

  /* for backwards compatibility */
  if (strlen(type) == 1 && (!pedantic || standards < 8))
    t = _GD_LegacyType(type[0]);
  else if (pedantic && standards < 5)
    t = GD_UNKNOWN;

  else if (strcmp(type, "NULL") == 0)
    t = GD_NULL;
  else if (strcmp(type, "INT8") == 0)
    t = GD_INT8;
  else if (strcmp(type, "UINT8") == 0)
    t = GD_UINT8;
  else if (strcmp(type, "INT16") == 0)
    t = GD_INT16;
  else if (strcmp(type, "INT32") == 0)
    t = GD_INT32;
  else if (strcmp(type, "UINT32") == 0)
    t = GD_UINT32;
  else if (strcmp(type, "UINT64") == 0)
    t = GD_UINT64;
  else if (strcmp(type, "INT64") == 0)
    t = GD_INT64;
  else if (strcmp(type, "UINT16") == 0)
    t = GD_UINT16;
  else if (strcmp(type, "FLOAT32") == 0)
    t = GD_FLOAT32;
  else if (strcmp(type, "FLOAT") == 0)
    t = GD_FLOAT32;
  else if (strcmp(type, "FLOAT64") == 0)
    t = GD_FLOAT64;
  else if (strcmp(type, "DOUBLE") == 0)
    t = GD_FLOAT64;
  else if (pedantic && standards < 7)
    t = GD_UNKNOWN;

  else if (strcmp(type, "COMPLEX64") == 0)
    t = GD_COMPLEX64;
  else if (strcmp(type, "COMPLEX128") == 0)
    t = GD_COMPLEX128;

  dreturn("0x%X", t);
  return t;
}

static gd_windop_t _GD_WindOp(const char *op)
{
  gd_windop_t o = GD_WINDOP_UNK;
  dtrace("\"%s\"", op);

  switch (op[0]) {
    case 'E':
      if (op[1] == 'Q')
        o = GD_WINDOP_EQ;
      break;
    case 'L':
      if (op[1] == 'T')
        o = GD_WINDOP_LT;
      else if (op[1] == 'E')
        o = GD_WINDOP_LE;
      break;
    case 'G':
      if (op[1] == 'T')
        o = GD_WINDOP_GT;
      else if (op[1] == 'E')
        o = GD_WINDOP_GE;
      break;
    case 'N':
      if (op[1] == 'E')
        o = GD_WINDOP_NE;
      break;
    case 'S':
      if (op[1] == 'E' && op[2] == 'T')
        o = GD_WINDOP_SET;
      break;
    case 'C':
      if (op[1] == 'L' && op[2] == 'R')
        o = GD_WINDOP_CLR;
      break;
  }

  dreturn("%i", o);
  return o;
}

/* Returns a newly malloc'd string containing the scalar field name, or NULL on
 * numeric literal or error */
static char *_GD_SetScalar(DIRFILE *restrict D, const char *restrict token,
    void *restrict data, int type, const char *restrict format_file, int line,
    int *restrict index, int *restrict comp_scal, int standards, int pedantic)
{
  char *ptr = NULL;
  char *lt;

  dtrace("%p, \"%s\", %p, 0x%X, \"%s\", %i, %p, %p, %i, %i", D, token, data,
      type, format_file, line, index, comp_scal, standards, pedantic);

  if (type & (GD_COMPLEX | GD_IEEE754)) {
    /* try to convert to double */
    const char *semicolon;
    double i = 0;
    double d = strtod(token, &ptr);

    /* check for a complex value -- look for the semicolon */
    for (semicolon = token; *semicolon; ++semicolon)
      if (*semicolon == ';')
        break;

    /* there were trailing characters in the double or real part of complex */
    if (ptr != semicolon) {
      ptr = _GD_Strdup(D, token);
      if (D->error) {
        dreturn("%p", NULL);
        return NULL;
      }
      goto carray_check;
    }

    /* If there was a semicolon, try to extract the imaginary part */
    if (*semicolon == ';') {
      /* if a complex value is not permitted, complain */
      if (!(type & GD_COMPLEX)) {
        _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_LITERAL, format_file, line,
            token);
        dreturn("%p", NULL);
        return NULL;
      }

      i = strtod(semicolon + 1, &ptr);

      /* there were trailing characters in the imaginary part of complex -- this
       * can't be a valid field name, since ; is prohibited */
      if (*ptr != '\0') {
        _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_LITERAL, format_file, line,
            token);
        dreturn("%p", NULL);
        return NULL;
      }

      *comp_scal = 1;
    }

    if (type == GD_COMPLEX128) {
      *(double *)data = d;
      *((double *)data + 1) = i;
    } else if (type == GD_COMPLEX64) {
      *(float *)data = (float)d;
      *((float *)data + 1) = (float)i;
    } else if (type == GD_FLOAT64)
      *(double *)data = d;
    else if (type == GD_FLOAT32)
      *(float *)data = (float)d;
    else
      _GD_InternalError(D);
  } else if (type & GD_SIGNED) {
    /* try to convert to long long int */
    long long int lli = gd_strtoll(token, &ptr,
        (!pedantic || standards >= 9) ? 0 : 10);

    /* there were trailing characters in the long long int */
    if (*ptr != '\0') {
      ptr = _GD_Strdup(D, token);
      if (D->error) {
        dreturn("%p", NULL);
        return NULL;
      }
      goto carray_check;
    }

    if (type == GD_INT64)
      *(int64_t *)data = (int64_t)lli;
    else if (type == GD_INT32)
      *(int32_t *)data = (int32_t)lli;
    else if (type == GD_INT16)
      *(int16_t *)data = (int16_t)lli;
    else if (type == GD_INT8)
      *(int8_t *)data = (int8_t)lli;
    else
      _GD_InternalError(D);
  } else {
    /* try to convert to unsigned long long int */
    unsigned long long int ulli = gd_strtoull(token, &ptr,
        (!pedantic || standards >= 9) ? 0 : 10);

    /* there were trailing characters in the unsigned long long int */
    if (*ptr != '\0') {
      ptr = _GD_Strdup(D, token);
      if (D->error) {
        dreturn("%p", NULL);
        return NULL;
      }
      goto carray_check;
    }

    if (type == GD_UINT64)
      *(uint64_t *)data = (uint64_t)ulli;
    else if (type == GD_UINT32)
      *(uint32_t *)data = (uint32_t)ulli;
    else if (type == GD_UINT16)
      *(uint16_t *)data = (uint16_t)ulli;
    else if (type == GD_UINT8)
      *(uint8_t *)data = (uint8_t)ulli;
    else
      _GD_InternalError(D);
  }

  dreturn("%p", NULL);
  return NULL;

carray_check:
  /* look for a < delimeter */
  *index = -1;
  for (lt = ptr; *lt; ++lt)
    if (*lt == '<') {
      *lt = '\0';
      *index = atoi(lt + 1);
      break;
    }

  dreturn("\"%s\" (%i)", ptr, *index);
  return ptr;
}

/* _GD_ParseRaw: parse a RAW entry in the format file
*/
static gd_entry_t *_GD_ParseRaw(DIRFILE *restrict D,
    char *gd_restrict_arr in_cols[MAX_IN_COLS], int n_cols,
    const gd_entry_t *restrict parent, const char *restrict format_file,
    int line, int me, int standards, int pedantic, int *restrict is_dot)
{
  gd_entry_t *E;
  int offset;

  dtrace("%p, %p, %i, %p, \"%s\", %i, %i, %i, %i, %p", D, in_cols, n_cols,
      parent, format_file, line, me, standards, pedantic, is_dot);

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

  E = (gd_entry_t *)_GD_Malloc(D, sizeof(gd_entry_t));
  if (E == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = (struct _gd_private_entry *)_GD_Malloc(D,
      sizeof(struct _gd_private_entry));
  if (E->e == NULL) {
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct _gd_private_entry));

  E->field_type = GD_RAW_ENTRY;
  E->e->u.raw.file[0].idata = E->e->u.raw.file[1].idata = -1;
  E->e->u.raw.file[0].subenc = GD_ENC_UNKNOWN; /* don't know the encoding
                                                    subscheme yet */

  E->field = _GD_MungeFromFrag(D, NULL, me, in_cols[0], &offset);
  if (E->field && _GD_ValidateField(E->field + offset, standards, pedantic, 0,
        is_dot))
  {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    _GD_FreeE(D, E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  E->e->u.raw.filebase = _GD_Strdup(D, in_cols[0]);
  E->EN(raw,data_type) = _GD_RawType(in_cols[2], standards, pedantic);
  E->e->u.raw.size = GD_SIZE(E->EN(raw,data_type));

  if (E->e->u.raw.size == 0 || E->EN(raw,data_type) & 0x40)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_TYPE, format_file, line,
        in_cols[2]);
  else if ((E->scalar[0] = _GD_SetScalar(D, in_cols[3], &E->EN(raw,spf),
          GD_UINT16, format_file, line, E->scalar_ind, NULL, standards,
          pedantic)) == NULL)
  {
    E->e->calculated = 1;
    if (E->EN(raw,spf) <= 0)
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_SPF, format_file, line,
          in_cols[3]);
  }

  if (D->error != GD_E_OK) {
    _GD_FreeE(D, E, 1);
    E = NULL;
  }

  dreturn("%p", E);
  return E;
}

/* _GD_ParseLincom: parse a LINCOM entry in the format file.
*/
static gd_entry_t *_GD_ParseLincom(DIRFILE *restrict D,
    char *gd_restrict_arr in_cols[MAX_IN_COLS], int n_cols,
    const gd_entry_t *restrict parent, const char *restrict format_file,
    int line, int me, int standards, int pedantic, int *restrict is_dot)
{
  int i, offset;
  char* ptr = NULL;
  gd_entry_t *E;

  dtrace("%p, %p, %i, %p, \"%s\", %i, %i, %i, %i, %p", D, in_cols, n_cols,
      parent, format_file, line, me, standards, pedantic, is_dot);

  if (n_cols < 3) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, format_file, line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  E = (gd_entry_t *)_GD_Malloc(D, sizeof(gd_entry_t));
  if (E == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = (struct _gd_private_entry *)_GD_Malloc(D,
      sizeof(struct _gd_private_entry));
  if (E->e == NULL) {
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct _gd_private_entry));

  E->field_type = GD_LINCOM_ENTRY;

  E->field = _GD_MungeFromFrag(D, parent, me, in_cols[0], &offset);
  if (E->field && _GD_ValidateField(E->field + offset, standards, pedantic, 0,
        is_dot))
  {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    _GD_FreeE(D, E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  E->e->calculated = 1;
  E->EN(lincom,n_fields) = (int)(strtol(in_cols[2], &ptr, 10));
  if (*ptr != '\0') {
    E->EN(lincom,n_fields) = (n_cols - 2) / 3;
    /* assume <n> has been omitted */
    if (n_cols % 3 != 2 || E->EN(lincom,n_fields) < 1 || E->EN(lincom,n_fields)
        > GD_MAX_LINCOM)
    {
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, format_file, line, NULL);
      _GD_FreeE(D, E, 1);
      dreturn("%p", NULL);
      return NULL;
    }
    /* the following two statements are somewhat hacky.... */
    n_cols++;
    in_cols--;
  }

  if ((E->EN(lincom,n_fields) < 1) || (E->EN(lincom,n_fields) >
        GD_MAX_LINCOM))
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_FIELDS, format_file, line,
        in_cols[2]);
  else if (n_cols < E->EN(lincom,n_fields) * 3 + 3)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, format_file, line, NULL);
  else
    for (i = 0; i < E->EN(lincom,n_fields); i++) {
      E->in_fields[i] = _GD_Strdup(D, in_cols[i * 3 + 3]);
      E->scalar[i] = _GD_SetScalar(D, in_cols[i * 3 + 4], &E->EN(lincom,cm)[i],
          GD_COMPLEX128, format_file, line, E->scalar_ind + i, &E->comp_scal,
          standards, pedantic);
      E->EN(lincom,m)[i] = creal(E->EN(lincom,cm)[i]);
      E->scalar[i + GD_MAX_LINCOM] = _GD_SetScalar(D, in_cols[i * 3 + 5],
          &E->EN(lincom,cb)[i], GD_COMPLEX128, format_file, line,
          E->scalar_ind + i + GD_MAX_LINCOM, &E->comp_scal, standards,
          pedantic);
      E->EN(lincom,b)[i] = creal(E->EN(lincom,cb)[i]);

      if (E->scalar[i] != NULL || E->scalar[i + GD_MAX_LINCOM] != NULL)
        E->e->calculated = 0;
    }

  if (D->error != GD_E_OK) {
    _GD_FreeE(D, E, 1);
    E = NULL;
  }

  dreturn("%p", E);
  return E;
}

/* _GD_ParseLinterp: parse a LINTERP entry in the format file.
*/
static gd_entry_t *_GD_ParseLinterp(DIRFILE *restrict D,
    char *gd_restrict_arr in_cols[MAX_IN_COLS], int n_cols,
    const gd_entry_t *restrict parent, const char *restrict format_file,
    int line, int me, int standards, int pedantic, int *restrict is_dot)
{
  gd_entry_t *E;
  int offset;

  dtrace("%p, %p, %i, %p, \"%s\", %i, %i, %i, %i, %p", D, in_cols, n_cols,
      parent, format_file, line, me, standards, pedantic, is_dot);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, format_file, line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  E = (gd_entry_t *)_GD_Malloc(D, sizeof(gd_entry_t));
  if (E == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = (struct _gd_private_entry *)_GD_Malloc(D,
      sizeof(struct _gd_private_entry));
  if (E->e == NULL) {
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct _gd_private_entry));

  E->field_type = GD_LINTERP_ENTRY;
  E->in_fields[0] = NULL;
  E->e->entry[0] = NULL;
  E->e->calculated = 1;
  E->EN(linterp,table) = NULL;

  E->field = _GD_MungeFromFrag(D, parent, me, in_cols[0], &offset);
  if (E->field && _GD_ValidateField(E->field + offset, standards, pedantic, 0,
        is_dot))
  {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    _GD_FreeE(D, E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  E->in_fields[0] = _GD_Strdup(D, in_cols[2]);
  E->e->u.linterp.table_len = -1; /* linterp file not read yet */

  E->EN(linterp,table) = _GD_Strdup(D, in_cols[3]);

  if (D->error) {
    _GD_FreeE(D, E, 1);
    E = NULL;
  }

  dreturn("%p", E);
  return E;
}

/* _GD_ParseMultiply: parse MULTIPLY entry in format file.
*/
static gd_entry_t *_GD_ParseMultiply(DIRFILE *restrict D,
    char *gd_restrict_arr in_cols[MAX_IN_COLS], int n_cols,
    const gd_entry_t *restrict parent, const char *restrict format_file,
    int line, int me, int standards, int pedantic, int *restrict is_dot)
{
  gd_entry_t *E;
  int offset;

  dtrace("%p, %p, %i, %p, \"%s\", %i, %i, %i, %i, %p", D, in_cols, n_cols,
      parent, format_file, line, me, standards, pedantic, is_dot);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, format_file, line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  E = (gd_entry_t *)_GD_Malloc(D, sizeof(gd_entry_t));
  if (E == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = (struct _gd_private_entry *)_GD_Malloc(D,
      sizeof(struct _gd_private_entry));
  if (E->e == NULL) {
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct _gd_private_entry));

  E->field_type = GD_MULTIPLY_ENTRY;
  E->in_fields[0] = E->in_fields[1] = NULL;
  E->e->entry[0] = E->e->entry[1] = NULL;
  E->e->calculated = 1;

  E->field = _GD_MungeFromFrag(D, parent, me, in_cols[0], &offset);
  if (E->field && _GD_ValidateField(E->field + offset, standards, pedantic, 0,
        is_dot))
  {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    _GD_FreeE(D, E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  E->in_fields[0] = _GD_Strdup(D, in_cols[2]);
  E->in_fields[1] = _GD_Strdup(D, in_cols[3]);

  if (D->error) {
    _GD_FreeE(D, E, 1);
    E = NULL;
  }

  dreturn("%p", E);
  return E;
}

/* _GD_ParseRecip: parse RECIP entry in format file.
*/
static gd_entry_t *_GD_ParseRecip(DIRFILE *restrict D,
    char *gd_restrict_arr in_cols[MAX_IN_COLS], int n_cols,
    const gd_entry_t *restrict parent, const char *restrict format_file,
    int line, int me, int standards, int pedantic, int *restrict is_dot)
{
  gd_entry_t *E;
  int offset;

  dtrace("%p, %p, %i, %p, \"%s\", %i, %i, %i, %i, %p", D, in_cols, n_cols,
      parent, format_file, line, me, standards, pedantic, is_dot);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, format_file, line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  E = (gd_entry_t *)_GD_Malloc(D, sizeof(gd_entry_t));
  if (E == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = (struct _gd_private_entry *)_GD_Malloc(D,
      sizeof(struct _gd_private_entry));
  if (E->e == NULL) {
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct _gd_private_entry));

  E->field_type = GD_RECIP_ENTRY;
  E->in_fields[0] = NULL;
  E->e->entry[0] = NULL;
  E->e->calculated = 0;

  E->field = _GD_MungeFromFrag(D, parent, me, in_cols[0], &offset);
  if (E->field && _GD_ValidateField(E->field + offset, standards, pedantic, 0,
        is_dot))
  {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    _GD_FreeE(D, E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  E->in_fields[0] = _GD_Strdup(D, in_cols[2]);

  E->scalar[0] = _GD_SetScalar(D, in_cols[3], &E->EN(recip,cdividend),
      GD_COMPLEX128, format_file, line, E->scalar_ind, &E->comp_scal, standards,
      pedantic);
  E->EN(recip,dividend) = creal(E->EN(recip,cdividend));
  E->comp_scal = (cimag(E->EN(recip,cdividend)) == 0) ? 0 : 1;

  if (D->error != GD_E_OK) {
    _GD_FreeE(D, E, 1);
    E = NULL;
  }

  dreturn("%p", E);
  return E;
}

/* _GD_ParseWindow: parse WINDOW entry in format file.
*/
static gd_entry_t *_GD_ParseWindow(DIRFILE *restrict D,
    char *gd_restrict_arr in_cols[MAX_IN_COLS], int n_cols,
    const gd_entry_t *restrict parent, const char *restrict format_file,
    int line, int me, int standards, int pedantic, int *restrict is_dot)
{
  gd_entry_t *E;
  int offset;

  dtrace("%p, %p, %i, %p, \"%s\", %i, %i, %i, %i, %p", D, in_cols, n_cols,
      parent, format_file, line, me, standards, pedantic, is_dot);

  if (n_cols < 6) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, format_file, line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  E = (gd_entry_t *)_GD_Malloc(D, sizeof(gd_entry_t));
  if (E == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = (struct _gd_private_entry *)_GD_Malloc(D,
      sizeof(struct _gd_private_entry));
  if (E->e == NULL) {
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct _gd_private_entry));

  E->field_type = GD_WINDOW_ENTRY;

  E->field = _GD_MungeFromFrag(D, parent, me, in_cols[0], &offset);
  if (E->field && _GD_ValidateField(E->field + offset, standards, pedantic, 0,
        is_dot))
  {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    _GD_FreeE(D, E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  E->in_fields[0] = _GD_Strdup(D, in_cols[2]);
  E->in_fields[1] = _GD_Strdup(D, in_cols[3]);

  E->EN(window,windop) = _GD_WindOp(in_cols[4]);
  if (E->EN(window,windop) == GD_WINDOP_UNK) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_WINDOP, format_file, line,
        in_cols[4]);
    _GD_FreeE(D, E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  switch (E->EN(window,windop)) {
    case GD_WINDOP_EQ:
    case GD_WINDOP_NE:
      E->scalar[0] = _GD_SetScalar(D, in_cols[5], &E->EN(window,threshold.i),
          GD_INT64, format_file, line, E->scalar_ind, NULL, standards,
          pedantic);
      break;
    case GD_WINDOP_SET:
    case GD_WINDOP_CLR:
      E->scalar[0] = _GD_SetScalar(D, in_cols[5], &E->EN(window,threshold.u),
          GD_UINT64, format_file, line, E->scalar_ind, NULL, standards,
          pedantic);
      break;
    default:
      E->scalar[0] = _GD_SetScalar(D, in_cols[5], &E->EN(window,threshold.r),
          GD_FLOAT64, format_file, line, E->scalar_ind, NULL, standards,
          pedantic);
      break;
  }

  if (D->error != GD_E_OK) {
    _GD_FreeE(D, E, 1);
    E = NULL;
  }

  dreturn("%p", E);
  return E;
}

/* _GD_ParseMplex: parse MPLEX entry in format file.
*/
static gd_entry_t *_GD_ParseMplex(DIRFILE *restrict D,
    char *gd_restrict_arr in_cols[MAX_IN_COLS], int n_cols,
    const gd_entry_t *restrict parent, const char *restrict format_file,
    int line, int me, int standards, int pedantic, int *restrict is_dot)
{
  gd_entry_t *E;
  int offset;

  dtrace("%p, %p, %i, %p, \"%s\", %i, %i, %i, %i, %p", D, in_cols, n_cols,
      parent, format_file, line, me, standards, pedantic, is_dot);

  if (n_cols < 6) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, format_file, line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  E = (gd_entry_t *)_GD_Malloc(D, sizeof(gd_entry_t));
  if (E == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = (struct _gd_private_entry *)_GD_Malloc(D,
      sizeof(struct _gd_private_entry));
  if (E->e == NULL) {
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct _gd_private_entry));

  E->field_type = GD_MPLEX_ENTRY;

  E->field = _GD_MungeFromFrag(D, parent, me, in_cols[0], &offset);
  if (E->field && _GD_ValidateField(E->field + offset, standards, pedantic, 0,
        is_dot))
  {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    _GD_FreeE(D, E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  /* These are backwards! */
  E->in_fields[0] = _GD_Strdup(D, in_cols[3]);
  E->in_fields[1] = _GD_Strdup(D, in_cols[2]);

  E->scalar[0] = _GD_SetScalar(D, in_cols[4], &E->EN(mplex,count_val),
      GD_UINT16, format_file, line, E->scalar_ind, NULL, standards, pedantic);

  E->scalar[1] = _GD_SetScalar(D, in_cols[5], &E->EN(mplex,count_max),
      GD_UINT16, format_file, line, E->scalar_ind + 1, NULL, standards,
      pedantic);

  if (E->scalar[0] == NULL && E->scalar[1] == NULL &&
      E->EN(mplex,count_val) >= E->EN(mplex,count_max))
  {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_MPLEXVAL, format_file, line,
        in_cols[4]);
  }

  if (D->error != GD_E_OK) {
    _GD_FreeE(D, E, 1);
    E = NULL;
  }

  dreturn("%p", E);
  return E;
}

/* _GD_ParseDivide: parse DIVIDE entry in format file.
*/
static gd_entry_t *_GD_ParseDivide(DIRFILE *restrict D,
    char *gd_restrict_arr in_cols[MAX_IN_COLS], int n_cols,
    const gd_entry_t *restrict parent, const char *restrict format_file,
    int line, int me, int standards, int pedantic, int *restrict is_dot)
{
  gd_entry_t *E;
  int offset;

  dtrace("%p, %p, %i, %p, \"%s\", %i, %i, %i, %i, %p", D, in_cols, n_cols,
      parent, format_file, line, me, standards, pedantic, is_dot);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, format_file, line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  E = (gd_entry_t *)_GD_Malloc(D, sizeof(gd_entry_t));
  if (E == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = (struct _gd_private_entry *)_GD_Malloc(D,
      sizeof(struct _gd_private_entry));
  if (E->e == NULL) {
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct _gd_private_entry));

  E->field_type = GD_DIVIDE_ENTRY;
  E->in_fields[0] = E->in_fields[1] = NULL;
  E->e->entry[0] = E->e->entry[1] = NULL;
  E->e->calculated = 0;

  E->field = _GD_MungeFromFrag(D, parent, me, in_cols[0], &offset);
  if (E->field && _GD_ValidateField(E->field + offset, standards, pedantic, 0,
        is_dot))
  {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    _GD_FreeE(D, E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  E->in_fields[0] = _GD_Strdup(D, in_cols[2]);
  E->in_fields[1] = _GD_Strdup(D, in_cols[3]);

  if (D->error) {
    _GD_FreeE(D, E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  dreturn("%p", E);
  return E;
}

/* _GD_ParseBit: parse BIT entry in format file.
*/
static gd_entry_t *_GD_ParseBit(DIRFILE *restrict D, int is_signed,
    char *gd_restrict_arr in_cols[MAX_IN_COLS], int n_cols,
    const gd_entry_t *restrict parent, const char *restrict format_file,
    int line, int me, int standards, int pedantic, int *restrict is_dot)
{
  gd_entry_t *E;
  int offset;

  dtrace("%p, %i, %p, %i, %p, \"%s\", %i, %i, %i, %i, %p", D, is_signed,
      in_cols, n_cols, parent, format_file, line, me, standards, pedantic,
      is_dot);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, format_file, line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  E = (gd_entry_t *)_GD_Malloc(D, sizeof(gd_entry_t));
  if (E == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = (struct _gd_private_entry *)_GD_Malloc(D,
      sizeof(struct _gd_private_entry));
  if (E->e == NULL) {
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct _gd_private_entry));

  E->field_type = (is_signed) ? GD_SBIT_ENTRY : GD_BIT_ENTRY;
  E->in_fields[0] = NULL;
  E->e->entry[0] = NULL;
  E->e->calculated = 1;

  E->field = _GD_MungeFromFrag(D, parent, me, in_cols[0], &offset);
  if (E->field && _GD_ValidateField(E->field + offset, standards, pedantic, 0,
        is_dot))
  {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    _GD_FreeE(D, E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  E->in_fields[0] = _GD_Strdup(D, in_cols[2]);
  E->scalar[0] = _GD_SetScalar(D, in_cols[3], &E->EN(bit,bitnum), GD_INT16,
      format_file, line, E->scalar_ind, NULL, standards, pedantic);

  if (n_cols > 4)
    E->scalar[1] = _GD_SetScalar(D, in_cols[4], &E->EN(bit,numbits), GD_INT16,
        format_file, line, E->scalar_ind + 1, NULL, standards, pedantic);
  else
    E->EN(bit,numbits) = 1;

  if (E->scalar[0] != NULL || E->scalar[1] != NULL)
    E->e->calculated = 0;

  if (E->scalar[1] == NULL && E->EN(bit,numbits) < 1)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_NUMBITS, format_file, line, NULL);
  else if (E->scalar[0] == NULL && E->EN(bit,bitnum) < 0)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BITNUM, format_file, line, NULL);
  else if (E->e->calculated && E->EN(bit,bitnum) + E->EN(bit,numbits) - 1 > 63)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BITSIZE, format_file, line, NULL);

  if (D->error != GD_E_OK) {
    _GD_FreeE(D, E, 1);
    E = NULL;
  }

  dreturn("%p", E);
  return E;
}

/* _GD_ParsePhase: parse PHASE entry in formats file.
*/
static gd_entry_t *_GD_ParsePhase(DIRFILE *restrict D,
    char *gd_restrict_arr in_cols[MAX_IN_COLS], int n_cols,
    const gd_entry_t *restrict parent, const char *restrict format_file,
    int line, int me, int standards, int pedantic, int *restrict is_dot)
{
  gd_entry_t *E;
  int offset;

  dtrace("%p, %p, %i, %p, \"%s\", %i, %i, %i, %i, %p", D, in_cols, n_cols,
      parent, format_file, line, me, standards, pedantic, is_dot);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, format_file, line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  E = (gd_entry_t *)_GD_Malloc(D, sizeof(gd_entry_t));
  if (E == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = (struct _gd_private_entry *)_GD_Malloc(D,
      sizeof(struct _gd_private_entry));
  if (E->e == NULL) {
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct _gd_private_entry));

  E->field_type = GD_PHASE_ENTRY;
  E->in_fields[0] = NULL;
  E->e->entry[0] = NULL;

  E->field = _GD_MungeFromFrag(D, parent, me, in_cols[0], &offset);
  if (E->field && _GD_ValidateField(E->field + offset, standards, pedantic, 0,
        is_dot))
  {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    _GD_FreeE(D, E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  E->in_fields[0] = _GD_Strdup(D, in_cols[2]); /* field */

  if ((E->scalar[0] = _GD_SetScalar(D, in_cols[3], &E->EN(phase,shift),
          GD_INT64, format_file, line, E->scalar_ind, NULL, standards,
          pedantic)) == NULL)
  {
    E->e->calculated = 1;
  }

  if (D->error != GD_E_OK) {
    _GD_FreeE(D, E, 1);
    E = NULL;
  }

  dreturn("%p", E);
  return E;
}

/* _GD_ParsePolynom: parse a POLYNOM in the format file.
*/
static gd_entry_t *_GD_ParsePolynom(DIRFILE *restrict D,
    char *gd_restrict_arr in_cols[MAX_IN_COLS], int n_cols,
    const gd_entry_t *restrict parent, const char *restrict format_file,
    int line, int me, int standards, int pedantic, int *restrict is_dot)
{
  int i, offset;
  gd_entry_t *E;

  dtrace("%p, %p, %i, %p, \"%s\", %i, %i, %i, %i, %p", D, in_cols, n_cols,
      parent, format_file, line, me, standards, pedantic, is_dot);

  if (n_cols < 5) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, format_file, line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  E = (gd_entry_t *)_GD_Malloc(D, sizeof(gd_entry_t));
  if (E == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = (struct _gd_private_entry *)_GD_Malloc(D,
      sizeof(struct _gd_private_entry));
  if (E->e == NULL) {
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct _gd_private_entry));

  E->field_type = GD_POLYNOM_ENTRY;
  E->field = _GD_MungeFromFrag(D, parent, me, in_cols[0], &offset);
  if (E->field && _GD_ValidateField(E->field + offset, standards, pedantic, 0,
        is_dot))
  {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    _GD_FreeE(D, E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  E->EN(polynom,poly_ord) = n_cols - 4;

  /* the legacy ignore-trailing-tokens "feature" */
  if (E->EN(polynom,poly_ord) > GD_MAX_POLYORD)
    E->EN(polynom,poly_ord) = GD_MAX_POLYORD;

  E->e->calculated = 1;

  E->in_fields[0] = _GD_Strdup(D, in_cols[2]);

  for (i = 0; i <= E->EN(polynom,poly_ord); i++) {
    E->scalar[i] = _GD_SetScalar(D, in_cols[i + 3], &E->EN(polynom,ca)[i],
        GD_COMPLEX128, format_file, line, E->scalar_ind + i, &E->comp_scal,
        standards, pedantic);
    E->EN(polynom,a)[i] = creal(E->EN(polynom,ca)[i]);

    if (E->scalar[i] != NULL)
      E->e->calculated = 0;
  }

  if (D->error != GD_E_OK) {
    _GD_FreeE(D, E, 1);
    E = NULL;
  }

  dreturn("%p", E);
  return E;
}

gd_type_t _GD_ConstType(DIRFILE *D, gd_type_t type)
{
  dtrace("%p, 0x%X", D, type);

  switch (type) {
    case GD_UINT8:
    case GD_UINT16:
    case GD_UINT32:
    case GD_UINT64:
      dreturn("0x%X", GD_UINT64);
      return GD_UINT64;
    case GD_INT8:
    case GD_INT16:
    case GD_INT32:
    case GD_INT64:
      dreturn("0x%X", GD_INT64);
      return GD_INT64;
    case GD_FLOAT32:
    case GD_FLOAT64:
      dreturn("0x%X", GD_FLOAT64);
      return GD_FLOAT64;
    case GD_COMPLEX64:
    case GD_COMPLEX128:
      dreturn("0x%X", GD_COMPLEX128);
      return GD_COMPLEX128;
    case GD_NULL:
    case GD_UNKNOWN:
      _GD_InternalError(D);
  }

  dreturn("0x%X", GD_NULL);
  return GD_NULL;
}

/* _GD_ParseConst: parse CONST entry in formats file.
*/
static gd_entry_t *_GD_ParseConst(DIRFILE *restrict D,
    char *gd_restrict_arr in_cols[MAX_IN_COLS], int n_cols,
    const gd_entry_t *restrict parent, const char *restrict format_file,
    int line, int me, int standards, int pedantic, int *restrict is_dot)
{
  int offset;
  char* ptr;
  gd_type_t type;
  gd_entry_t *E;

  dtrace("%p, %p, %i, %p, \"%s\", %i, %i, %i, %p", D, in_cols, n_cols, parent,
      format_file, line, standards, pedantic, is_dot);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, format_file, line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  E = (gd_entry_t *)_GD_Malloc(D, sizeof(gd_entry_t));
  if (E == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = (struct _gd_private_entry *)_GD_Malloc(D,
      sizeof(struct _gd_private_entry));
  if (E->e == NULL) {
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct _gd_private_entry));

  E->field_type = GD_CONST_ENTRY;
  E->e->calculated = 1;

  E->field = _GD_MungeFromFrag(D, parent, me, in_cols[0], &offset);
  if (E->field && _GD_ValidateField(E->field + offset, standards, pedantic, 0,
        is_dot))
  {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    _GD_FreeE(D, E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  E->EN(scalar,const_type) = _GD_RawType(in_cols[2], standards, pedantic);
  E->EN(scalar,array_len) = -1;

  if (GD_SIZE(E->EN(scalar,const_type)) == 0 || E->EN(scalar,const_type) & 0x40)
  {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_TYPE, format_file, line,
        in_cols[2]);
    _GD_FreeE(D, E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  type = _GD_ConstType(D, E->EN(scalar,const_type));
  E->e->u.scalar.d = _GD_Malloc(D, GD_SIZE(type));

  if (D->error) {
    _GD_FreeE(D, E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  ptr = _GD_SetScalar(D, in_cols[3], E->e->u.scalar.d, type, format_file, line,
      &offset, &offset, standards, pedantic);
  if (ptr) {
    free(ptr);
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_LITERAL, format_file, line,
        in_cols[3]);
  }

  if (D->error) {
    _GD_FreeE(D, E, 1);
    E = NULL;
  }

  dreturn("%p", E);
  return E;
}

/* _GD_ParseCarray: parse CARRAY entry in formats file.
*/
static gd_entry_t *_GD_ParseCarray(DIRFILE *restrict D,
    char *in_cols[MAX_IN_COLS], int n_cols, const gd_entry_t *restrict parent,
    const char *restrict format_file, int line, int me, int standards,
    int pedantic, int *restrict is_dot, char **outstring, const char *tok_pos)
{
  int offset;
  int c, first, n, new_z, s, z;
  gd_type_t t;
  char* ptr;
  void *data;
  gd_entry_t *E;

  dtrace("%p, %p, %i, %p, \"%s\", %i, %i, %i, %i, %p, %p, %p", D, in_cols,
      n_cols, parent, format_file, line, me, standards, pedantic, is_dot,
      outstring, tok_pos);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, format_file, line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  E = (gd_entry_t *)_GD_Malloc(D, sizeof(gd_entry_t));
  if (E == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = (struct _gd_private_entry *)_GD_Malloc(D,
      sizeof(struct _gd_private_entry));
  if (E->e == NULL) {
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct _gd_private_entry));

  E->field_type = GD_CARRAY_ENTRY;
  E->e->calculated = 1;

  E->field = _GD_MungeFromFrag(D, parent, me, in_cols[0], &offset);
  if (E->field && _GD_ValidateField(E->field + offset, standards, pedantic, 0,
        is_dot))
  {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
    _GD_FreeE(D, E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  E->EN(scalar,const_type) = _GD_RawType(in_cols[2], standards, pedantic);

  if (GD_SIZE(E->EN(scalar,const_type)) == 0 || E->EN(raw,data_type) & 0x40) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_TYPE, format_file, line,
        in_cols[2]);
  }

  if (D->error) {
    _GD_FreeE(D, E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  /* spool in the data */
  new_z = MAX_IN_COLS - 2;
  z = 0;
  n = 0;
  first = 3;
  data = NULL;
  t = _GD_ConstType(D, E->EN(scalar,const_type));
  s = GD_SIZE(t);

  for (;;) {
    if (z < new_z) {
      void *new_data = (char *)_GD_Realloc(D, data, new_z * s);
      if (new_data == NULL) {
        free(data);
        _GD_FreeE(D, E, 1);
        dreturn("%p", NULL);
        return NULL;
      }
      data = new_data;
      z = new_z;
    }

    for (c = first; c < n_cols; ++c) {
      ptr = _GD_SetScalar(D, in_cols[c], (char *)data + s * n++, t, format_file,
          line, &offset, &offset, standards, pedantic);
      if (n == GD_MAX_CARRAY_LENGTH)
        break;

      if (ptr) {
        free(ptr);
        free(data);
        _GD_FreeE(D, E, 1);
        _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_LITERAL, format_file, line,
            in_cols[c]);
      }
    }

    if (n_cols < MAX_IN_COLS)
      break;

    /* get more tokens */
    free(*outstring);
    n_cols = _GD_Tokenise(D, tok_pos, outstring, &tok_pos, MAX_IN_COLS, in_cols,
        format_file, line, standards, pedantic);
    if (n_cols == 0 || D->error)
      break;
    first = 0;
    new_z = z + n_cols;
  }
  /* save the list */
  E->e->u.scalar.d = data;
  E->EN(scalar,array_len) = n;

  if (D->error) {
    _GD_FreeE(D, E, 1);
    E = NULL;
  }

  dreturn("%p", E);
  return E;
}

/* _GD_ParseString: parse STRING entry in formats file.
*/
static gd_entry_t *_GD_ParseString(DIRFILE *restrict D,
    char *gd_restrict_arr in_cols[MAX_IN_COLS], int n_cols,
    const gd_entry_t *restrict parent, const char *restrict format_file,
    int line, int me, int standards, int pedantic, int *restrict is_dot)
{
  gd_entry_t *E;
  int offset;

  dtrace("%p, %p, %i, %p, \"%s\", %i, %i, %i, %i, %p", D, in_cols, n_cols,
      parent, format_file, line, me, standards, pedantic, is_dot);

  if (n_cols < 3) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, format_file, line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  E = (gd_entry_t *)_GD_Malloc(D, sizeof(gd_entry_t));
  if (E == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = (struct _gd_private_entry *)_GD_Malloc(D,
      sizeof(struct _gd_private_entry));
  if (E->e == NULL) {
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct _gd_private_entry));

  E->field_type = GD_STRING_ENTRY;
  E->e->u.string = _GD_Strdup(D, in_cols[2]);
  E->e->calculated = 1;

  E->field = _GD_MungeFromFrag(D, parent, me, in_cols[0], &offset);
  if (E->field && _GD_ValidateField(E->field + offset, standards, pedantic, 0,
        is_dot))
  {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
        in_cols[0]);
  }

  if (D->error) {
    _GD_FreeE(D, E, 1);
    E = NULL;
  }

  dreturn("%p", E);
  return E;
}

static int _GD_UTF8Encode(DIRFILE *restrict D, const char *restrict format_file,
    int linenum, char **restrict op, uint32_t value)
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

/* _GD_CheckParent: look for a slashed field name and, if found, see if the
 * parent exists in the current fragment.  Returns parent entry on success,
 * and points *name to the metaname part.  me == -1 implies we're not in the
 * parser, but called from _GD_Add.
 */
gd_entry_t *_GD_CheckParent(DIRFILE *restrict D, char **restrict name, int me,
    int linenum)
{
  int dummy;
  char *cptr, *munged_code;
  gd_entry_t *P = NULL;

  dtrace("%p, \"%s\", %i, %i", D, *name, me, linenum);

  for (cptr = *name + 1; *cptr != '\0'; ++cptr)
    if (*cptr == '/') {
      *cptr = '\0';
      if (me == -1)
        munged_code = strdup(*name);
      else
        munged_code = _GD_MungeFromFrag(D, NULL, me, *name, &dummy);
      if (munged_code) {
        P = _GD_FindField(D, munged_code, D->entry, D->n_entries, 0, NULL);
        free(munged_code);
      }
      if (P == NULL) {
        if (me == -1) {
          *cptr = '/'; /* undo field munging; _GD_Add will conclude this is
                          a field with an illegal '/' */
          dreturn("%p", NULL);
          return NULL;
        }
        _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_NO_FIELD,
            D->fragment[me].cname, linenum, *name);
      } else if (P->field_type == GD_ALIAS_ENTRY) {
        if (me == -1)
          P = P->e->entry[0]; /* just de-alias */
        else
          _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_ALIAS, D->fragment[me].cname,
              linenum, *name);
      } else if (P->fragment_index != me && me != -1)
        _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_LOCATION,
            D->fragment[me].cname, linenum, *name);

      if (D->error) {
        dreturn("%p", NULL);
        return NULL;
      }

      /* point name to the metafield name */
      *name = cptr + 1;
      break;
    }

  dreturn("%p", P);
  return P;
}

/* _GD_ParseFieldSpec: Parse a format file line fragment containing a field
 * specification */
gd_entry_t *_GD_ParseFieldSpec(DIRFILE *restrict D, int n_cols, char **in_cols,
    const gd_entry_t *restrict P, const char *restrict format_file, int linenum,
    int me, int standards, int creat, unsigned long flags, int insert,
    char **outstring, const char *tok_pos)
{
  gd_entry_t* E = NULL;
  void *ptr;
  int is_dot = 0;
  const int pedantic = flags & GD_PEDANTIC;

  dtrace("%p, %i, %p, %p, \"%s\", %i, %i, %i, %i, %lx, %i, %p, %p", D, n_cols,
      in_cols, P, format_file, linenum, me, standards, creat, flags, insert,
      outstring, tok_pos);

  /* Check for barth-style metafield definition */
  if (P == NULL && (!pedantic || standards >= 7)) {
    P = _GD_CheckParent(D, in_cols + 0, me, linenum);
    if (P) {
      if (n_cols < 2)
        _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, D->fragment[me].cname,
            linenum, NULL);
      else
        E = _GD_ParseFieldSpec(D, n_cols, in_cols, P, D->fragment[me].cname,
            linenum, me, standards, creat, flags, insert, outstring, tok_pos);
      dreturn("%p", (!insert) ? E : NULL);
      return (!insert) ? E : NULL;
    }
  }

  ptr = _GD_Realloc(D, D->entry, (D->n_entries + 1) * sizeof(gd_entry_t*));
  if (ptr == NULL) {
    dreturn ("%p", NULL);
    return NULL;
  }
  D->entry = (gd_entry_t **)ptr;

  if (n_cols < 2)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, format_file, linenum,
        NULL);
  else if (P == NULL && (strcmp(in_cols[0], "INDEX") == 0 || (pedantic &&
          standards < 6 && strcmp(in_cols[0], "FILEFRAM") == 0)))
    /* reserved field name */
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_RES_NAME, format_file, linenum,
        NULL);
  else if (strcmp(in_cols[1], "RAW") == 0) {
    E = _GD_ParseRaw(D, in_cols, n_cols, P, format_file, linenum, me, standards,
        pedantic, &is_dot);

    /* Create the binary file, if requested */
    if (!D->error && creat) {
      /* If this fragment is protected, we can't do anything */
      if (D->fragment[me].protection != GD_PROTECT_NONE)
        _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_DATA, NULL, 0,
            D->fragment[me].cname);
      /* If the encoding scheme is unknown, we can't add the field */
      if (D->fragment[me].encoding == GD_AUTO_ENCODED)
        _GD_SetError(D, GD_E_UNKNOWN_ENCODING, GD_E_UNENC_UNDET, NULL, 0, NULL);
      else if (D->fragment[me].encoding == GD_ENC_UNSUPPORTED)
        /* If the encoding scheme is unsupported, we can't add the field */
        _GD_SetError(D, GD_E_UNSUPPORTED, 0, NULL, 0, NULL);
      else
        _GD_InitRawIO(D, E, NULL, 0, NULL, 0, GD_FILE_WRITE | GD_FILE_TOUCH, 0);
    }

    /* Is this the first raw field ever defined? */
    if (!D->error && D->fragment[E->fragment_index].ref_name == NULL)
      if (D->reference_field == NULL)
        D->reference_field = E;
  } else if (strcmp(in_cols[1], "LINCOM") == 0)
    E = _GD_ParseLincom(D, in_cols, n_cols, P, format_file, linenum, me,
        standards, pedantic, &is_dot);
  else if (strcmp(in_cols[1], "LINTERP") == 0)
    E = _GD_ParseLinterp(D, in_cols, n_cols, P, format_file, linenum, me,
        standards, pedantic, &is_dot);
  else if (strcmp(in_cols[1], "MULTIPLY") == 0 && (!pedantic || standards >= 2))
    E = _GD_ParseMultiply(D, in_cols, n_cols, P, format_file, linenum, me,
        standards, pedantic, &is_dot);
  else if (strcmp(in_cols[1], "BIT") == 0)
    E = _GD_ParseBit(D, 0, in_cols, n_cols, P, format_file, linenum, me,
        standards, pedantic, &is_dot);
  else if (strcmp(in_cols[1], "PHASE") == 0 && (!pedantic || standards >= 4))
    E = _GD_ParsePhase(D, in_cols, n_cols, P, format_file, linenum, me,
        standards, pedantic, &is_dot);
  else if (strcmp(in_cols[1], "POLYNOM") == 0 && (!pedantic || standards >= 7))
    E = _GD_ParsePolynom(D, in_cols, n_cols, P, format_file, linenum, me,
        standards, pedantic, &is_dot);
  else if (strcmp(in_cols[1], "SBIT") == 0 && (!pedantic || standards >= 7))
    E = _GD_ParseBit(D, 1, in_cols, n_cols, P, format_file, linenum, me,
        standards, pedantic, &is_dot);
  else if (strcmp(in_cols[1], "DIVIDE") == 0 && (!pedantic || standards >= 8))
    E = _GD_ParseDivide(D, in_cols, n_cols, P, format_file, linenum, me,
        standards, pedantic, &is_dot);
  else if (strcmp(in_cols[1], "RECIP") == 0 && (!pedantic || standards >= 8))
    E = _GD_ParseRecip(D, in_cols, n_cols, P, format_file, linenum, me,
        standards, pedantic, &is_dot);
  else if (strcmp(in_cols[1], "WINDOW") == 0 && (!pedantic || standards >= 9))
    E = _GD_ParseWindow(D, in_cols, n_cols, P, format_file, linenum, me,
        standards, pedantic, &is_dot);
  else if (strcmp(in_cols[1], "MPLEX") == 0 && (!pedantic || standards >= 9))
    E = _GD_ParseMplex(D, in_cols, n_cols, P, format_file, linenum, me,
        standards, pedantic, &is_dot);
  else if (strcmp(in_cols[1], "CONST") == 0 && (!pedantic || standards >= 6))
    E = _GD_ParseConst(D, in_cols, n_cols, P, format_file, linenum, me,
        standards, pedantic, &is_dot);
  else if (strcmp(in_cols[1], "CARRAY") == 0 && (!pedantic || standards >= 8))
    E = _GD_ParseCarray(D, in_cols, n_cols, P, format_file, linenum, me,
        standards, pedantic, &is_dot, outstring, tok_pos);
  else if (strcmp(in_cols[1], "STRING") == 0 && (!pedantic || standards >= 6))
    E = _GD_ParseString(D, in_cols, n_cols, P, format_file, linenum, me,
        standards, pedantic, &is_dot);
  else if (standards <= GD_DIRFILE_STANDARDS_VERSION || pedantic)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_LINE, format_file, linenum,
        NULL);

  if (is_dot) {
    ptr = _GD_Realloc(D, D->dot_list, (D->n_dot + 1) * sizeof(gd_entry_t*));
    if (ptr == NULL) {
      dreturn ("%p", NULL);
      return NULL;
    }
    D->dot_list = (gd_entry_t **)ptr;
  }

  if (insert && D->error == GD_E_OK && E != NULL) {
    /* the Format file fragment index */
    unsigned int u;

    E->fragment_index = me;

    /* Check for duplicate */
    if (_GD_FindField(D, E->field, D->entry, D->n_entries, 0, &u)) {
      if (~flags & GD_IGNORE_DUPS)
        _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_DUPLICATE, format_file,
            linenum, E->field);
      _GD_FreeE(D, E, 1);
      dreturn("%p", NULL);
      return NULL;
    }

    /* Initialse the meta counts */
    if (P != NULL) {
      E->e->n_meta = -1;
      E->e->p.parent = P;
      /* there is no need to sort this list */
      ptr = _GD_Realloc(D, P->e->p.meta_entry, (P->e->n_meta + 1) *
          sizeof(gd_entry_t*));
      if (ptr == NULL) {
        _GD_FreeE(D, E, 1);
        dreturn ("%p", NULL);
        return NULL;
      }
      P->e->p.meta_entry = (gd_entry_t **)ptr;
      P->e->p.meta_entry[P->e->n_meta++] = E;

      D->n_meta++;
      P->e->n[_GD_EntryIndex(E->field_type)]++;
    } else
      D->n[_GD_EntryIndex(E->field_type)]++;

    /* update the dot list if necessary */
    if (is_dot) {
      D->dot_list[D->n_dot++] = E;
      qsort(D->dot_list, D->n_dot, sizeof(gd_entry_t*), _GD_EntryCmp);
    }

    /* sort */
    _GD_InsertSort(D, E, u);
    D->n_entries++;
  }

  /* return the entry object if we either:
   * - didn't insert the object into the list of fields (ie. we were called
   *   by [m]alter_spec)
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
int _GD_Tokenise(DIRFILE *restrict D, const char *restrict instring,
    char **outstring, const char **pos, int tok_want, char **in_cols,
    const char *restrict format_file, int linenum, int standards, int pedantic)
{
  const char* ip;
  char* op;
  int n_cols = 0;
  int escaped_char = 0;
  int quotated = 0;
  int ws = 1;
  uint32_t accumulator = 0;
  int n_acc = 0;
  int acc_mode = ACC_MODE_NONE;

  dtrace("%p, \"%s\", %p, %p, %i, %p, \"%s\", %i, %i, %i", D, instring,
      outstring, pos, tok_want, in_cols, format_file, linenum, standards,
      pedantic);

  op = *outstring = _GD_Strdup(D, instring);
  if (op == NULL) {
    dreturn("%i", 0);
    return 0;
  }
  *op = '\0';

  /* tokenise the line */
  for (ip = instring; *ip != '\0'; ++ip) {
    if (escaped_char) {
      if (ws) {
        if (n_cols >= tok_want)
          break; /* Ignore trailing data on the line */
        in_cols[n_cols++] = op;
        ws = 0;
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
          acc_mode = ACC_MODE_NONE;
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
          acc_mode = ACC_MODE_NONE;
        } else if (acc_mode == ACC_MODE_UTF8 && (n_acc == 7 ||
              accumulator > 0x10FF || !isxdigit(*ip)))
        {
          if (_GD_UTF8Encode(D, format_file, linenum, &op, accumulator))
            break; /* syntax error */

          if (!isxdigit(*ip))
            ip--; /* rewind */
          escaped_char = 0;
          acc_mode = ACC_MODE_NONE;
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
      if (*ip == '\\' && (!pedantic || standards >= 6))
        escaped_char = 1;
      else if (*ip == '"' && (!pedantic || standards >= 6)) {
        if ((quotated = !quotated)) {
          if (ws) {
            if (n_cols >= tok_want)
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
          if (n_cols >= tok_want)
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

  if (pos)
    *pos = ip;

  dreturn("%i", n_cols);
  return n_cols;
}

/* _GD_ParseAlias: set up an alias
 */
static void _GD_ParseAlias(DIRFILE *restrict D, char **restrict name,
    const char *restrict target, int me, int line, int standards, int pedantic,
    int ignore_dups)
{
  gd_entry_t **new_meta_list = NULL;
  gd_entry_t *E, *P = NULL;
  unsigned int u;
  int offset;
  void *ptr;

  dtrace("%p, \"%s\", \"%s\", %i, %i, %i, %i, %i", D, *name, target, me, line,
      standards, pedantic, ignore_dups);

  P = _GD_CheckParent(D, name, me, line);
  if (D->error) {
    dreturnvoid();
    return;
  }

  ptr = _GD_Realloc(D, D->entry, (D->n_entries + 1) * sizeof(gd_entry_t*));
  if (ptr == NULL) {
    dreturnvoid();
    return;
  }
  D->entry = (gd_entry_t **)ptr;

  E = (gd_entry_t *)_GD_Malloc(D, sizeof(gd_entry_t));
  if (E == NULL) {
    dreturnvoid();
    return;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = (struct _gd_private_entry *)_GD_Malloc(D,
      sizeof(struct _gd_private_entry));
  if (E->e == NULL) {
    free(E);
    dreturnvoid();
    return;
  }
  memset(E->e, 0, sizeof(struct _gd_private_entry));

  E->field_type = GD_ALIAS_ENTRY;
  E->fragment_index = me;
  E->in_fields[0] = _GD_Strdup(D, target);

  E->field = _GD_MungeFromFrag(D, P, me, *name, &offset);
  if (E->field && _GD_ValidateField(E->field + offset, standards, pedantic, 0,
        NULL))
  {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, D->fragment[me].cname,
        line, *name);
  }

  if (D->error != GD_E_OK) {
    _GD_FreeE(D, E, 1);
    dreturnvoid();
    return;
  }

  /* Check for duplicate */
  if (_GD_FindField(D, E->field, D->entry, D->n_entries, 0, &u)) {
    if (!ignore_dups)
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_DUPLICATE, D->fragment[me].cname,
          line, E->field);
    _GD_FreeE(D, E, 1);
    dreturnvoid();
    return;
  }

  /* Allocate where necessary */
  if (P) {
    new_meta_list = (gd_entry_t **)_GD_Realloc(D, P->e->p.meta_entry,
        (P->e->n_meta + 1) * sizeof(gd_entry_t*));
    if (new_meta_list == NULL) {
      _GD_FreeE(D, E, 1);
      dreturnvoid();
      return;
    }
  }

  /* Nothing from here on may fail */

  /* Initialse the meta counts */
  if (P) {
    E->e->n_meta = -1;
    E->e->p.parent = P;
    /* there is no need to sort this list */
    P->e->p.meta_entry = new_meta_list;
    P->e->p.meta_entry[P->e->n_meta++] = E;

    D->n_meta++;
  }

  /* sort */
  _GD_InsertSort(D, E, u);
  D->n_entries++;

  dreturnvoid();
}

/* _GD_ParseDirective: Actually parse a single format file line.
 *       Returns 1 if a match was made.
 */
static int _GD_ParseDirective(DIRFILE *restrict D, char **in_cols, int n_cols,
    int me, int *restrict standards, int linenum, char **restrict ref_name,
    unsigned long *restrict flags, char **outstring, const char *tok_pos)
{
  const char* ptr;
  char *munged_code;
  int i, dummy, matched = 0;
  int pedantic = *flags & GD_PEDANTIC;
  gd_entry_t *E = NULL;

  dtrace("%p, %p, %i, %u, %p, %i, %p, %p, %p, %p", D, in_cols, n_cols, me,
      standards, linenum, ref_name, flags, outstring, tok_pos);

  /* Starting with Standards Version 8, the forward slash is required. */
  if (*standards >= 8 && pedantic && in_cols[0][0] != '/') {
    dreturn("%i", 0);
    return 0;
  }

  /* set up for possibly slashed reserved words */
  ptr = in_cols[0];
  if (*standards >= 5 || !pedantic)
    if (in_cols[0][0] == '/')
      ptr++;

  if (!pedantic && in_cols[0][0] != '/' && n_cols > 2)
    /* check for a field spec masquerading as a directive */
    if ((strcmp(in_cols[1], "RAW") == 0) ||
        (strcmp(in_cols[1], "LINCOM") == 0) ||
        (strcmp(in_cols[1], "BIT") == 0) ||
        (strcmp(in_cols[1], "LINTERP") == 0) ||
        (strcmp(in_cols[1], "PHASE") == 0) ||
        (strcmp(in_cols[1], "MULTIPLY") == 0) ||
        (strcmp(in_cols[1], "SBIT") == 0) ||
        (strcmp(in_cols[1], "POLYNOM") == 0) ||
        (strcmp(in_cols[1], "STRING") == 0) ||
        (strcmp(in_cols[1], "CONST") == 0))
    {
      dreturn("%i", 0);
      return 0;
    }

  switch(ptr[0]) {
    case 'A':
      if (strcmp(ptr, "ALIAS") == 0 && (!pedantic || *standards >= 9)) {
        matched = 1;
        if (n_cols < 3) {
          _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, D->fragment[me].cname,
              linenum, NULL);
          break;
        }

        _GD_ParseAlias(D, in_cols + 1, in_cols[2], me, linenum, *standards,
            pedantic, *flags & GD_IGNORE_DUPS);
      }
      break;
    case 'E':
      if (strcmp(ptr, "ENCODING") == 0 && (!pedantic || *standards >= 6)) {
        matched = 1;
        if (!(*flags & GD_FORCE_ENCODING)) {
          D->fragment[me].encoding = GD_ENC_UNSUPPORTED;
          for (i = 0; i < GD_N_SUBENCODINGS - 1; ++i)
            if (strcmp(in_cols[1], _gd_ef[i].ffname) == 0) {
              D->fragment[me].encoding = _gd_ef[i].scheme;
              free(D->fragment[me].enc_data);
              if (n_cols > 2 && _gd_ef[i].flags & GD_EF_EDAT)
                D->fragment[me].enc_data = _GD_Strdup(D, in_cols[2]);
              else
                D->fragment[me].enc_data = NULL;
              break;
            }
        }
      } else if (strcmp(ptr, "ENDIAN") == 0 && (!pedantic || *standards >= 5)) {
        matched = 1;
        if (!(*flags & GD_FORCE_ENDIAN)) {
          if (strcmp(in_cols[1], "big") == 0)
            D->fragment[me].byte_sex = GD_BIG_ENDIAN;
          else if (strcmp(in_cols[1], "little") == 0)
            D->fragment[me].byte_sex = GD_LITTLE_ENDIAN;
          else
            _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_ENDIAN,
            D->fragment[me].cname, linenum, NULL);
          if (n_cols > 2 && (!pedantic || *standards >= 8)) {
            if (strcmp(in_cols[2], "arm") == 0) {
#if ! defined(ARM_ENDIAN_DOUBLES)
              D->fragment[me].byte_sex |= GD_ARM_FLAG;
#endif
            } else
              _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_ENDIAN,
                  D->fragment[me].cname, linenum, NULL);
          }
#ifdef ARM_ENDIAN_DOUBLES
          else
            D->fragment[me].byte_sex |= GD_ARM_FLAG;
#endif
        }
      }
      break;
    case 'F':
      if (strcmp(ptr, "FRAMEOFFSET") == 0 && (!pedantic || *standards >= 1)) {
        matched = 1;
        D->fragment[me].frame_offset = gd_strtoll(in_cols[1], NULL,
            (!pedantic || *standards >= 9) ? 0 : 10);
      }
      break;
    case 'H':
      if (strcmp(ptr, "HIDDEN") == 0 && (!pedantic || *standards >= 9)) {
        matched = 1;
        munged_code = _GD_MungeFromFrag(D, NULL, me, in_cols[1], &dummy);
        if (munged_code)
          E = _GD_FindField(D, munged_code, D->entry, D->n_entries, 0, NULL);
        free(munged_code);

        if (E == NULL)
          _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_NO_FIELD,
              D->fragment[me].cname, linenum, in_cols[1]);
        else if (E->fragment_index != me)
          _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_LOCATION,
              D->fragment[me].cname, linenum, in_cols[1]);
        else {
          E->hidden = 1;

          /* update counts */
          if (E->e->n_meta != -1) {
            D->n[_GD_EntryIndex(E->field_type)]--;
            D->n_hidden++;
          } else {
            gd_entry_t *P = (gd_entry_t*)E->e->p.parent;
            P->e->n_hidden++;
            P->e->n[_GD_EntryIndex(E->field_type)]--;
          }
        }
      }
      break;
    case 'I':
      if (strcmp(ptr, "INCLUDE") == 0 && (!pedantic || *standards >= 3)) {
        matched = 1;
        unsigned long subflags = D->fragment[me].encoding
          | D->fragment[me].byte_sex | (*flags & (GD_PEDANTIC | GD_PERMISSIVE
                | GD_FORCE_ENDIAN | GD_FORCE_ENCODING | GD_IGNORE_DUPS
                | GD_IGNORE_REFS));

        int frag = _GD_Include(D, in_cols[1], D->fragment[me].cname, linenum,
            ref_name, me, (n_cols > 2) ? in_cols[2] : NULL,
            (n_cols > 3) ? in_cols[3] : NULL, standards, &subflags, 0);

        if ((pedantic = subflags & GD_PEDANTIC))
          *flags |= GD_PEDANTIC;
        if (frag != -1)
          D->fragment[me].vers |= D->fragment[frag].vers;
      }
      break;
    case 'M':
      if (strcmp(ptr, "META") == 0 && (!pedantic || *standards >= 6)) {
        matched = 1;
        munged_code = _GD_MungeFromFrag(D, NULL, me, in_cols[1], &dummy);
        if (munged_code) {
          E = _GD_FindField(D, munged_code, D->entry, D->n_entries, 0, NULL);
          free(munged_code);
        }

        if (E == NULL)
          _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_NO_FIELD,
              D->fragment[me].cname, linenum, in_cols[1]);
        else if (E->field_type == GD_ALIAS_ENTRY)
          _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_ALIAS, D->fragment[me].cname,
              linenum, in_cols[1]);
        else if (E->fragment_index != me)
          _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_LOCATION,
              D->fragment[me].cname, linenum, in_cols[1]);
        else if (E->e->n_meta == -1)
          _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_META_META,
              D->fragment[me].cname, linenum, in_cols[1]);
        else if (n_cols < 4)
          _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, D->fragment[me].cname,
              linenum, NULL);
        else
          _GD_ParseFieldSpec(D, n_cols - 2, in_cols + 2, E,
              D->fragment[me].cname, linenum, me, *standards, 0, *flags, 1,
              outstring, tok_pos);
      }
      break;
    case 'P':
      if (strcmp(ptr, "PROTECT") == 0 && (!pedantic || *standards >= 6)) {
        matched = 1;
        if (strcmp(in_cols[1], "none") == 0)
          D->fragment[me].protection = GD_PROTECT_NONE;
        else if (strcmp(in_cols[1], "format") == 0)
          D->fragment[me].protection = GD_PROTECT_FORMAT;
        else if (strcmp(in_cols[1], "data") == 0)
          D->fragment[me].protection = GD_PROTECT_DATA;
        else if (strcmp(in_cols[1], "all") == 0)
          D->fragment[me].protection = GD_PROTECT_ALL;
        else
          _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_PROTECT,
              D->fragment[me].cname, linenum, in_cols[1]);
      }
      break;
    case 'R':
      if (strcmp(ptr, "REFERENCE") == 0 && (!pedantic || *standards >= 6)) {
        matched = 1;
        free(*ref_name);
        *ref_name = _GD_MungeFromFrag(D, NULL, me, in_cols[1], &dummy);
      }
      break;
    case 'V':
      if (strcmp(ptr, "VERSION") == 0 && (!pedantic || *standards >= 5)) {
        matched = 1;
        *standards = atoi(in_cols[1]);
        if (!pedantic && ~(*flags) & GD_PERMISSIVE)
          *flags |= (pedantic = GD_PEDANTIC);
        if (pedantic)
          D->fragment[me].vers |= 1ULL << *standards;
      }
      break;
  }

  dreturn("%i", matched);
  return matched;
}

/* Resolve and record an alias, taking care of loops */
static gd_entry_t *_GD_ResolveAlias(DIRFILE *restrict D, gd_entry_t *restrict E)
{
  gd_entry_t *T = NULL;
  char *munged_code;
  int dummy;

  dtrace("%p, %p", D, E);

  if (++D->recurse_level >= GD_MAX_RECURSE_LEVEL) {
    _GD_SetError(D, GD_E_RECURSE_LEVEL, GD_E_RECURSE_CODE, NULL, 0, E->field);
    D->recurse_level--;
    dreturn("%p", NULL);
    return NULL;
  }

  /* Find the target */
  munged_code = _GD_MungeFromFrag(D, NULL, E->fragment_index, E->in_fields[0],
      &dummy);
  if (munged_code) {
    T = _GD_FindField(D, munged_code, D->entry, D->n_entries, 0, NULL);
    free(munged_code);
  }

  /* Aliases store the ulitmate target in entry[0] and the direct link
   * in entry[1].
   */
  E->e->entry[0] = E->e->entry[1] = T;
  if (T) {
    if (T->field_type == GD_ALIAS_ENTRY) {
      if (T->e->entry[0])
        T = T->e->entry[0];
      else
        T = _GD_ResolveAlias(D, T);
    }

    E->e->entry[0] = T;
  }

  D->recurse_level--;
  dreturn("%p", D->error ? NULL : E->e->entry[0]);
  return D->error ? NULL : E->e->entry[0];
}

void _GD_UpdateAliases(DIRFILE *D)
{
  unsigned u;

  dtrace("%p", D);

  for (u = 0; u < D->n_entries; ++u)
    if (D->entry[u]->field_type == GD_ALIAS_ENTRY &&
        D->entry[u]->e->entry[1] == NULL)
    {
      _GD_ResolveAlias(D, D->entry[u]);
    }

  dreturnvoid();
}

/* _GD_ParseFragment: Parse each line of the fragment.
 *
 *       Returns NULL unless this fragment contains a REFERENCE directive.
 */
char *_GD_ParseFragment(FILE *restrict fp, DIRFILE *restrict D, int me,
    int *restrict standards, unsigned long *restrict flags, int resolve)
{
  char *instring = NULL;
  char *outstring = NULL;
  const char *tok_pos = NULL;
  char *in_cols[MAX_IN_COLS];
  int linenum = 0;
  char* ref_name = NULL;
  int n_cols;
  size_t n;
  int match = 0;
  int rescan = 0;
  int se_action = GD_SYNTAX_ABORT;
  gd_entry_t* first_raw = NULL;
  gd_parser_data_t pdata;

  int saved_error = 0;
  int saved_suberror = 0;
  int saved_line = 0;
  char* saved_token = NULL;

  dtrace("%p, %p, %i, %p, %p, %i", fp, D, me, standards, flags, resolve);

  /* start parsing */
  while (rescan || (instring = _GD_GetLine(fp, &n, &linenum))) {
    rescan = 0;
    n_cols = _GD_Tokenise(D, instring, &outstring, &tok_pos, MAX_IN_COLS,
        in_cols, D->fragment[me].cname, linenum, *standards,
        *flags & GD_PEDANTIC);

    if (n_cols == 0) {/* a blank line */
      free(outstring);
      continue;
    }
    else if (n_cols < 2) /* any valid, non-blank line has at least two tokens */
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, D->fragment[me].cname,
          linenum, NULL);

    if (D->error == GD_E_OK)
      match = _GD_ParseDirective(D, in_cols, n_cols, me, standards, linenum,
          &ref_name, flags, &outstring, tok_pos);

    if (D->error == GD_E_OK && !match)
      first_raw = _GD_ParseFieldSpec(D, n_cols, in_cols, NULL,
          D->fragment[me].cname, linenum, me, *standards, 0, *flags, 1,
          &outstring, tok_pos);

    if (D->error == GD_E_FORMAT) {
      /* we guarantee a buffer size of at least GD_MAX_LINE_LENGTH */
      if (n < GD_MAX_LINE_LENGTH) {
        char *ptr = (char *)_GD_Realloc(D, instring, GD_MAX_LINE_LENGTH);
        if (ptr == NULL)
          break;
        instring = ptr;
      }
      /* call the callback for this error */
      if (D->sehandler != NULL) {
        pdata.dirfile = D;
        pdata.suberror = D->suberror;
        pdata.linenum = linenum;
        pdata.filename = D->fragment[me].cname;
        pdata.line = instring;
        pdata.buflen = n;
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
              saved_token = _GD_Strdup(D, D->error_string);
            saved_error = 1;
          }
          /* ... and continue parsing (fallthrough) */
        case GD_SYNTAX_IGNORE:
          _GD_ClearError(D); /* ignore this line, continue parsing */
          break;
        case GD_SYNTAX_RESCAN:
          _GD_ClearError(D);
          rescan = 1; /* rescan the modified instring */
          if (pdata.line != instring) {
            /* a new line was malloc'd by the caller, delete our old one. */
            free(instring);
            instring = pdata.line;
            n = pdata.buflen;
          }
          break;
        default:
          /* improper callback response */
          _GD_SetError(D, GD_E_CALLBACK, 0, NULL, se_action, NULL);
          break;
      }
    }

    /* clean up */
    free(outstring);
    if (!rescan)
      free(instring);

    if (D->error)
      break; /* abort in the event of a non-syntax error */
  }
  if (instring == NULL && errno == EOVERFLOW)
    _GD_SetError(D, GD_E_LINE_TOO_LONG, 0, D->fragment[me].cname, linenum,
        NULL);

  /* restore a saved error, if we have one */
  if (!D->error && saved_error) {
    _GD_SetError(D, GD_E_FORMAT, saved_suberror, D->fragment[me].cname,
        saved_line, saved_token);
    free(saved_token);
  }

  /* Set reference */
  if (!D->error) {
    if (ref_name != NULL)
      D->fragment[me].ref_name = _GD_Strdup(D, ref_name);
    else if (first_raw != NULL)
      D->fragment[me].ref_name = _GD_Strdup(D, first_raw->field);
  }

  /* resolve aliases, if requested */
  if (resolve && !D->error)
    _GD_UpdateAliases(D);

  dreturn("%p", ref_name);
  return ref_name;
}

/* public access to the GetData tokeniser */
char *gd_tokenise(DIRFILE *D, const char *string) gd_nothrow
{
  char *outstring, *in_col;
  int n_cols;

  dtrace("%p, \"%s\"", D, string);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_ClearError(D);

  if (string)
    D->tok_pos = string;
  else if (D->tok_pos == NULL) {
    _GD_SetError(D, GD_E_ARGUMENT, GD_E_ARG_NODATA, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  /* tokenise! */
  n_cols = _GD_Tokenise(D, D->tok_pos, &outstring, &D->tok_pos, 1, &in_col,
      "gd_tokenise()", 0, D->standards, D->flags & GD_PERMISSIVE);

  if (D->error || n_cols < 1) {
    D->tok_pos = NULL;
    free(outstring);
    outstring = NULL;
  }

  /* in_col invariably points to outstring, so just let the caller worry about
   * cleaning up.  Ha! */
  dreturn("\"%s\"", outstring);
  return outstring;
}
/* vim: ts=2 sw=2 et tw=80
*/
