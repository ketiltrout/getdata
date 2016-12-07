/* Copyright (C) 2002-2005 C. Barth Netterfield
 * Copyright (C) 2005-2016 D. V. Wiebe
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

  if (type[0] != '\0' && type[1] == '\0' && (!pedantic || standards < 8))
    t = _GD_LegacyType(type[0]);
  else if (pedantic && standards < 5)
    t = GD_UNKNOWN;

  else if (type[0] == 'I') {
    if (strcmp(type, "INT8") == 0)
      t = GD_INT8;
    else if (strcmp(type, "INT16") == 0)
      t = GD_INT16;
    else if (strcmp(type, "INT32") == 0)
      t = GD_INT32;
    else if (strcmp(type, "INT64") == 0)
      t = GD_INT64;
  } else if (type[0] == 'U') {
    if (strcmp(type, "UINT8") == 0)
      t = GD_UINT8;
    else if (strcmp(type, "UINT16") == 0)
      t = GD_UINT16;
    else if (strcmp(type, "UINT32") == 0)
      t = GD_UINT32;
    else if (strcmp(type, "UINT64") == 0)
      t = GD_UINT64;
  } else if (type[0] == 'F') {
    if (strcmp(type, "FLOAT64") == 0)
      t = GD_FLOAT64;
    else if (strcmp(type, "FLOAT32") == 0)
      t = GD_FLOAT32;
    else if (strcmp(type, "FLOAT") == 0)
      t = GD_FLOAT32;
  } else if (strcmp(type, "DOUBLE") == 0)
    t = GD_FLOAT64;
  else if (pedantic && standards < 7)
    t = GD_UNKNOWN;

  else if (strcmp(type, "COMPLEX128") == 0)
    t = GD_COMPLEX128;
  else if (strcmp(type, "COMPLEX64") == 0)
    t = GD_COMPLEX64;

  dreturn("0x%X", t);
  return t;
}

static gd_windop_t _GD_WindOp(const char *op)
{
  gd_windop_t o = GD_WINDOP_UNK;
  dtrace("\"%s\"", op);

  switch (op[0]) {
    case 'E':
      if (op[1] == 'Q' && op[2] == 0)
        o = GD_WINDOP_EQ;
      break;
    case 'L':
      if (op[1] == 'T' && op[2] == 0)
        o = GD_WINDOP_LT;
      else if (op[1] == 'E' && op[2] == 0)
        o = GD_WINDOP_LE;
      break;
    case 'G':
      if (op[1] == 'T' && op[2] == 0)
        o = GD_WINDOP_GT;
      else if (op[1] == 'E' && op[2] == 0)
        o = GD_WINDOP_GE;
      break;
    case 'N':
      if (op[1] == 'E' && op[2] == 0)
        o = GD_WINDOP_NE;
      break;
    case 'S':
      if (op[1] == 'E' && op[2] == 'T' && op[3] == 0)
        o = GD_WINDOP_SET;
      break;
    case 'C':
      if (op[1] == 'L' && op[2] == 'R' && op[3] == 0)
        o = GD_WINDOP_CLR;
      break;
  }

  dreturn("%i", o);
  return o;
}

/* Convert a string to a number.  The non-NULL pointers provided dictate the
 * returned type.  Returns -1 if it wasn't a number or -2 if it was but was
 * out of range */
int _GD_TokToNum(const char *restrict token, int standards, int pedantic,
    double *re, double *im, uint64_t *u, int64_t *i)
{
  int64_t ir = 0, ii = 0;
  uint64_t ur = 0, ui = 0;
  double dr = 0, di = 0;
  char *endptr = NULL;
  gd_type_t rt = GD_UNKNOWN, it = GD_UNKNOWN;
  const int base = (!pedantic || standards >= 9) ? 0 : 10;

  dtrace("\"%s\", %i, %i, %p, %p, %p, %p", token, standards, pedantic, re, im,
      u, i);

  /* we have to run the token through both strtod and strtol because neither of
   * these handles all the allowed syntax */

  /* the real part */
  errno = 0;
  ir = gd_strtoll(token, &endptr, base);
  if (!errno && (*endptr == '\0' || *endptr == ';'))
    rt = GD_INT64;

  if (rt == GD_UNKNOWN && errno == ERANGE) {
    /* could be a uint64 > 2**63 */
    errno = 0;
    ur = gd_strtoull(token, &endptr, base);
    if (!errno && (*endptr == '\0' || *endptr == ';'))
      rt = GD_UINT64;
  }

  if (rt == GD_UNKNOWN) { /* all that's left to try */
    errno = 0;
    dr = gd_strtod(token, &endptr);

    if (!errno && (*endptr == '\0' || *endptr == ';'))
      rt = GD_FLOAT64;
  }

  /* check for real-part success */
  if (rt == GD_UNKNOWN) {
    dreturn("%i", -1);
    return -1;
  }

  /* if there's no semicolon, set the imaginary part to zero */
  if (*endptr == '\0') {
    it = GD_NULL;
  } else {
    /* convert imaginary part the same way */
    token = endptr + 1;
    errno = 0;
    ii = gd_strtoll(token, &endptr, base);
    if (!errno && *endptr == '\0')
      it = (ii == 0) ? GD_NULL : GD_INT64;

    if (it == GD_UNKNOWN && errno == ERANGE) {
      /* could be a uint64 > 2**63 */
      errno = 0;
      ui = gd_strtoull(token, &endptr, base);
      if (!errno && *endptr == '\0')
        it = (ui == 0) ? GD_NULL : GD_UINT64;
    }

    if (it == GD_UNKNOWN) { /* all that's left to try */
      errno = 0;
      di = gd_strtod(token, &endptr);

      if (!errno && *endptr == '\0')
        it = (di == 0) ? GD_NULL : GD_FLOAT64;
    }

    /* check for imaginary-part success */
    if (it == GD_UNKNOWN) {
      dreturn("%i", -1);
      return -1;
    }

    if (!im && it != GD_NULL) { /* reject unwanted complex value */
      dreturn("%i", -2);
      return -2;
    }
  }

  /* return the desired value, if possible */
  if (re) { /* float or complex */
    if (rt == GD_FLOAT64)
      *re = dr;
    else if (rt == GD_INT64)
      *re = ir;
    else
      *re = ur;

    if (im) { /* complex */
      if (it == GD_NULL)
        *im = 0;
      else if (it == GD_FLOAT64)
        *im = di;
      else if (it == GD_INT64)
        *im = ii;
      else if (it == GD_UINT64)
        *im = ui;
    }
  } else if (u) { /* unsigned int -- reject negative values */
    if (rt == GD_UINT64)
      *u = ur;
    else if (rt == GD_INT64) {
      if (ir < 0) {
        dreturn("%i", -2);
        return -2;
      }
      *u = ir;
    } else {
      if (dr < 0) {
        dreturn("%i", -2);
        return -2;
      }
      *u = dr;
    }
  } else if (i) { /* int */
    if (rt == GD_INT64)
      *i = ir;
    else if (rt == GD_FLOAT64)
      *i = dr;
    else { /* this must be an overflow */
      dreturn("%i", -2);
      return -2;
    }
  }

  dreturn("%i", 0);
  return 0;
}

/* Compose a subfield code from a parent code and a subfield name -- this
 * used to be done in _GD_MungeCode */
static char *_GD_SubfieldCode(DIRFILE *D, const gd_entry_t *P, const char *code,
    size_t *offset)
{
  size_t len, len_par;
  char *new_code;

  dtrace("%p, %p, \"%s\", %p", D, P, code, offset);

  if (code == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  len = strlen(code);
  len_par = strlen(P->field) + 1;

  if ((new_code = _GD_Malloc(D, len_par + len + 1)) == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  strcpy(new_code, P->field);
  new_code[len_par - 1] = '/';
  strcpy(new_code + len_par, code);

  if (offset)
    *offset = len_par;

  dreturn("\"%s\" (%" PRIuSIZE ")", new_code, offset ? *offset : 0);
  return new_code;
}

/* Create a field code using the prefix and suffix of the given fragment and the
 * current namespace.
 *
 * Returns a newly malloc'd code, or NULL on error.  For subfields, offset is
 * the offset to the character following / for a metafield.  For non-subfields,
 * it's the length of the namespace. */
static char *_GD_CodeFromFrag(DIRFILE *restrict D,
    const struct parser_state *restrict p, const gd_entry_t *restrict P,
    int me, const char *code, size_t *offset)
{
  char *new_code;

  dtrace("%p, %p, %p, %i, \"%s\", %p", D, p, P, me, code, offset);

  if (P)
    new_code = _GD_SubfieldCode(D, P, code, offset);
  else
    new_code = _GD_BuildCode(D, me, p->ns, p->nsl, code,
        p->pedantic && p->standards < 10, offset);

  dreturn("\"%s\" (%" PRIuSIZE ")", new_code, offset ? *offset : 0);
  return new_code;
}

/* Canonicalise a field code used to specify an input field (scalar or vector)
 */
static char *_GD_InputCode(DIRFILE *D, const struct parser_state *restrict p,
    int me, const char *token)
{
  char *code;

  dtrace("%p, %p, %i, \"%s\"", D, p, me, token);

  code = _GD_BuildCode(D, me, p->ns, p->nsl, token,
      p->pedantic && p->standards <= 5, NULL);

  dreturn("\"%s\"", code);
  return code;
}

/* Create E->field; frees the entry and returns non-zero on error. */
static int _GD_SetField(DIRFILE *restrict D,
    const struct parser_state *restrict p, gd_entry_t *restrict E,
    const gd_entry_t *restrict P, int me, const char *restrict name)
{
  size_t offset;

  dtrace("%p, %p{%s,%i}, %p, %p, %i, \"%s\"", D, p, p->file, p->line, E, P, me,
      name);

  E->field = _GD_CodeFromFrag(D, p, P, me, name, &offset);

  /* The _GD_ValidateField call differs based on whether we're dealing with
   * a subfield or not because the meaning of offset is different */
  if (E->field) {
    int bad_code;
    if (P)
      bad_code = _GD_ValidateField(E->field + offset, 0, p->standards,
          p->pedantic, GD_VF_NAME);
    else
      bad_code = _GD_ValidateField(E->field, offset, p->standards, p->pedantic,
          GD_VF_NAME);

    if (bad_code)
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, p->file, p->line,
          name);
  }

  if (D->error) {
    _GD_FreeE(D, E, 1);
    dreturn("%i", 1);
    return 1;
  }

  E->e->len = strlen(E->field);
  dreturn("%i", 0);
  return 0;
}

/* Returns a newly malloc'd string containing the scalar field name, or NULL on
 * numeric literal or error */
static char *_GD_SetScalar(DIRFILE *restrict D,
    const struct parser_state *restrict p, const char *restrict token,
    void *restrict data, gd_type_t type, int me, int *restrict index,
    unsigned *restrict flags)
{
  char *lt, *ptr;
  int i;

  dtrace("%p, %p, \"%s\", %p, 0x%X, %p, %p", D, p, token, data, type, index,
      flags);

  /* try a numerical conversion */
  if (type & GD_COMPLEX) {
    double re, im;

    i = _GD_TokToNum(token, p->standards, p->pedantic, &re, &im, NULL, NULL);

    /* _GD_TokToNum can't return -2 for complex return type */
    if (i == -1) { /* assume it's a field name */
      ptr = _GD_InputCode(D, p, me, token);
      if (D->error) {
        dreturn("%p", NULL);
        return NULL;
      }
      goto carray_check;
    }

    /* flag */
    if (im && flags)
      *flags |= GD_EN_COMPSCAL;

    /* store the number */
    if (type == GD_COMPLEX128) {
      *(double *)data = re;
      *((double *)data + 1) = im;
    } else if (type == GD_COMPLEX64) {
      *(float *)data = (float)re;
      *((float *)data + 1) = (float)im;
    } else
      _GD_InternalError(D);
  } else if (type & GD_IEEE754) {
    double d;

    i = _GD_TokToNum(token, p->standards, p->pedantic, &d, NULL, NULL, NULL);

    if (i == -2) { /* malformed number */
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_LITERAL, p->file, p->line,
          token);
      dreturn("%p", NULL);
      return NULL;
    } else if (i == -1) { /* assume it's a field name */
      ptr = _GD_InputCode(D, p, me, token);
      if (D->error) {
        dreturn("%p", NULL);
        return NULL;
      }
      goto carray_check;
    }

    /* store the number */
    if (type == GD_FLOAT64)
      *(double *)data = d;
    else if (type == GD_FLOAT32)
      *(float *)data = (float)d;
    else
      _GD_InternalError(D);
  } else if (type & GD_SIGNED) {
    int64_t lli;

    i = _GD_TokToNum(token, p->standards, p->pedantic, NULL, NULL, NULL, &lli);

    if (i == -2) { /* malformed number */
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_LITERAL, p->file, p->line,
          token);
      dreturn("%p", NULL);
      return NULL;
    } else if (i == -1) { /* assume it's a field name */
      ptr = _GD_InputCode(D, p, me, token);
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
    uint64_t ulli;

    i = _GD_TokToNum(token, p->standards, p->pedantic, NULL, NULL, &ulli, NULL);

    if (i == -2) { /* malformed number */
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_LITERAL, p->file, p->line,
          token);
      dreturn("%p", NULL);
      return NULL;
    } else if (i == -1) { /* assume it's a field name */
      ptr = _GD_InputCode(D, p, me, token);
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
  /* look for < > delimeters */
  *index = -1;
  for (lt = ptr; *lt; ++lt) {
    if (*lt == '<') {
      char *endptr = NULL;

      *lt = '\0';
      *index = (int)strtol(lt + 1, &endptr, 0);

      if (*endptr != '>') {
        /* invalid CARRAY index, undo the elision */
        *lt = '<';
        *index = -1;
      } else
        break;
    }
  }

  dreturn("\"%s\" (%i)", ptr, *index);
  return ptr;
}

/* _GD_ParseRaw: parse a RAW entry in the format file
*/
static gd_entry_t *_GD_ParseRaw(DIRFILE *restrict D,
    const struct parser_state *restrict p,
    char *gd_restrict_arr in_cols[MAX_IN_COLS], int n_cols,
    const gd_entry_t *restrict parent, int me)
{
  gd_entry_t *E;

  dtrace("%p, %p, %p, %i, %p, %i", D, p, in_cols, n_cols, parent, me);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, p->file, p->line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  /* META RAW fields are prohibited */
  if (parent != NULL) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_METARAW, p->file, p->line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  E = (gd_entry_t *)_GD_Malloc(D, sizeof(gd_entry_t));
  if (E == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = (struct gd_private_entry_ *)_GD_Malloc(D,
      sizeof(struct gd_private_entry_));
  if (E->e == NULL) {
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct gd_private_entry_));

  E->field_type = GD_RAW_ENTRY;
  E->e->u.raw.file[0].idata = E->e->u.raw.file[1].idata = -1;
  E->e->u.raw.file[0].subenc = GD_ENC_UNKNOWN; /* don't know the encoding
                                                    subscheme yet */

  if (_GD_SetField(D, p, E, NULL, me, in_cols[0])) {
    dreturn("%p", NULL);
    return NULL;
  }

  if (p->pedantic && p->standards <= 5)
    E->flags |= GD_EN_EARLY;

  E->e->u.raw.filebase = _GD_Strdup(D, in_cols[0]);
  E->EN(raw,data_type) = _GD_RawType(in_cols[2], p->standards, p->pedantic);
  E->e->u.raw.size = GD_SIZE(E->EN(raw,data_type));

  if (E->e->u.raw.size == 0)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_TYPE, p->file, p->line,
        in_cols[2]);
  else if ((E->scalar[0] = _GD_SetScalar(D, p, in_cols[3], &E->EN(raw,spf),
          GD_UINT_TYPE, me, E->scalar_ind, NULL)) == NULL)
  {
    E->flags |= GD_EN_CALC;
    if (!D->error && E->EN(raw,spf) <= 0)
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_SPF, p->file, p->line,
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
    const struct parser_state *restrict p,
    char *gd_restrict_arr in_cols[MAX_IN_COLS], int n_cols,
    const gd_entry_t *restrict parent, int me)
{
  int i;
  char* ptr = NULL;
  gd_entry_t *E;

  dtrace("%p, %p, %p, %i, %p, %i", D, p, in_cols, n_cols, parent, me);

  if (n_cols < 3) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, p->file, p->line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  E = (gd_entry_t *)_GD_Malloc(D, sizeof(gd_entry_t));
  if (E == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = (struct gd_private_entry_ *)_GD_Malloc(D,
      sizeof(struct gd_private_entry_));
  if (E->e == NULL) {
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct gd_private_entry_));

  E->field_type = GD_LINCOM_ENTRY;

  if (_GD_SetField(D, p, E, parent, me, in_cols[0])) {
    dreturn("%p", NULL);
    return NULL;
  }

  E->flags |= GD_EN_CALC;
  E->EN(lincom,n_fields) = (int)(strtol(in_cols[2], &ptr, 10));
  if (*ptr != '\0') {
    E->EN(lincom,n_fields) = (n_cols - 2) / 3;
    /* assume <n> has been omitted */
    if (n_cols % 3 != 2 || E->EN(lincom,n_fields) < 1 || E->EN(lincom,n_fields)
        > GD_MAX_LINCOM)
    {
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, p->file, p->line, NULL);
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
  {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_FIELDS, p->file, p->line,
        in_cols[2]);
  } else if (n_cols < E->EN(lincom,n_fields) * 3 + 3)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, p->file, p->line, NULL);
  else
    for (i = 0; i < E->EN(lincom,n_fields); i++) {
      E->in_fields[i] = _GD_InputCode(D, p, me, in_cols[i * 3 + 3]);
      E->scalar[i] = _GD_SetScalar(D, p, in_cols[i * 3 + 4],
          &E->EN(lincom,cm)[i], GD_COMPLEX128, me, E->scalar_ind + i,
          &E->flags);
      E->EN(lincom,m)[i] = creal(E->EN(lincom,cm)[i]);
      E->scalar[i + GD_MAX_LINCOM] = _GD_SetScalar(D, p, in_cols[i * 3 + 5],
          &E->EN(lincom,cb)[i], GD_COMPLEX128, me,
          E->scalar_ind + i + GD_MAX_LINCOM, &E->flags);
      E->EN(lincom,b)[i] = creal(E->EN(lincom,cb)[i]);

      if (E->scalar[i] != NULL || E->scalar[i + GD_MAX_LINCOM] != NULL)
        E->flags &= ~GD_EN_CALC;
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
    const struct parser_state *restrict p,
    char *gd_restrict_arr in_cols[MAX_IN_COLS], int n_cols,
    const gd_entry_t *restrict parent, int me)
{
  gd_entry_t *E;

  dtrace("%p, %p, %p, %i, %p, %i", D, p, in_cols, n_cols, parent, me);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, p->file, p->line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  E = (gd_entry_t *)_GD_Malloc(D, sizeof(gd_entry_t));
  if (E == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = (struct gd_private_entry_ *)_GD_Malloc(D,
      sizeof(struct gd_private_entry_));
  if (E->e == NULL) {
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct gd_private_entry_));

  E->field_type = GD_LINTERP_ENTRY;
  E->in_fields[0] = NULL;
  E->e->entry[0] = NULL;
  E->flags |= GD_EN_CALC;
  E->EN(linterp,table) = NULL;

  if (_GD_SetField(D, p, E, parent, me, in_cols[0])) {
    dreturn("%p", NULL);
    return NULL;
  }

  E->in_fields[0] = _GD_InputCode(D, p, me, in_cols[2]);
  E->e->u.linterp.table_len = -1; /* linterp file not read yet */

  E->EN(linterp,table) = _GD_Strdup(D, in_cols[3]);

  if (D->error) {
    _GD_FreeE(D, E, 1);
    E = NULL;
  }

  dreturn("%p", E);
  return E;
}

/* _GD_ParseYoke: parse a field specified by two input fields only (MULTIPLY,
 * DIVIDE, INDIR, SINDIR)
*/
static gd_entry_t *_GD_ParseYoke(DIRFILE *restrict D, gd_entype_t type,
    const struct parser_state *restrict p,
    char *gd_restrict_arr in_cols[MAX_IN_COLS], int n_cols,
    const gd_entry_t *restrict parent, int me)
{
  gd_entry_t *E;

  dtrace("%p, 0x%X, %p, %p, %i, %p, %i", D, type, p, in_cols, n_cols, parent,
      me);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, p->file, p->line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  E = (gd_entry_t *)_GD_Malloc(D, sizeof(gd_entry_t));
  if (E == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = (struct gd_private_entry_ *)_GD_Malloc(D,
      sizeof(struct gd_private_entry_));
  if (E->e == NULL) {
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct gd_private_entry_));

  E->field_type = type;
  E->in_fields[0] = E->in_fields[1] = NULL;
  E->e->entry[0] = E->e->entry[1] = NULL;
  E->flags |= GD_EN_CALC;

  if (_GD_SetField(D, p, E, parent, me, in_cols[0])) {
    dreturn("%p", NULL);
    return NULL;
  }

  E->in_fields[0] = _GD_InputCode(D, p, me, in_cols[2]);
  E->in_fields[1] = _GD_InputCode(D, p, me, in_cols[3]);

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
    const struct parser_state *restrict p,
    char *gd_restrict_arr in_cols[MAX_IN_COLS], int n_cols,
    const gd_entry_t *restrict parent, int me)
{
  gd_entry_t *E;

  dtrace("%p, %p, %p, %i, %p, %i", D, p, in_cols, n_cols, parent, me);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, p->file, p->line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  E = (gd_entry_t *)_GD_Malloc(D, sizeof(gd_entry_t));
  if (E == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = (struct gd_private_entry_ *)_GD_Malloc(D,
      sizeof(struct gd_private_entry_));
  if (E->e == NULL) {
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct gd_private_entry_));

  E->field_type = GD_RECIP_ENTRY;
  E->in_fields[0] = NULL;
  E->e->entry[0] = NULL;

  if (_GD_SetField(D, p, E, parent, me, in_cols[0])) {
    dreturn("%p", NULL);
    return NULL;
  }

  E->in_fields[0] = _GD_InputCode(D, p, me, in_cols[2]);

  E->scalar[0] = _GD_SetScalar(D, p, in_cols[3], &E->EN(recip,cdividend),
      GD_COMPLEX128, me, E->scalar_ind, &E->flags);
  E->EN(recip,dividend) = creal(E->EN(recip,cdividend));

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
    const struct parser_state *restrict p,
    char *gd_restrict_arr in_cols[MAX_IN_COLS], int n_cols,
    const gd_entry_t *restrict parent, int me)
{
  gd_entry_t *E;

  dtrace("%p, %p, %p, %i, %p, %i", D, p, in_cols, n_cols, parent, me);

  if (n_cols < 6) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, p->file, p->line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  E = (gd_entry_t *)_GD_Malloc(D, sizeof(gd_entry_t));
  if (E == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = (struct gd_private_entry_ *)_GD_Malloc(D,
      sizeof(struct gd_private_entry_));
  if (E->e == NULL) {
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct gd_private_entry_));

  E->field_type = GD_WINDOW_ENTRY;

  if (_GD_SetField(D, p, E, parent, me, in_cols[0])) {
    dreturn("%p", NULL);
    return NULL;
  }

  E->in_fields[0] = _GD_InputCode(D, p, me, in_cols[2]);
  E->in_fields[1] = _GD_InputCode(D, p, me, in_cols[3]);

  E->EN(window,windop) = _GD_WindOp(in_cols[4]);
  if (E->EN(window,windop) == GD_WINDOP_UNK) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_WINDOP, p->file, p->line,
        in_cols[4]);
    _GD_FreeE(D, E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  switch (E->EN(window,windop)) {
    case GD_WINDOP_EQ:
    case GD_WINDOP_NE:
      E->scalar[0] = _GD_SetScalar(D, p, in_cols[5], &E->EN(window,threshold.i),
          GD_INT64, me, E->scalar_ind, NULL);
      break;
    case GD_WINDOP_SET:
    case GD_WINDOP_CLR:
      E->scalar[0] = _GD_SetScalar(D, p, in_cols[5], &E->EN(window,threshold.u),
          GD_UINT64, me, E->scalar_ind, NULL);
      break;
    default:
      E->scalar[0] = _GD_SetScalar(D, p, in_cols[5], &E->EN(window,threshold.r),
          GD_FLOAT64, me, E->scalar_ind, NULL);
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
    const struct parser_state *restrict p,
    char *gd_restrict_arr in_cols[MAX_IN_COLS], int n_cols,
    const gd_entry_t *restrict parent, int me)
{
  gd_entry_t *E;

  dtrace("%p, %p, %p, %i, %p, %i", D, p, in_cols, n_cols, parent, me);

  if (n_cols < 5) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, p->file, p->line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  E = (gd_entry_t *)_GD_Malloc(D, sizeof(gd_entry_t));
  if (E == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = (struct gd_private_entry_ *)_GD_Malloc(D,
      sizeof(struct gd_private_entry_));
  if (E->e == NULL) {
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct gd_private_entry_));

  E->field_type = GD_MPLEX_ENTRY;

  if (_GD_SetField(D, p, E, parent, me, in_cols[0])) {
    dreturn("%p", NULL);
    return NULL;
  }

  E->in_fields[0] = _GD_InputCode(D, p, me, in_cols[2]);
  E->in_fields[1] = _GD_InputCode(D, p, me, in_cols[3]);

  E->scalar[0] = _GD_SetScalar(D, p, in_cols[4], &E->EN(mplex,count_val),
      GD_INT_TYPE, me, E->scalar_ind, NULL);

  /* the period, if present */
  if (n_cols > 5) {
    E->scalar[1] = _GD_SetScalar(D, p, in_cols[5], &E->EN(mplex,period),
        GD_INT_TYPE, me, E->scalar_ind + 1, NULL);
    if (E->scalar[1] == NULL && E->EN(mplex,period) < 0)
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_MPLEXVAL, p->file, p->line,
        in_cols[5]);
  }

  if (D->error != GD_E_OK) {
    _GD_FreeE(D, E, 1);
    E = NULL;
  }

  dreturn("%p", E);
  return E;
}

/* _GD_ParseBit: parse BIT entry in format file.
*/
static gd_entry_t *_GD_ParseBit(DIRFILE *restrict D, int is_signed,
    const struct parser_state *restrict p,
    char *gd_restrict_arr in_cols[MAX_IN_COLS], int n_cols,
    const gd_entry_t *restrict parent, int me)
{
  gd_entry_t *E;

  dtrace("%p, %i, %p, %p, %i, %p, %i", D, is_signed, p, in_cols, n_cols, parent,
      me);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, p->file, p->line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  E = (gd_entry_t *)_GD_Malloc(D, sizeof(gd_entry_t));
  if (E == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = (struct gd_private_entry_ *)_GD_Malloc(D,
      sizeof(struct gd_private_entry_));
  if (E->e == NULL) {
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct gd_private_entry_));

  E->field_type = (is_signed) ? GD_SBIT_ENTRY : GD_BIT_ENTRY;
  E->in_fields[0] = NULL;
  E->e->entry[0] = NULL;
  E->flags |= GD_EN_CALC;

  if (_GD_SetField(D, p, E, parent, me, in_cols[0])) {
    dreturn("%p", NULL);
    return NULL;
  }

  E->in_fields[0] = _GD_InputCode(D, p, me, in_cols[2]);
  E->scalar[0] = _GD_SetScalar(D, p, in_cols[3], &E->EN(bit,bitnum),
      GD_INT_TYPE, me, E->scalar_ind, NULL);

  if (n_cols > 4)
    E->scalar[1] = _GD_SetScalar(D, p, in_cols[4], &E->EN(bit,numbits),
        GD_INT_TYPE, me, E->scalar_ind + 1, NULL);
  else
    E->EN(bit,numbits) = 1;

  if (E->scalar[0] != NULL || E->scalar[1] != NULL)
    E->flags &= ~GD_EN_CALC;

  if (E->scalar[1] == NULL && E->EN(bit,numbits) < 1)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_NUMBITS, p->file, p->line, NULL);
  else if (E->scalar[0] == NULL && E->EN(bit,bitnum) < 0)
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BITNUM, p->file, p->line, NULL);
  else if ((E->flags & GD_EN_CALC) &&
      E->EN(bit,bitnum) + E->EN(bit,numbits) - 1 > 63)
  {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BITSIZE, p->file, p->line, NULL);
  }

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
    const struct parser_state *restrict p,
    char *gd_restrict_arr in_cols[MAX_IN_COLS], int n_cols,
    const gd_entry_t *restrict parent, int me)
{
  gd_entry_t *E;

  dtrace("%p, %p, %p, %i, %p, %i", D, p, in_cols, n_cols, parent, me);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, p->file, p->line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  E = (gd_entry_t *)_GD_Malloc(D, sizeof(gd_entry_t));
  if (E == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = (struct gd_private_entry_ *)_GD_Malloc(D,
      sizeof(struct gd_private_entry_));
  if (E->e == NULL) {
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct gd_private_entry_));

  E->field_type = GD_PHASE_ENTRY;
  E->in_fields[0] = NULL;
  E->e->entry[0] = NULL;

  if (_GD_SetField(D, p, E, parent, me, in_cols[0])) {
    dreturn("%p", NULL);
    return NULL;
  }

  E->in_fields[0] = _GD_InputCode(D, p, me, in_cols[2]);

  if ((E->scalar[0] = _GD_SetScalar(D, p, in_cols[3], &E->EN(phase,shift),
          GD_INT64, me, E->scalar_ind, NULL)) == NULL)
  {
    E->flags |= GD_EN_CALC;
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
    const struct parser_state *restrict p,
    char *gd_restrict_arr in_cols[MAX_IN_COLS], int n_cols,
    const gd_entry_t *restrict parent, int me)
{
  int i;
  gd_entry_t *E;

  dtrace("%p, %p, %p, %i, %p, %i", D, p, in_cols, n_cols, parent, me);

  if (n_cols < 5) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, p->file, p->line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  E = (gd_entry_t *)_GD_Malloc(D, sizeof(gd_entry_t));
  if (E == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = (struct gd_private_entry_ *)_GD_Malloc(D,
      sizeof(struct gd_private_entry_));
  if (E->e == NULL) {
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct gd_private_entry_));

  E->field_type = GD_POLYNOM_ENTRY;
  if (_GD_SetField(D, p, E, parent, me, in_cols[0])) {
    dreturn("%p", NULL);
    return NULL;
  }

  E->EN(polynom,poly_ord) = n_cols - 4;

  /* the legacy ignore-trailing-tokens "feature" */
  if (E->EN(polynom,poly_ord) > GD_MAX_POLYORD)
    E->EN(polynom,poly_ord) = GD_MAX_POLYORD;

  E->flags |= GD_EN_CALC;

  E->in_fields[0] = _GD_InputCode(D, p, me, in_cols[2]);

  for (i = 0; i <= E->EN(polynom,poly_ord); i++) {
    E->scalar[i] = _GD_SetScalar(D, p, in_cols[i + 3], &E->EN(polynom,ca)[i],
        GD_COMPLEX128, me, E->scalar_ind + i, &E->flags);
    E->EN(polynom,a)[i] = creal(E->EN(polynom,ca)[i]);

    if (E->scalar[i] != NULL)
      E->flags &= ~GD_EN_CALC;
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
    case GD_STRING:
      _GD_InternalError(D);
  }

  dreturn("0x%X", GD_NULL);
  return GD_NULL;
}

/* _GD_ParseConst: parse CONST entry in formats file.
*/
static gd_entry_t *_GD_ParseConst(DIRFILE *restrict D,
    const struct parser_state *restrict p,
    char *gd_restrict_arr in_cols[MAX_IN_COLS], int n_cols,
    const gd_entry_t *restrict parent, int me)
{
  int offset;
  char* ptr;
  gd_type_t type;
  gd_entry_t *E;

  dtrace("%p, %p, %p, %i, %p", D, p, in_cols, n_cols, parent);

  if (n_cols < 4) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, p->file, p->line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  E = (gd_entry_t *)_GD_Malloc(D, sizeof(gd_entry_t));
  if (E == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = (struct gd_private_entry_ *)_GD_Malloc(D,
      sizeof(struct gd_private_entry_));
  if (E->e == NULL) {
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct gd_private_entry_));

  E->field_type = GD_CONST_ENTRY;
  E->flags |= GD_EN_CALC;

  if (_GD_SetField(D, p, E, parent, me, in_cols[0])) {
    dreturn("%p", NULL);
    return NULL;
  }

  E->EN(scalar,const_type) = _GD_RawType(in_cols[2], p->standards, p->pedantic);
  E->EN(scalar,array_len) = -1;

  if (GD_SIZE(E->EN(scalar,const_type)) == 0 || E->EN(scalar,const_type) & 0x40)
  {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_TYPE, p->file, p->line,
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

  ptr = _GD_SetScalar(D, p, in_cols[3], E->e->u.scalar.d, type, me, &offset,
      NULL);
  if (ptr) {
    free(ptr);
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_LITERAL, p->file, p->line,
        in_cols[3]);
  }

  if (D->error) {
    _GD_FreeE(D, E, 1);
    E = NULL;
  }

  dreturn("%p", E);
  return E;
}

/* _GD_ParseArray: parse [CS]ARRAY entry in formats file.
*/
static gd_entry_t *_GD_ParseArray(DIRFILE *restrict D, int string,
    const struct parser_state *restrict p, char *in_cols[MAX_IN_COLS],
    int n_cols, const gd_entry_t *restrict parent, int me, char **outstring,
    const char *tok_pos)
{
  int offset, c, first, s;
  size_t n = 0, data_size = 0, new_size;
  gd_type_t t = GD_NULL;
  char* ptr;
  void *data = NULL;
  gd_entry_t *E;

  dtrace("%p, %i, %p, %p, %i, %p, %i, %p, %p", D, string, p, in_cols, n_cols,
      parent, me, outstring, tok_pos);

  /* CARRAYs have a data_type token which SARRAYs lack */
  if (n_cols < 4 - string) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, p->file, p->line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  E = (gd_entry_t *)_GD_Malloc(D, sizeof(gd_entry_t));
  if (E == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = (struct gd_private_entry_ *)_GD_Malloc(D,
      sizeof(struct gd_private_entry_));
  if (E->e == NULL) {
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct gd_private_entry_));

  E->field_type = string ? GD_SARRAY_ENTRY : GD_CARRAY_ENTRY;
  E->flags |= GD_EN_CALC;

  if (_GD_SetField(D, p, E, parent, me, in_cols[0])) {
    dreturn("%p", NULL);
    return NULL;
  }

  if (string) {
    s = sizeof(const char *);
    first = 2;
  } else {
    E->EN(scalar,const_type) = _GD_RawType(in_cols[2], p->standards,
        p->pedantic);
    t = _GD_ConstType(D, E->EN(scalar,const_type));
    first = 3;
    s = GD_SIZE(t);

    if (GD_SIZE(E->EN(scalar,const_type)) == 0 || E->EN(raw,data_type) & 0x40) {
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_TYPE, p->file, p->line,
          in_cols[2]);
      _GD_FreeE(D, E, 1);
      dreturn("%p", NULL);
      return NULL;
    }
  }

  /* spool in the data */
  new_size = MAX_IN_COLS - first;

  for (;;) {
    if (data_size < new_size) {
      void *new_data = _GD_Realloc(D, data, new_size * s);
      if (new_data == NULL) {
        free(data);
        _GD_FreeE(D, E, 1);
        dreturn("%p", NULL);
        return NULL;
      }
      data = new_data;
      data_size = new_size;
    }

    for (c = first; c < n_cols; ++c) {
      if (n == GD_SIZE_T_MAX)
        break;

      if (string) {
        ((char**)data)[n++] = _GD_Strdup(D, in_cols[c]);

        if (D->error) {
          size_t i;

          for (i = 0; i < n; ++i)
            free(((char**)data)[i]);
          free(data);
          _GD_FreeE(D, E, 1);
          dreturn("%p", NULL);
          return NULL;
        }
      } else {
        ptr = _GD_SetScalar(D, p, in_cols[c], (char *)data + s * n++, t, me,
            &offset, NULL);

        if (ptr) {
          free(ptr);
          free(data);
          _GD_FreeE(D, E, 1);
          _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_LITERAL, p->file, p->line,
              in_cols[c]);
          dreturn("%p", NULL);
          return NULL;
        }
      }
    }

    if (n_cols < MAX_IN_COLS)
      break;

    /* get more tokens */
    free(*outstring);
    n_cols = _GD_Tokenise(D, p, tok_pos, outstring, &tok_pos, MAX_IN_COLS,
        in_cols);
    if (n_cols == 0 || D->error)
      break;
    first = 0;
    new_size = data_size + n_cols;
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
    const struct parser_state *restrict p,
    char *gd_restrict_arr in_cols[MAX_IN_COLS], int n_cols,
    const gd_entry_t *restrict parent, int me)
{
  gd_entry_t *E;

  dtrace("%p, %p, %p, %i, %p, %i", D, p, in_cols, n_cols, parent, me);

  if (n_cols < 3) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, p->file, p->line, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  E = (gd_entry_t *)_GD_Malloc(D, sizeof(gd_entry_t));
  if (E == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E, 0, sizeof(gd_entry_t));

  E->e = (struct gd_private_entry_ *)_GD_Malloc(D,
      sizeof(struct gd_private_entry_));
  if (E->e == NULL) {
    free(E);
    dreturn("%p", NULL);
    return NULL;
  }
  memset(E->e, 0, sizeof(struct gd_private_entry_));

  E->field_type = GD_STRING_ENTRY;
  E->e->u.string = _GD_Strdup(D, in_cols[2]);
  E->flags |= GD_EN_CALC;

  if (D->error) {
    _GD_FreeE(D, E, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  if (_GD_SetField(D, p, E, parent, me, in_cols[0])) {
    dreturn("%p", NULL);
    return NULL;
  }

  dreturn("%p", E);
  return E;
}

static int _GD_UTF8Encode(DIRFILE *restrict D, const char *restrict format_file,
    int linenum, char **restrict op, uint32_t value)
{
  dtrace("%p, %p, 0x%" PRIX32, D, op, value);

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
 * parser, but called from _GD_Add.  In this case, it never sets D->error.
 */
gd_entry_t *_GD_CheckParent(DIRFILE *restrict D,
    const struct parser_state *p, char **restrict name, size_t *len, int me)
{
  size_t plen = 0;
  char *cptr, *munged_code;
  gd_entry_t *P = NULL;

  dtrace("%p, %p, \"%s\", %p, %i", D, p, *name, len, me);

  for (cptr = *name + 1; *cptr != '\0'; ++cptr)
    if (*cptr == '/') {
      *cptr = '\0';
      if (me == -1) {
        plen = (cptr - *name);
        munged_code = malloc(plen + 1);
        if (munged_code)
          memcpy(munged_code, *name, plen + 1);
      } else {
        munged_code = _GD_CodeFromFrag(D, p, NULL, me, *name, NULL);
        if (munged_code)
          plen = strlen(munged_code);
      }

      if (munged_code) {
        P = _GD_FindField(D, munged_code, plen, D->entry, D->n_entries, 0,
            NULL);
        free(munged_code);
      }

      if (P == NULL) {
        if (me == -1) {
          *cptr = '/'; /* undo field munging; _GD_Add will conclude this is
                          a field with an illegal '/' */
          dreturn("%p", NULL);
          return NULL;
        }
        _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_NO_FIELD, p->file, p->line,
            *name);
      } else if (P->field_type == GD_ALIAS_ENTRY) {
        if (me == -1)
          P = P->e->entry[0]; /* just de-alias */
        else
          _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_ALIAS, p->file, p->line,
              *name);
      } else if (P->fragment_index != me && me != -1)
        _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_LOCATION, p->file, p->line,
            *name);

      if (D->error) {
        dreturn("%p", NULL);
        return NULL;
      }

      /* point name to the metafield name */
      *len -= (cptr - *name) + 1;
      *name = cptr + 1;
      break;
    }

  dreturn("%p (\"%s\", %" PRIuSIZE ")", P, *name, *len);
  return P;
}

/* _GD_ParseFieldSpec: Parse a format file line fragment containing a field
 * specification */
gd_entry_t *_GD_ParseFieldSpec(DIRFILE *restrict D,
    const struct parser_state *restrict p, int n_cols, char **in_cols,
    size_t len0, const gd_entry_t *restrict P, int me, int creat, int insert,
    char **outstring, const char *tok_pos)
{
  gd_entry_t* E = NULL;
  void *ptr;

  dtrace("%p, %p, %i, %p, %" PRIuSIZE ", %p, %i, %i, %i, %p, %p", D, p, n_cols,
      in_cols, len0, P, me, creat, insert, outstring, tok_pos);

  /* Check for barth-style metafield definition */
  if (P == NULL && GD_PVERS_GE(*p, 7)) {
    P = _GD_CheckParent(D, p, in_cols + 0, &len0, me);
    if (P) {
      E = _GD_ParseFieldSpec(D, p, n_cols, in_cols, len0, P, me, creat, insert,
          outstring, tok_pos);
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

  if (P == NULL && (strcmp(in_cols[0], "INDEX") == 0 || (p->pedantic &&
          p->standards < 6 && strcmp(in_cols[0], "FILEFRAM") == 0)))
    /* reserved field name */
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_RES_NAME, p->file, p->line,
        NULL);
  else
    switch (in_cols[1][0]) {
      case 'B':
        if (strcmp(in_cols[1], "BIT") == 0)
          E = _GD_ParseBit(D, 0, p, in_cols, n_cols, P, me);
        else
          goto NO_MATCH;
        break;
      case 'C':
        if (strcmp(in_cols[1], "CARRAY") == 0 && GD_PVERS_GE(*p, 8))
          E = _GD_ParseArray(D, 0, p, in_cols, n_cols, P, me, outstring,
              tok_pos);
        else if (strcmp(in_cols[1], "CONST") == 0 && GD_PVERS_GE(*p, 6))
          E = _GD_ParseConst(D, p, in_cols, n_cols, P, me);
        else
          goto NO_MATCH;
        break;
      case 'D':
        if (strcmp(in_cols[1], "DIVIDE") == 0 && GD_PVERS_GE(*p, 8))
          E = _GD_ParseYoke(D, GD_DIVIDE_ENTRY, p, in_cols, n_cols, P, me);
        else
          goto NO_MATCH;
        break;
      case 'I':
        if (strcmp(in_cols[1], "INDIR") == 0 && GD_PVERS_GE(*p, 10))
          E = _GD_ParseYoke(D, GD_INDIR_ENTRY, p, in_cols, n_cols, P, me);
        else
          goto NO_MATCH;
        break;
      case 'L':
        if (strcmp(in_cols[1], "LINCOM") == 0)
          E = _GD_ParseLincom(D, p, in_cols, n_cols, P, me);
        else if (strcmp(in_cols[1], "LINTERP") == 0)
          E = _GD_ParseLinterp(D, p, in_cols, n_cols, P, me);
        else
          goto NO_MATCH;
        break;
      case 'M':
        if (strcmp(in_cols[1], "MPLEX") == 0 && GD_PVERS_GE(*p, 9))
          E = _GD_ParseMplex(D, p, in_cols, n_cols, P, me);
        else if (strcmp(in_cols[1], "MULTIPLY") == 0 && GD_PVERS_GE(*p, 2))
          E = _GD_ParseYoke(D, GD_MULTIPLY_ENTRY, p, in_cols, n_cols, P, me);
        else
          goto NO_MATCH;
        break;
      case 'P':
        if (strcmp(in_cols[1], "PHASE") == 0 && GD_PVERS_GE(*p, 4))
          E = _GD_ParsePhase(D, p, in_cols, n_cols, P, me);
        else if (strcmp(in_cols[1], "POLYNOM") == 0 && GD_PVERS_GE(*p, 7))
          E = _GD_ParsePolynom(D, p, in_cols, n_cols, P, me);
        else
          goto NO_MATCH;
        break;
      case 'R':
        if (strcmp(in_cols[1], "RAW") == 0) {
          E = _GD_ParseRaw(D, p, in_cols, n_cols, P, me);

          /* Create the binary file, if requested */
          if (!D->error && creat) {
            /* If this fragment is protected, we can't do anything */
            if (D->fragment[me].protection != GD_PROTECT_NONE)
              _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_DATA, NULL, 0,
                  D->fragment[me].cname);
            else
              _GD_InitRawIO(D, E, NULL, -1, NULL, 0,
                  GD_FILE_WRITE | GD_FILE_TOUCH, _GD_FileSwapBytes(D, E));

            if (D->error) {
              _GD_FreeE(D, E, 1);
              E = NULL;
            }
          }

          /* Is this the first raw field ever defined? */
          if (!D->error && D->fragment[E->fragment_index].ref_name == NULL)
            if (D->reference_field == NULL)
              D->reference_field = E;
        } else if (strcmp(in_cols[1], "RECIP") == 0 && GD_PVERS_GE(*p, 8))
          E = _GD_ParseRecip(D, p, in_cols, n_cols, P, me);
        else
          goto NO_MATCH;
        break;
      case 'S':
        if (strcmp(in_cols[1], "SBIT") == 0 && GD_PVERS_GE(*p, 7))
          E = _GD_ParseBit(D, 1, p, in_cols, n_cols, P, me);
        else if (strcmp(in_cols[1], "SINDIR") == 0 && GD_PVERS_GE(*p, 2))
          E = _GD_ParseYoke(D, GD_SINDIR_ENTRY, p, in_cols, n_cols, P, me);
        else if (strcmp(in_cols[1], "SARRAY") == 0 && GD_PVERS_GE(*p, 10))
          E = _GD_ParseArray(D, 1, p, in_cols, n_cols, P, me, outstring,
              tok_pos);
        else if (strcmp(in_cols[1], "STRING") == 0 && GD_PVERS_GE(*p, 6))
          E = _GD_ParseString(D, p, in_cols, n_cols, P, me);
        else
          goto NO_MATCH;
        break;
      case 'W':
        if (strcmp(in_cols[1], "WINDOW") == 0 && GD_PVERS_GE(*p, 9))
          E = _GD_ParseWindow(D, p, in_cols, n_cols, P, me);
        else
          goto NO_MATCH;
        break;
      default:
NO_MATCH:
        /* We ignore indecipherable lines if we seen a /VERSION greater than
         * what we know about and we're not in pedantic mode */
        if (p->standards <= GD_DIRFILE_STANDARDS_VERSION || p->pedantic)
          _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_LINE, p->file, p->line,
              NULL);
    }

  if (insert && D->error == GD_E_OK && E != NULL) {
    unsigned int u;

    /* Check for duplicate */
    if (_GD_FindField(D, E->field, E->e->len, D->entry, D->n_entries, 0, &u)) {
      if (~p->flags & GD_IGNORE_DUPS)
        _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_DUPLICATE, p->file, p->line,
            E->field);
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

      /* Nothing may fail from now on */

      P->e->p.meta_entry = (gd_entry_t **)ptr;
      P->e->p.meta_entry[P->e->n_meta++] = E;
    }

    /* the Format file fragment index */
    E->fragment_index = me;

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
int _GD_Tokenise(DIRFILE *restrict D, const struct parser_state *restrict p,
    const char *restrict instring, char **outstring, const char **pos,
    int tok_want, char **in_cols)
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

  dtrace("%p, %p, \"%s\", %p, %p, %i, %p", D, p, instring, outstring, pos,
      tok_want, in_cols);

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
            _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_CHARACTER, p->file,
                p->line, NULL);
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
            accumulator = accumulator * 16 + *ip - 'A' + 10;
          else
            accumulator = accumulator * 16 + *ip - 'a' + 10;
        }

        if (acc_mode == ACC_MODE_HEX && (n_acc == 2 || !isxdigit(*ip))) {
          if (accumulator == 0) {
            _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_CHARACTER, p->file,
                p->line, NULL);
            break;
          }

          *(op++) = (char)accumulator;

          if (!isxdigit(*ip))
            ip--; /* rewind */
          escaped_char = 0;
          acc_mode = ACC_MODE_NONE;
        } else if (acc_mode == ACC_MODE_UTF8 && (n_acc == 7 ||
              accumulator > 0x10FFFF || !isxdigit(*ip)))
        {
          if (_GD_UTF8Encode(D, p->file, p->line, &op, accumulator))
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
      if (*ip == '\\' && GD_PVERS_GE(*p, 6))
        escaped_char = 1;
      else if (*ip == '"' && GD_PVERS_GE(*p, 6)) {
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

  if (quotated || escaped_char) {
    if (*ip == 0 || *ip == '\n') {
      /* Unterminated token */
      if (D->error == 0)
        _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_UNTERM, p->file, p->line,
            NULL);
    } else {
      /* a partial tokenisation where we've landed on top of " or \ when
       * finishing up; back up a space */
      ip--;
    }
  }

  if (pos)
    *pos = ip;

  dreturn("%i", n_cols);
  return n_cols;
}

/* _GD_ParseAlias: set up an alias
 */
static void _GD_ParseAlias(DIRFILE *restrict D,
    const struct parser_state *restrict p, char **restrict name,
    const char *restrict target, int me)
{
  size_t len = strlen(*name);
  gd_entry_t **new_meta_list = NULL;
  gd_entry_t *E, *P = NULL;
  unsigned int u;
  void *ptr;

  dtrace("%p, %p, \"%s\", \"%s\", %i", D, p, *name, target, me);

  P = _GD_CheckParent(D, p, name, &len, me);
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

  E->e = (struct gd_private_entry_ *)_GD_Malloc(D,
      sizeof(struct gd_private_entry_));
  if (E->e == NULL) {
    free(E);
    dreturnvoid();
    return;
  }
  memset(E->e, 0, sizeof(struct gd_private_entry_));

  E->field_type = GD_ALIAS_ENTRY;
  E->fragment_index = me;
  E->in_fields[0] = _GD_InputCode(D, p, me, target);

  if (D->error) {
    _GD_FreeE(D, E, 1);
    dreturnvoid();
    return;
  }

  if (_GD_SetField(D, p, E, P, me, *name)) {
    dreturnvoid();
    return;
  }

  /* Check for duplicate */
  if (_GD_FindField(D, E->field, E->e->len, D->entry, D->n_entries, 0, &u)) {
    if (!(p->flags & GD_IGNORE_DUPS))
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_DUPLICATE, p->file, p->line,
          E->field);
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
  }

  /* sort */
  _GD_InsertSort(D, E, u);
  D->n_entries++;

  dreturnvoid();
}

static void _GD_ParseNamespace(DIRFILE *D, struct parser_state *restrict p,
    const char *ns)
{
  char *ptr;

  dtrace("%p, %p, \"%s\"", D, p, ns);

  /* Ignore a leading '.' -- namespaces given in a /NAMESPACE directive are
   * always relative to the rootspace
   */
  if (ns[0] == '.')
    ns++;

  if (ns[0] == '\0') {
    /* Revert to rootspace */
    free(p->ns);
    p->ns = NULL;
    p->nsl = 0;
    dreturnvoid();
    return;
  }

  /* check new namespace name */
  if (_GD_ValidateField(ns, 0, p->standards, p->pedantic, GD_VF_NS)) {
    _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, p->file, p->line, ns);
    dreturnvoid();
    return;
  }

  ptr = _GD_Strdup(D, ns);
  if (ptr == NULL) { /* alloc error */
    dreturnvoid();
    return;
  }

  free(p->ns);
  p->ns = ptr;
  p->nsl = strlen(ptr);

  /* strip trailing dot */
  if (p->ns[p->nsl - 1] == '.') {
    p->ns[p->nsl - 1] = 0;
    p->nsl--;
  }

  dreturn("(nil) [\"%s\", %" PRIuSIZE "]", p->ns, p->nsl);
}

/* _GD_ParseDirective: Actually parse a single format file line.
 *       Returns 1 if a match was made.
 */
static int _GD_ParseDirective(DIRFILE *D, struct parser_state *restrict p,
    char **in_cols, int n_cols, int me, char **restrict ref_name,
    char **outstring, const char *tok_pos)
{
  const char* ptr;
  char *munged_code;
  int i, matched = 0;
  gd_entry_t *E = NULL;

  dtrace("%p, %p, %p, %i, %i, %p, %p, %p", D, p, in_cols, n_cols, me, ref_name,
      outstring, tok_pos);

  /* Starting with Standards Version 8, the forward slash is required. */
  if (p->standards >= 8 && p->pedantic && in_cols[0][0] != '/') {
    dreturn("%i", 0);
    return 0;
  }

  /* set up for possibly slashed reserved words */
  ptr = in_cols[0];
  if (GD_PVERS_GE(*p, 5))
    if (in_cols[0][0] == '/')
      ptr++;

  if (!p->pedantic && in_cols[0][0] != '/' && n_cols > 2)
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
      if (strcmp(ptr, "ALIAS") == 0 && GD_PVERS_GE(*p, 9)) {
        matched = 1;
        if (n_cols < 3) {
          _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, p->file, p->line,
              NULL);
          break;
        }

        _GD_ParseAlias(D, p, in_cols + 1, in_cols[2], me);
      }
      break;
    case 'E':
      if (strcmp(ptr, "ENCODING") == 0 && GD_PVERS_GE(*p, 6)) {
        matched = 1;
        if (!(p->flags & GD_FORCE_ENCODING)) {
          D->fragment[me].encoding = GD_ENC_UNSUPPORTED;
          for (i = 0; i < GD_N_SUBENCODINGS - 1; ++i)
            if (strcmp(in_cols[1], _GD_ef[i].ffname) == 0) {
              D->fragment[me].encoding = _GD_ef[i].scheme;
              free(D->fragment[me].enc_data);
              if (n_cols > 2 && _GD_ef[i].flags & GD_EF_EDAT)
                D->fragment[me].enc_data = _GD_Strdup(D, in_cols[2]);
              else
                D->fragment[me].enc_data = NULL;
              break;
            }
        }
      } else if (strcmp(ptr, "ENDIAN") == 0 && GD_PVERS_GE(*p, 5)) {
        matched = 1;
        if (!(p->flags & GD_FORCE_ENDIAN)) {
          if (strcmp(in_cols[1], "big") == 0)
            D->fragment[me].byte_sex = GD_BIG_ENDIAN;
          else if (strcmp(in_cols[1], "little") == 0)
            D->fragment[me].byte_sex = GD_LITTLE_ENDIAN;
          else
            _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_ENDIAN, p->file, p->line,
                NULL);
          if (n_cols > 2 && GD_PVERS_GE(*p, 8)) {
            if (strcmp(in_cols[2], "arm") == 0) {
#if ! defined(ARM_ENDIAN_DOUBLES)
              D->fragment[me].byte_sex |= GD_ARM_FLAG;
#endif
            } else
              _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_ENDIAN, p->file, p->line,
                  NULL);
          }
#ifdef ARM_ENDIAN_DOUBLES
          else
            D->fragment[me].byte_sex |= GD_ARM_FLAG;
#endif
        }
      }
      break;
    case 'F':
      if (strcmp(ptr, "FRAMEOFFSET") == 0 && GD_PVERS_GE(*p, 1)) {
        matched = 1;
        D->fragment[me].frame_offset = gd_strtoll(in_cols[1], NULL,
            GD_PVERS_GE(*p, 9) ? 0 : 10);
      }
      break;
    case 'H':
      if (strcmp(ptr, "HIDDEN") == 0 && GD_PVERS_GE(*p, 9)) {
        matched = 1;
        munged_code = _GD_CodeFromFrag(D, p, NULL, me, in_cols[1], NULL);
        if (munged_code) {
          E = _GD_FindField(D, munged_code, strlen(munged_code), D->entry,
              D->n_entries, 0, NULL);
          free(munged_code);
        }

        if (E == NULL)
          _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_NO_FIELD, p->file, p->line,
              in_cols[1]);
        else if (E->fragment_index != me)
          _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_LOCATION, p->file, p->line,
              in_cols[1]);
        else
          E->flags |= GD_EN_HIDDEN;
      }
      break;
    case 'I':
      if (strcmp(ptr, "INCLUDE") == 0 && GD_PVERS_GE(*p, 3)) {
        char *new_ref = NULL;
        int frag;
        unsigned long oldflags = p->flags;

        matched = 1;

        p->flags = D->fragment[me].encoding | D->fragment[me].byte_sex |
          (p->flags & (GD_PEDANTIC | GD_PERMISSIVE | GD_FORCE_ENDIAN |
                       GD_FORCE_ENCODING | GD_IGNORE_DUPS | GD_IGNORE_REFS));

        frag = _GD_Include(D, p, in_cols[1], &new_ref, me,
            (n_cols > 2) ? in_cols[2] : NULL, (n_cols > 3) ? in_cols[3] : NULL,
            0);

        if (new_ref) {
          free(*ref_name);
          *ref_name = new_ref;
        }

        p->flags = oldflags;
        if (p->pedantic)
          p->flags |= GD_PEDANTIC;
        if (frag != -1)
          D->fragment[me].vers |= D->fragment[frag].vers;
      }
      break;
    case 'M':
      if (strcmp(ptr, "META") == 0 && GD_PVERS_GE(*p, 6)) {
        matched = 1;
        munged_code = _GD_CodeFromFrag(D, p, NULL, me, in_cols[1], NULL);
        if (munged_code) {
          E = _GD_FindField(D, munged_code, strlen(munged_code), D->entry,
              D->n_entries, 0, NULL);
          free(munged_code);
        }

        if (E == NULL)
          _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_NO_FIELD, p->file, p->line,
              in_cols[1]);
        else if (E->field_type == GD_ALIAS_ENTRY)
          _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_ALIAS, p->file, p->line,
              in_cols[1]);
        else if (E->fragment_index != me)
          _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_LOCATION, p->file, p->line,
              in_cols[1]);
        else if (E->e->n_meta == -1)
          _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_META_META, p->file, p->line,
              in_cols[1]);
        else if (n_cols < 4)
          _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, p->file, p->line,
              NULL);
        else
          _GD_ParseFieldSpec(D, p, n_cols - 2, in_cols + 2, strlen(in_cols[2]),
              E, me, 0, 1, outstring, tok_pos);
      }
      break;
    case 'N':
      if (strcmp(ptr, "NAMESPACE") == 0 && GD_PVERS_GE(*p, 10)) {
        matched = 1;
        _GD_ParseNamespace(D, p, in_cols[1]);
      }
      break;
    case 'P':
      if (strcmp(ptr, "PROTECT") == 0 && GD_PVERS_GE(*p, 6)) {
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
          _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_PROTECT, p->file, p->line,
              in_cols[1]);
      }
      break;
    case 'R':
      if (strcmp(ptr, "REFERENCE") == 0 && GD_PVERS_GE(*p, 6)) {
        matched = 1;
        free(*ref_name);
        *ref_name = _GD_InputCode(D, p, me, in_cols[1]);
      }
      break;
    case 'V':
      if (strcmp(ptr, "VERSION") == 0 && GD_PVERS_GE(*p, 5)) {
        matched = 1;
        p->standards = atoi(in_cols[1]);
        if (!p->pedantic && ~(p->flags) & GD_PERMISSIVE)
          p->flags |= (p->pedantic = GD_PEDANTIC);
        if (p->pedantic)
          D->fragment[me].vers |= 1ULL << p->standards;
      }
      break;
  }

  dreturn("%i", matched);
  return matched;
}

/* Resolve and record an alias, taking care of loops */
static gd_entry_t *_GD_ResolveAlias(DIRFILE *restrict D, const gd_entry_t *base,
    gd_entry_t *E)
{
  gd_entry_t *T = NULL;

  dtrace("%p, %p, %p(%s)", D, base, E, E->field);

  /* Find the target */
  T = _GD_FindField(D, E->in_fields[0], strlen(E->in_fields[0]), D->entry,
      D->n_entries, 0, NULL);

  /* Aliases store the ulitmate target in entry[0] and the direct link in
   * entry[1] */
  E->e->entry[1] = T;
  if (T) {
    if (T->field_type == GD_ALIAS_ENTRY) {
      if (T->e->entry[0])
        T = T->e->entry[0];
      else if (base == T) /* loop */
        T = NULL;
      else
        T = _GD_ResolveAlias(D, base, T);
    }

  }

  E->e->entry[0] = T;

  if (D->error) {
    dreturn("%p", NULL);
    return NULL;
  }

  dreturn("%p (\"%s\" > \"%s\" > \"%s\")", E->e->entry[0], E->field,
      E->e->entry[1] ? E->e->entry[1]->field : "",
      E->e->entry[0] ? E->e->entry[0]->field : "");
  return E->e->entry[0];
}

void _GD_UpdateAliases(DIRFILE *D, int reset)
{
  unsigned u;

  dtrace("%p, %i", D, reset);

  if (reset)
    for (u = 0; u < D->n_entries; ++u)
      if (D->entry[u]->field_type == GD_ALIAS_ENTRY)
        D->entry[u]->e->entry[0] = D->entry[u]->e->entry[1] = NULL;

  for (u = 0; u < D->n_entries; ++u)
    if (D->entry[u]->field_type == GD_ALIAS_ENTRY &&
        D->entry[u]->e->entry[1] == NULL)
    {
      _GD_ResolveAlias(D, D->entry[u], D->entry[u]);
    }

  dreturnvoid();
}

/* _GD_ParseFragment: Parse each line of the fragment.
 *
 *       Returns NULL unless this fragment contains a REFERENCE directive.
 */
char *_GD_ParseFragment(FILE *restrict fp, DIRFILE *D, struct parser_state *p,
    int me, int resolve)
{
  char *instring = NULL;
  char *outstring = NULL;
  const char *tok_pos = NULL;
  char *in_cols[MAX_IN_COLS];
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

  dtrace("%p, %p, %p, %i, %i", fp, D, p, me, resolve);

  /* update the parser */
  p->line = 0;
  p->file = D->fragment[me].cname;

  /* start parsing */
  while (rescan || (instring = _GD_GetLine(fp, &n, &p->line))) {
    rescan = 0;
    n_cols = _GD_Tokenise(D, p, instring, &outstring, &tok_pos, MAX_IN_COLS,
        in_cols);

    if (n_cols == 0) {/* a blank line */
      free(outstring);
      continue;
    }
    else if (n_cols < 2) /* any valid, non-blank line has at least two tokens */
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_N_TOK, p->file, p->line, NULL);

    if (D->error == GD_E_OK)
      match = _GD_ParseDirective(D, p, in_cols, n_cols, me, &ref_name,
          &outstring, tok_pos);

    if (D->error == GD_E_OK && !match)
      first_raw = _GD_ParseFieldSpec(D, p, n_cols, in_cols, strlen(in_cols[0]),
          NULL, me, 0, 1, &outstring, tok_pos);

    if (D->error == GD_E_FORMAT) {
      /* call the callback for this error */
      if (D->sehandler != NULL) {
        /* we guarantee a buffer size of at least GD_MAX_LINE_LENGTH */
        if (n < GD_MAX_LINE_LENGTH) {
          char *ptr = (char *)_GD_Realloc(D, instring, GD_MAX_LINE_LENGTH);
          if (ptr == NULL)
            break;
          instring = ptr;
        }
        pdata.dirfile = D;
        pdata.suberror = D->suberror;
        pdata.linenum = p->line;
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
            /* a new line was malloc'd by the caller, delete our old one
             * then copy it from the caller's heap to libc's to avoid
             * problems later. */
            free(instring);
            if (_GD_CFree != free) {
              instring = strdup(pdata.line);
              _GD_CFree(pdata.line);
              if (instring == NULL) {
                _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
                break;
              }
            } else
              instring = pdata.line;

            /* The callback isn't required to update pdata.buflen when it
             * updates pdata.line, so just assume the buffer is as large as
             * the string it contains
             */
            n = strlen(instring) + 1;
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
    _GD_SetError(D, GD_E_LINE_TOO_LONG, 0, p->file, p->line,
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
    _GD_UpdateAliases(D, 0);

  dreturn("%p", ref_name);
  return ref_name;
}

void _GD_SimpleParserInit(DIRFILE *D, const char *name,
    struct parser_state *p)
{
  dtrace("%p, \"%s\", %p", D, name, p);

  if (~D->flags & GD_HAVE_VERSION)
    _GD_FindVersion(D);

  p->file = name;
  p->line = 0;
  if (D->av) {
    p->standards = D->standards;
    p->pedantic = 1;
  } else {
    p->standards = GD_DIRFILE_STANDARDS_VERSION;
    p->pedantic = 0;
  }
  p->ns = NULL;
  p->nsl = 0;
  p->flags = 0;

  dreturnvoid();
}

/* public access to the GetData tokeniser */
char *gd_strtok(DIRFILE *D, const char *string) gd_nothrow
{
  char *outstring, *in_col;
  int n_cols;
  struct parser_state p;

  dtrace("%p, \"%s\"", D, string);

  GD_RETURN_IF_INVALID(D, "%p", NULL);

  if (string) {
    free(D->tok_base);
    D->tok_pos = D->tok_base = _GD_Strdup(D, string);
    if (D->error) {
      dreturn("%p", NULL);
      return NULL;
    }
  } else if (D->tok_pos == NULL) {
    _GD_SetError(D, GD_E_ARGUMENT, GD_E_ARG_NODATA, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  /* tokenise! */
  _GD_SimpleParserInit(D, "gd_strtok()", &p);
  n_cols = _GD_Tokenise(D, &p, D->tok_pos, &outstring,
      (const char **)&D->tok_pos, 1, &in_col);

  if (D->error || n_cols < 1) {
    free(D->tok_base);
    D->tok_pos = D->tok_base = NULL;
    free(outstring);
    dreturn("%p", NULL);
    return NULL;
  }

  /* move to caller's heap -- in_col and outstring always end up pointing to
   * the same thing, so if we don't have to do this, there's nothing else we
   * need to do */
  if (_GD_CMalloc != malloc) {
    outstring = _GD_CStrdup(in_col);
    free(in_col);
    if (outstring == NULL)
      _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
  }

  dreturn("\"%s\"", outstring);
  return outstring;
}
/* vim: ts=2 sw=2 et tw=80
*/
