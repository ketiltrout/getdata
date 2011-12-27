/* Copyright (C) 2008-2011 D. V. Wiebe
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

/* add/remove/modify the prefix and suffix from a field code */
char *_GD_MungeCode(DIRFILE *D, const gd_entry_t *P, const char *old_prefix,
    const char *old_suffix, const char *new_prefix, const char *new_suffix,
    const char *code, int *offset)
{
  size_t len, oplen = 0, oslen = 0, nplen = 0, nslen = 0, plen = 0, mlen = 0;
  const char *ptr, *slash;
  char *new_code;

  dtrace("%p, %p, \"%s\", \"%s\", \"%s\", \"%s\", \"%s\", %p", D, P, old_prefix,
      old_suffix, new_prefix, new_suffix, code, offset);

  if (code == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  len = strlen(code);

  /* Verify the old prefix is present */
  if (old_prefix) {
    oplen = strlen(old_prefix);
    if (strncmp(old_prefix, code, oplen)) {
      /* prefix missing */
      _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_INVALID, NULL, 0, code);
      dreturn("%p", NULL);
      return NULL;
    }
    ptr = code + oplen;
    len -= oplen;
  } else
    ptr = code;

  /* look for a /, which could indicate this is a metafield code.  If it is
   * just an illegal name with a / in it, mungeing will screw up, but
   * validation will catch the illegal name later anyways.
   */
  if ((slash = memchr(ptr, '/', len))) {
    mlen = len + (ptr - slash);
    len = slash++ - ptr;
  }

  /* Verify the suffix is present */
  if (old_suffix) {
    oslen = strlen(old_suffix);
    if (strncmp(old_suffix, ptr + len - oslen, oslen)) {
      /* suffix missing */
      _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_INVALID, NULL, 0, code);
      dreturn("%p", NULL);
      return NULL;
    }
    len -= oslen;
  }

  if (new_prefix)
    nplen = strlen(new_prefix);

  if (new_suffix)
    nslen = strlen(new_suffix);

  if (P)
    plen = strlen(P->field) + 1;

  if ((new_code = _GD_Malloc(D, plen + nplen + len + nslen + mlen + 1)) == NULL)
  {
    dreturn("%p", NULL);
    return NULL;
  }

  if (P) {
    strcpy(new_code, P->field);
    new_code[plen - 1] = '/';
    strcpy(new_code + plen, ptr);
  } else {
    if (nplen > 0)
      strcpy(new_code, new_prefix);

    strncpy(new_code + nplen, ptr, len);

    if (nslen > 0)
      strcpy(new_code + nplen + len, new_suffix);

    if (slash) {
      new_code[nplen + len + nslen] = '/';
      strcpy(new_code + nplen + len + nslen + 1, slash);
    }

    new_code[nplen + len + nslen + mlen] = '\0';
  }

  *offset = plen;

  dreturn("\"%s\" (%i)", new_code, *offset);
  return new_code;
}

/* Munge a field code or field name using the prefix and suffix of the current
 * fragment.  Returns a newly malloc'd munged code, or NULL on error */
char *_GD_MungeFromFrag(DIRFILE *D, const gd_entry_t *P, int me,
    const char *code, int *offset)
{
  char *new_code;
  dtrace("%p, %p, %i, \"%s\", %p", D, P, me, code, offset);

  new_code = _GD_MungeCode(D, P, NULL, NULL, D->fragment[me].prefix,
      D->fragment[me].suffix, code, offset);

  dreturn("\"%s\"", new_code);
  return new_code;
}

/* Check for a valid field name -- returns 1 on error */
int _GD_ValidateField(const char* field_code, int standards, int strict,
    int affix, int* is_dot)
{
  const size_t len = strlen(field_code);
  size_t i, local_dot = 0;

  dtrace("\"%s\", %i, %i, %i, %p", field_code, standards, strict, affix,
      is_dot);

  if (is_dot)
    *is_dot = 0;

  if (!affix && (field_code[0] == '\0' || (strict &&
          ((len > 50 && standards < 5) || (len > 16 && standards < 3)))))
  {
    dreturn("%i", 1);
    return 1;
  }

  for (i = 0; i < len; ++i)
    if (field_code[i] == '/' || field_code[i] < 0x20) {
      /* these characters are always forbidden */
      dreturn("%i", 1);
      return 1;
    } else if (strict && ((standards >= 5 && (field_code[i] == '<' ||
              field_code[i] == '>' || field_code[i] == ';' ||
              field_code[i] == '|' || field_code[i] == '&')) ||
          (standards == 5 && (field_code[i] == '\\' || field_code[i] == '#'))))
    {
      /* these characters are sometimes forbidden */
      dreturn("%i", 1);
      return 1;
    } else if (field_code[i] == '.') {
      if (affix || is_dot == NULL || (standards >= 6 && strict)) {
        dreturn("%i", 1);
        return 1;
      } else
        local_dot = 1;
    }

  if (!affix) {
    if (strict && standards < 8)
      if ((strcmp("FRAMEOFFSET", field_code) == 0 && standards >= 1)
          || (strcmp("ENCODING", field_code) == 0 && standards >= 6)
          || (strcmp("ENDIAN", field_code) == 0 && standards >= 5)
          || (strcmp("INCLUDE", field_code) == 0 && standards >= 3)
          || (strcmp("META", field_code) == 0 && standards >= 6)
          || (strcmp("VERSION", field_code) == 0 && standards >= 5)
          || (strcmp("PROTECT", field_code) == 0 && standards >= 6)
          || (strcmp("REFERENCE", field_code) == 0 && standards >= 6))
      {
        dreturn("%i", 1);
        return 1;
      }

    if (is_dot)
      *is_dot = local_dot;
  }

  dreturn("%i", 0);
  return 0;
}

#define GD_UPDI 0x1
#define GD_UPDU 0x2
/* pass == 0: finalise clear
 * pass == 1: initialise clear
 * pass == 2: finalise update
 * pass == 3: initialise update
 */
static char **_GD_UpdateScalar(DIRFILE *D, gd_entry_t *T, const gd_entry_t *E,
    char **list, size_t len, int n, int pass, int *nl)
{
  char **ptr;
  dtrace("%p, %p, %p, %p, %zu, %i, %i, %i", D, T, E, list, len, n, pass, *nl);

  if (pass & GD_UPDI) {
    if ((ptr = _GD_Realloc(D, list, sizeof(char *) * (*nl + 1))) == NULL) {
      *nl = -1;
      dreturn("%p", list);
      return list;
    }
    list = ptr;
    list[(*nl)++] = _GD_Malloc(D, len + 3);
  } else if (pass == 2) {
    D->fragment[T->fragment_index].modified = 1;
    free(T->scalar[n]);
    T->scalar[n] = list[(*nl)++];

    sprintf(T->scalar[n], "%s%s", E->field,
        (T->e->repr[n] == GD_REPR_NONE) ? "" :
        (T->e->repr[n] == GD_REPR_REAL) ? ".r" :
        (T->e->repr[n] == GD_REPR_IMAG) ? ".i" :
        (T->e->repr[n] == GD_REPR_MOD) ? ".m" : ".a");
  }

  dreturn("%p (%i)", list, *nl);
  return list;
}

static char **_GD_InvalidateConst(DIRFILE *D, const gd_entry_t *E, char **list,
    size_t len, int pass, int *nl)
{
  int i, j;
  unsigned u;

  dtrace("%p, %p, %p, %zu, %i, %p", D, E, list, len, pass, nl);

  if (pass & GD_UPDI)
    for (u = 0; u < D->n_entries; ++u)
      if (!D->entry[u]->e->calculated)
        _GD_CalculateEntry(D, D->entry[u], 0);

  for (j = 0; j < E->e->u.scalar.n_client; ++j) {
    if (!(pass & GD_UPDI))
      E->e->u.scalar.client[j]->e->calculated = 0;

    if (!(pass & GD_UPDU))
      continue;

    switch (E->e->u.scalar.client[j]->field_type) {
      case GD_LINCOM_ENTRY:
        for (i = 0; i < E->e->u.scalar.client[j]->n_fields; ++i) {
          list = _GD_UpdateScalar(D, E->e->u.scalar.client[j], E, list, len, i,
              pass, nl);
          list = _GD_UpdateScalar(D, E->e->u.scalar.client[j], E, list, len,
              i + GD_MAX_LINCOM, pass, nl);
        }
        break;
      case GD_POLYNOM_ENTRY:
        for (i = 0; i <= E->e->u.scalar.client[j]->poly_ord; ++i)
          list = _GD_UpdateScalar(D, E->e->u.scalar.client[j], E, list, len, i,
              pass, nl);
        break;
      case GD_BIT_ENTRY:
      case GD_SBIT_ENTRY:
        list = _GD_UpdateScalar(D, E->e->u.scalar.client[j], E, list, len, 1,
            pass, nl);
        /* Fallthrough */
      case GD_PHASE_ENTRY:
      case GD_RAW_ENTRY:
      case GD_RECIP_ENTRY:
      case GD_WINDOW_ENTRY:
        list = _GD_UpdateScalar(D, E->e->u.scalar.client[j], E, list, len, 0,
            pass, nl);
        break;
      case GD_NO_ENTRY:
      case GD_LINTERP_ENTRY:
      case GD_MULTIPLY_ENTRY:
      case GD_DIVIDE_ENTRY:
      case GD_INDEX_ENTRY:
      case GD_STRING_ENTRY:
      case GD_CONST_ENTRY:
      case GD_CARRAY_ENTRY:
        break;
    }
  }

  dreturn("%p", list);
  return list;
}

/* pass == 0: clear cached derived entries
 * pass == 1: initialise re-writen derived channels
 * pass == 2: finalise re-writen derivd channels
 */
static char **_GD_UpdateInField(DIRFILE *D, gd_entry_t *T, const gd_entry_t *E,
    char **list, size_t len, int n, int pass, int *nl)
{
  char **ptr;
  dtrace("%p, %p, %p, %p, %zu, %i, %i, %i", D, T, E, list, len, n, pass, *nl);

  if (pass != 0 && T->e->entry[n] == NULL)
    _GD_BadInput(D, T, n, 0);

  if (T->e->entry[n] != E) {
    dreturn("%p (-)", list);
    return list;
  }

  if (pass == 0)
    T->e->entry[n] = NULL;
  else if (pass == 1) {
    if ((ptr = _GD_Realloc(D, list, sizeof(char *) * (*nl + 1))) == NULL) {
      *nl = -1;
      dreturn("%p", list);
      return list;
    }
    list = ptr;
    list[(*nl)++] = _GD_Malloc(D, len + 3);
  } else if (pass == 2) {
    D->fragment[T->fragment_index].modified = 1;
    free(T->in_fields[n]);
    T->in_fields[n] = list[(*nl)++];

    sprintf(T->in_fields[n], "%s%s", E->field,
        (T->e->repr[n] == GD_REPR_NONE) ? "" :
        (T->e->repr[n] == GD_REPR_REAL) ? ".r" :
        (T->e->repr[n] == GD_REPR_IMAG) ? ".i" :
        (T->e->repr[n] == GD_REPR_MOD) ? ".m" : ".a");
  }

  dreturn("%p (%i)", list, *nl);
  return list;
}

static char **_GD_InvalidateVect(DIRFILE *D, const gd_entry_t *E, char **list,
    size_t len, int pass, int *nl)
{
  unsigned u;

  dtrace("%p, %p, %p, %zu, %i, %p", D, E, list, len, pass, nl);

  for (u = 0; u < D->n_entries; ++u) {
    if (D->entry[u] != E)
      switch (D->entry[u]->field_type) {
        case GD_LINCOM_ENTRY:
          list = _GD_UpdateInField(D, D->entry[u], E, list, len, 2, pass, nl);
          /* Fallthrough */
        case GD_MULTIPLY_ENTRY:
        case GD_DIVIDE_ENTRY:
        case GD_WINDOW_ENTRY:
          list = _GD_UpdateInField(D, D->entry[u], E, list, len, 1, pass, nl);
          /* Fallthrough */
        case GD_LINTERP_ENTRY:
        case GD_BIT_ENTRY:
        case GD_PHASE_ENTRY:
        case GD_POLYNOM_ENTRY:
        case GD_RECIP_ENTRY:
        case GD_SBIT_ENTRY:
          list = _GD_UpdateInField(D, D->entry[u], E, list, len, 0, pass, nl);
          break;
        case GD_INDEX_ENTRY:
        case GD_RAW_ENTRY:
        case GD_NO_ENTRY:
        case GD_CONST_ENTRY:
        case GD_CARRAY_ENTRY:
        case GD_STRING_ENTRY:
          break;
      }
    if (*nl == -1)
      break;
  }

  dreturn("%p", list);
  return list;
}

static int _GD_Rename(DIRFILE *D, gd_entry_t *E, const char *new_name,
    int old_dot, unsigned dot_ind, unsigned flags)
{
  gd_entry_t *Q;
  char *name;
  int dummy, new_dot;

  dtrace("%p, %p, \"%s\", %i, %u, 0x%X", D, E, new_name, old_dot, dot_ind,
      flags);

  /* check access mode */
  if ((D->flags & GD_ACCMODE) == GD_RDONLY) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* check metadata protection */
  if (D->fragment[E->fragment_index].protection & GD_PROTECT_FORMAT) {
    _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[E->fragment_index].cname);
    dreturn("%i", -1);
    return -1;
  }

  if (_GD_ValidateField(new_name, D->standards, 1, 0, &new_dot)) {
    _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_INVALID, NULL, 0, new_name);
    dreturn("%i", -1);
    return -1;
  }

  if (E->e->n_meta == -1) {
    name = _GD_Malloc(D, strlen(E->e->p.parent->field) + strlen(new_name) + 2);
    if (name == NULL) {
      dreturn("%i", -1);
      return -1;
    }      
    sprintf("%s/%s", E->e->p.parent->field, new_name);
  } else {
    /* Verify prefix and suffix */
    name = _GD_MungeCode(D, NULL, D->fragment[E->fragment_index].prefix,
        D->fragment[E->fragment_index].suffix, NULL, NULL, new_name, &dummy);
    if (name == NULL || name[0] == '\0') {
      _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_INVALID, NULL, 0, new_name);
      dreturn("%i", -1);
      return -1;
    }
    free(name);
    name = _GD_Strdup(D, new_name);
  }

  if (D->error) {
    dreturn("%i", -1);
    return -1;
  }

  /* Duplicate check */
  Q = _GD_FindField(D, name, D->entry, D->n_entries, 1, NULL);

  if (Q == E) {
    free(name);
    dreturn("%i", 0);
    return 0;
  }

  if (Q != NULL) {
    _GD_SetError(D, GD_E_DUPLICATE, 0, NULL, 0, name);
    free(name);
    dreturn("%i", -1);
    return -1;
  }

  if (E->field_type == GD_RAW_ENTRY) {
    /* Compose the new filename */
    char *filebase = _GD_Strdup(D, new_name);

    if (filebase == NULL) {
      free(name);
      dreturn("%i", -1);
      return -1;
    }

    /* Close the old file */
    if (_GD_FiniRawIO(D, E, E->fragment_index, GD_FINIRAW_KEEP)) {
      free(name);
      free(filebase);
      dreturn("%i", -1);
      return -1;
    }

    /* Resize the dot list; this must be done early in case it fails */
    if (new_dot && !old_dot) {
      gd_entry_t** ptr = (gd_entry_t **)_GD_Realloc(D, D->dot_list,
          sizeof(gd_entry_t*) * (D->n_dot + 1));

      if (ptr == NULL) {
        free(name);
        dreturn("%i", -1);
        return -1;
      }

      D->dot_list = ptr;
    }

    if (flags & GD_REN_DATA) {
      struct _gd_raw_file temp;

      /* check data protection */
      if (D->fragment[E->fragment_index].protection & GD_PROTECT_DATA) {
        _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_DATA, NULL, 0,
            D->fragment[E->fragment_index].cname);
        free(name);
        free(filebase);
        dreturn("%i", -1);
        return -1;
      }

      if (!_GD_Supports(D, E, GD_EF_MOVE)) {
        free(name);
        free(filebase);
        dreturn("%i", -1);
        return -1;
      }

      memcpy(&temp, E->e->u.raw.file, sizeof(struct _gd_raw_file));
      temp.name = NULL;
      if (_GD_SetEncodedName(D, &temp, filebase, 0)) {
        free(name);
        free(filebase);
        dreturn("%i", -1);
        return -1;
      }

      if (_GD_SetEncodedName(D, E->e->u.raw.file, E->e->u.raw.filebase, 0)) {
        free(name);
        free(filebase);
        dreturn("%i", -1);
        return -1;
      }

      if ((*_gd_ef[E->e->u.raw.file[0].subenc].move)(
            D->fragment[E->fragment_index].dirfd, E->e->u.raw.file,
            D->fragment[E->fragment_index].dirfd, temp.name))
      {
        _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.raw.file[0].name, errno, NULL);
        free(filebase);
        dreturn("%i", -1);
        return -1;
      }

      /* Nothing may fail from now on */

    } else {
      free(E->e->u.raw.file[0].name);
      E->e->u.raw.file[0].name = NULL;
    }

    free(E->e->u.raw.filebase);
    E->e->u.raw.filebase = filebase;
  }

  free(E->field);
  E->field = name;

  D->fragment[E->fragment_index].modified = 1;

  /* Update the dot list */
  if (old_dot && !new_dot)
    memmove(D->dot_list + dot_ind, D->dot_list + dot_ind + 1,
        sizeof(gd_entry_t*) * (--D->n_dot - dot_ind));
  else if (new_dot && !old_dot)
    D->dot_list[D->n_dot++] = E;

  /* re-sort the lists */
  qsort(D->entry, D->n_entries, sizeof(gd_entry_t*), _GD_EntryCmp);
  if (new_dot)
    qsort(D->dot_list, D->n_dot, sizeof(gd_entry_t*), _GD_EntryCmp);

  /* Invalidate the field lists */
  D->list_validity = 0;
  D->type_list_validity = 0;

  dreturn("%i", 0);
  return 0;
}

int gd_rename(DIRFILE *D, const char *old_code, const char *new_name,
    unsigned flags)
{
  gd_entry_t *E = NULL;
  int ret, i, nl = 0, old_dot = 0;
  size_t len;
  unsigned dot_ind = 0;
  char **code_list = NULL;

  dtrace("%p, \"%s\", \"%s\", 0x%X", D, old_code, new_name, flags);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ClearError(D);

  len = strlen(new_name);

  /* check for a dotted field name */
  if (D->n_dot > 0)
    E = _GD_FindField(D, old_code, D->dot_list, D->n_dot, 0, &dot_ind);

  if (E)
    old_dot = 1;
  else
    E = _GD_FindField(D, old_code, D->entry, D->n_entries, 1, NULL);

  if (E == NULL) {
    _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_MISSING, NULL, 0, old_code);
    dreturn("%i", -1);
    return -1;
  }

  if (E->field_type == GD_INDEX_ENTRY) {
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, "INDEX");
    dreturn("%i", -1);
    return -1;
  }

  /* check derived/client fields */
  if (E->field_type == GD_CARRAY_ENTRY || E->field_type == GD_CONST_ENTRY)
    code_list = _GD_InvalidateConst(D, E, code_list, len,
        GD_UPDI | (flags & GD_REN_UPDB ? GD_UPDU : 0), &nl);
  else if (E->field_type != GD_STRING_ENTRY)
    if (flags & GD_REN_UPDB)
      code_list = _GD_InvalidateVect(D, E, code_list, len, 1, &nl);

  if (D->error) {
    if (code_list) {
      for (i = 0; i < nl; ++i)
        free(code_list[i]);
      free(code_list);
    }
    dreturn("%i", -1);
    return -1;
  }

  ret = _GD_Rename(D, E, new_name, old_dot, dot_ind, flags);

  if (!ret) {
    nl = 0;
    /* update derived/client fields */
    if (E->field_type == GD_CARRAY_ENTRY || E->field_type == GD_CONST_ENTRY)
      _GD_InvalidateConst(D, E, code_list, 0,
          flags & GD_REN_UPDB ? GD_UPDU : 0, &nl);
    else if (E->field_type != GD_STRING_ENTRY)
      _GD_InvalidateVect(D, E, code_list, 0,
          flags & GD_REN_UPDB ? GD_UPDU : 0, &nl);
  }

  if (code_list) {
    if (D->error)
      for (i = 0; i < nl; ++i)
        free(code_list[i]);
    free(code_list);
  }

  dreturn("%i", ret);
  return ret;
}

int gd_rename_alias(DIRFILE *D, const char *old_code, const char *new_name)
  gd_nothrow
{
  gd_entry_t *E;
  int ret;

  dtrace("%p, \"%s\", \"%s\"", D, old_code, new_name);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ClearError(D);

  E = _GD_FindField(D, old_code, D->entry, D->n_entries, 0, NULL);

  if (E == NULL) {
    _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_MISSING, NULL, 0, old_code);
    dreturn("%i", -1);
    return -1;
  }

  if (E->field_type != GD_ALIAS_ENTRY) {
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, "INDEX");
    dreturn("%i", -1);
    return -1;
  }

  ret = _GD_Rename(D, E, new_name, 0, 0, 0);

  dreturn("%i", ret);
  return ret;
}
