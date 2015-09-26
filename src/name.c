/* Copyright (C) 2008, 2010-2015 D. V. Wiebe
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

/* compose/modify a field code.  This function takes care of adding and
 * removing affixes and namespaces */
char *_GD_MungeCode(DIRFILE *D, const char *ns, size_t len_newns,
    const char *old_prefix, const char *old_suffix, const char *new_prefix,
    const char *new_suffix, const char *code, char **nso, int *offset,
    unsigned flags)
{
  size_t len, len_oldpx = 0, len_oldsx = 0, len_newpx, len_newsx;
  size_t len_sub = 0, len_oldns;
  const char *ptr, *slash, *old_ns;
  char *new_code, *nptr;

  dtrace("%p, \"%s\", %" PRNsize_t ", \"%s\", \"%s\", \"%s\", \"%s\", \"%s\", "
      "%p, 0x%X", D, ns, len_newns, old_prefix, old_suffix, new_prefix,
      new_suffix, code, nso, flags);

  if (code == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  old_ns = code;
  len = strlen(code);

  /* Check for a required namespace.  In this case, the supplied 'ns' is the
   * namespace to check for and is not later prepended */
  if (flags & GD_MC_CHECK_NS && ns) {
    if (len <= len_newns || strncmp(ns, code, len_newns) ||
        code[len_newns] != '.')
    {
      /* namespace missing */
      if (flags & GD_MC_RQ_PARTS) {
        if (flags & GD_MC_ERROR_OK)
          _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_INVALID_NS, NULL, 0, code);
        else
          _GD_InternalError(D);
      }
      dreturn("%p", NULL);
      return NULL;
    }
    /* don't re-add it if it's there already */
    ns = NULL;
  }

  /* find the length of the current namespace tag */
  if (flags & GD_MC_NO_NS)
    len_oldns = 0;
  else
    for (len_oldns = len; len_oldns > 0; --len_oldns)
      if (code[len_oldns - 1] == '.')
        break;

  code += len_oldns;
  len -= len_oldns;

  /* Verify the old prefix is present */
  if (old_prefix) {
    len_oldpx = strlen(old_prefix);
    if (strncmp(old_prefix, code, len_oldpx)) {
      /* prefix missing */
      if (flags & GD_MC_RQ_PARTS) {
        if (flags & GD_MC_ERROR_OK)
          _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_INVALID, NULL, 0, code);
        else
          _GD_InternalError(D);
      }
      dreturn("%p", NULL);
      return NULL;
    }
    ptr = code + len_oldpx;
    len -= len_oldpx;
  } else {
    ptr = code;
    len_oldpx = 0;
  }

  /* look for a /, which could indicate this is a metafield code.  If it is
   * just an illegal name with a / in it, mungeing will screw up, but
   * validation will catch the illegal name later anyways.
   */
  if ((slash = (char*)memchr(ptr, '/', len))) {
    len_sub = len + (ptr - slash);
    len = slash - ptr;
  }

  /* Verify the suffix is present */
  if (old_suffix) {
    len_oldsx = strlen(old_suffix);
    if (strncmp(old_suffix, ptr + len - len_oldsx, len_oldsx)) {
      /* suffix missing */
      if (flags & GD_MC_RQ_PARTS) {
        if (flags & GD_MC_ERROR_OK)
          _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_INVALID, NULL, 0, code);
        else
          _GD_InternalError(D);
      }
      dreturn("%p", NULL);
      return NULL;
    }
    len -= len_oldsx;
  } else
    len_oldsx = 0;

  /* In this case, we're using MungeCode to verify affixes are present, so
   * nothing more to do, return input code */
  if (flags & GD_MC_NO_ALLOC) {
    dreturn("%p", code);
    return (char*)code;
  }

  if (new_prefix)
    len_newpx = strlen(new_prefix);
  else
    len_newpx = 0;

  if (new_suffix)
    len_newsx = strlen(new_suffix);
  else
    len_newsx = 0;

  if (ns == NULL)
    len_newns = 0;

  /* for the '.' */
  if (len_newns)
    len_newns++;

  if ((new_code = _GD_Malloc(D, len_newns + len_oldns + len_newpx + len +
          len_newsx + len_sub + 1)) == NULL)
  {
    dreturn("%p", NULL);
    return NULL;
  }

  /* compose the new code */
  nptr = new_code;
  if (len_newns > 0) {
    strcpy(nptr, ns);
    nptr[len_newns - 1] = '.';
    nptr += len_newns;
  }

  if (len_oldns > 0) {
    strncpy(nptr, old_ns, len_oldns);
    nptr += len_oldns;
  }

  if (len_newpx > 0) {
    strcpy(nptr, new_prefix);
    nptr += len_newpx;
  }

  strncpy(nptr, ptr, len);
  nptr += len;

  if (len_newsx > 0) {
    strcpy(nptr, new_suffix);
    nptr += len_newsx;
  }

  if (slash) {
    strcpy(nptr, slash);
    nptr += len_sub;
  }

  *nptr = '\0';

  /* A field called "INDEX" can never take a namespace.   We have to check this
   * late because the field name "INDEX" could be built up via /INCLUDE affixes
   * (as crazy as that may be) */
  if (len_newns + len_oldns > 0 &&
      strncmp(new_code + len_newns + len_oldns, "INDEX", 5) == 0
      && (new_code[len_newns + len_oldns + 5] == '\0'
        || new_code[len_newns + len_oldns + 5] == '/'))
  {
    char *index = _GD_Strdup(D, new_code + len_newns + len_oldns);
    free(new_code);
    if (index == NULL) {
      dreturn("%p", NULL);
      return NULL;
    }

    new_code = index;
  }

  if (nso)
    *nso = new_code + len_newns + len_oldns;
  
  if (offset)
    *offset = len_newns + len_oldns;

  dreturn("\"%s\" (%i, %p)", new_code, offset ? *offset : -1,
      nso ? *nso : NULL);
  return new_code;
}

/* Return non-zero if the a field code doesn't contain the correct affixes or
 * namespace. */
int _GD_CheckCodeAffixes(DIRFILE *D, const char *field_code, int fragment,
    int set_error)
{
  dtrace("%p, \"%s\", %i, %i", D, field_code, fragment, set_error);

  if (field_code == NULL) {
    dreturn("%i", 0);
    return 0;
  }

  if (_GD_MungeCode(D, D->fragment[fragment].ns, D->fragment[fragment].nsl,
      D->fragment[fragment].prefix, D->fragment[fragment].suffix, NULL, NULL,
      field_code, NULL, NULL, GD_MC_NO_ALLOC | GD_MC_CHECK_NS |
      (set_error ? (GD_MC_RQ_PARTS | GD_MC_ERROR_OK) : 0)))
  {
    /* success */
    dreturn("%i", 0);
    return 0;
  }

  dreturn("%i", 1);
  return 1;
}

/* Check for a valid name -- returns 1 on error */
int _GD_ValidateField(const char* field_code, int standards, int strict,
    unsigned type, int* is_dot)
{
  const size_t len = strlen(field_code);
  size_t i;
  int local_dot = 0, last_dot;

  dtrace("\"%s\", %i, %i, %u, %p", field_code, standards, strict, type,
      is_dot);

  /* field codes may not start with a dot */
  last_dot = (type == GD_VF_CODE && (!strict || standards >= 10)) ? 1 : 0;

  if (is_dot)
    *is_dot = 0;

  if ((type == GD_VF_NAME) && (field_code[0] == '\0' || (strict &&
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
      if (type == GD_VF_NS ||
          (type == GD_VF_CODE && (!strict || standards >= 10)))
      {
        if (last_dot) { /* multiple consecutive dots are forbidden */
          dreturn("%i", 1);
          return 1;
        }
        last_dot = 1;
      } else if (type == GD_VF_AFFIX || is_dot == NULL ||
          (standards >= 6 && strict))
      {
        dreturn("%i", 1);
        return 1;
      } else
        local_dot = 1;
    } else
      last_dot = 0;

  /* Field codes may not end in a dot */
  if (type == GD_VF_CODE && last_dot) {
    dreturn("%i", 1);
    return 1;
  }

  /* forbidden field names */
  if (type == GD_VF_NAME) {
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
  }

  if (is_dot)
    *is_dot = local_dot;

  dreturn("%i (%i)", 0, local_dot);
  return 0;
}

/* Compare field codes candidate and code. Returns:
 * 0    if they're identical
 * '.'  if they're the same code, but candidate has a representation suffix
 * '/'  if candidate is a subfield of code
 * -1   otherwise
 */
static int _GD_MatchCode(const char *candidate, const char *code, size_t len,
    int meta_ok)
{
  int c;

  dtrace("\"%s\", \"%s\", %" PRNsize_t ", %i", candidate, code, len,
      meta_ok);

  if (strncmp(candidate, code, len)) {
    /* field not matched */
    dreturn("%i (m)", -1);
    return -1;
  }

  /* partial match only */
  c = candidate[len];
  if (c && c != '.' && (!meta_ok || c != '/')) {
    dreturn("%i (p)", -1);
    return -1;
  }

  dreturn("%i", c);
  return c;
}

int _GD_MakeNewCode(DIRFILE *D, const char *old_code, int frag,
    const gd_entry_t *E, int repr, int c, struct gd_rename_data_ *rdat)
{
  char **ptr, *base_code;
  int ret = 0;

  dtrace("%p, \"%s\", %i, %p, %i, %i, %p", D, old_code, frag, E, repr, c, rdat);

  if ((ptr = _GD_Realloc(D, rdat->code_list, sizeof(*ptr) * (rdat->n_code + 1)))
      == NULL)
  {
    dreturn("%i", -1);
    return -1;
  }
  rdat->code_list = ptr;

  /* extract representation suffix, if necessary (if c is nil, we already
   * know there's no representation) */
  if (E == NULL && (c == '.' || c == '/')) {
    repr = _GD_GetRepr(D, old_code, &base_code, 0);
    if (D->error) {
      dreturn("%i", -1);
      return -1;
    }
  } else
    base_code = (char*)old_code;

  if (c == '/') { /* a meta subfield of the field we're renaming */
    size_t base_len = strlen(base_code);
    rdat->code_list[rdat->n_code] = (char*)_GD_Malloc(D, base_len +
        rdat->new_len - rdat->old_len + ((repr == GD_REPR_NONE) ? 1 : 3));
    if (D->error) {
      ret = -1;
      goto DONE;
    }
    sprintf(rdat->code_list[rdat->n_code], "%s%s%s", rdat->new_code,
        base_code + rdat->old_len,
        (repr == GD_REPR_NONE) ? "" : (repr == GD_REPR_REAL) ? ".r" :
        (repr == GD_REPR_IMAG) ? ".i" : (repr == GD_REPR_MOD) ? ".m" : ".a");
  } else {
    rdat->code_list[rdat->n_code] = (char*)_GD_Malloc(D, rdat->new_len +
        ((repr == GD_REPR_NONE) ? 1 : 3));
    if (D->error) {
      ret = -1;
      goto DONE;
    }
    sprintf(rdat->code_list[rdat->n_code], "%s%s", rdat->new_code,
        (repr == GD_REPR_NONE) ? "" : (repr == GD_REPR_REAL) ? ".r" :
        (repr == GD_REPR_IMAG) ? ".i" : (repr == GD_REPR_MOD) ? ".m" : ".a");
  }
  /* check that we haven't made a code that's invalid in the destination
   * fragment
   */
  if (_GD_CheckCodeAffixes(D, rdat->code_list[rdat->n_code], frag,
        !(rdat->flags & GD_REN_FORCE)))
  {
    free(rdat->code_list[rdat->n_code]);
    if (rdat->flags & GD_REN_FORCE) {
      rdat->code_list[rdat->n_code] = NULL;
      rdat->n_code++;
    } else
      ret = -1;
  } else
    rdat->n_code++;

DONE:
  /* clean up after GetRepr */
  if (base_code != old_code)
    free(base_code);

  dreturn("%i", ret);
  return ret;
}

static void _GD_SetNewCode(DIRFILE *D, char **code, int frag,
    struct gd_rename_data_ *rdat)
{
  dtrace("%p, %p, %i, %p", D, code, frag, rdat);

  /* a NULL here is a field we couldn't update but the rename was forced via
   * GD_REN_FORCE
   */
  if (rdat->code_list[rdat->n_code]) {
    D->fragment[frag].modified = 1;
    free(*code);
    *code = rdat->code_list[rdat->n_code];
  }
  rdat->n_code++;

  dreturnvoid();
}

/* internal update flags */
#define GD_UPDI 0x1 /* initialise */
#define GD_UPDU 0x2 /* update */

#define GD_UP_DO_CL 0
#define GD_UP_IN_CL (GD_UPDI)
#define GD_UP_DO_UP (GD_UPDU)
#define GD_UP_IN_UP (GD_UPDI | GD_UPDU)
static int _GD_UpdateScalar(DIRFILE *D, gd_entry_t *T,
    struct gd_rename_data_ *rdat, int n, int search_meta, unsigned mode)
{
  int c;

  dtrace("%p, %p, %p, %i, %i, %u", D, T, rdat, n, search_meta, mode);

  /* nothing to do */
  if (T->scalar[n] == NULL) {
    dreturn("%i (-)", 0);
    return 0;
  }

  c = _GD_MatchCode(T->scalar[n], rdat->old_code, rdat->old_len, search_meta);
  if (c < 0) {
    dreturn("%i", 0);
    return 0;
  }

  if (!(mode & GD_UPDI)) /* clear the cache */
    T->flags &= ~GD_EN_CALC;

  if (mode == GD_UP_IN_UP) { /* create new field codes */
    if (_GD_MakeNewCode(D, T->scalar[n], T->fragment_index, NULL, 0, c, rdat)) {
      dreturn("%i", -1);
      return -1;
    }
  } else if (mode == GD_UP_DO_UP) /* move the new field codes into place */
    _GD_SetNewCode(D, T->scalar + n, T->fragment_index, rdat);

  dreturn("%i (%i)", 0, rdat->n_code);
  return 0;
}

static int _GD_UpdateInField(DIRFILE *D, gd_entry_t *T,
    struct gd_rename_data_ *rdat, int n, int search_meta, unsigned mode)
{
  int c;

  dtrace("%p, %p, %p, %i, %i, %u", D, T, rdat, n, search_meta, mode);

  c = _GD_MatchCode(T->in_fields[n], rdat->old_code, rdat->old_len,
      search_meta);
  if (c < 0) {
    dreturn("%i", 0);
    return 0;
  }

  if (!(mode & GD_UPDI)) { /* clear the cache */
    T->e->entry[n] = NULL;
    if (T->field_type == GD_ALIAS_ENTRY)
      T->e->entry[1] = NULL;
  }

  if (mode == GD_UP_IN_UP) { /* create new field codes */
    if (_GD_MakeNewCode(D, T->in_fields[n], T->fragment_index, T->e->entry[n],
          T->e->repr[n], c, rdat))
    {
      dreturn("%i", -1);
      return -1;
    }
  } else if (mode == GD_UP_DO_UP) /* move the new field codes into place */
    _GD_SetNewCode(D, T->in_fields + n, T->fragment_index, rdat);


  dreturn("%i (%i)", 0, rdat->n_code);
  return 0;
}

/* search for and update field metadata to account for a renamed field -- this
 * is a combination of the old _GD_InvalidateVect and _GD_InvalidateConst */
static int _GD_UpdateInputs(DIRFILE *D, struct gd_rename_data_ *rdat,
    int perform)
{
  unsigned u;
  int i;

  /* look for meta fields */
  const int search_meta = (rdat->E->e->n_meta != -1);

  /* classes of things to update */
  const int update_scalars = (search_meta ||
      (rdat->E->field_type & GD_SCALAR_ENTRY_BIT));
  const int update_vectors = (search_meta ||
      !(rdat->E->field_type & GD_SCALAR_ENTRY_BIT));
  const int update_aliases = !(rdat->flags & GD_REN_DANGLE);

  /* update mode for scalars and vectors */
  const int mode = ((rdat->flags & GD_REN_UPDB) ? GD_UPDU : 0) |
    (perform ? 0 : GD_UPDI);
  /* update mode for aliases */
  const int amode = (update_aliases ? GD_UPDU : 0) |
    (perform ? 0 : GD_UPDI);

  dtrace("%p, %p, %i", D, rdat, perform);

  /* reset the code count */
  rdat->n_code = 0;

  for (u = 0; u < D->n_entries; ++u) {
    switch (D->entry[u]->field_type) {
      case GD_LINCOM_ENTRY:
        if (update_vectors)
          for (i = 0; i < D->entry[u]->EN(lincom,n_fields); ++i) {
            if (_GD_UpdateInField(D, D->entry[u], rdat, i, search_meta, mode)) {
              dreturn("%i", -1);
              return -1;
            }
          }
        break;
      case GD_MULTIPLY_ENTRY:
      case GD_DIVIDE_ENTRY:
      case GD_WINDOW_ENTRY:
      case GD_MPLEX_ENTRY:
        if (update_vectors)
          if (_GD_UpdateInField(D, D->entry[u], rdat, 1, search_meta, mode)) {
            dreturn("%i", -1);
            return -1;
          }
        /* Fallthrough */
      case GD_LINTERP_ENTRY:
      case GD_BIT_ENTRY:
      case GD_PHASE_ENTRY:
      case GD_POLYNOM_ENTRY:
      case GD_RECIP_ENTRY:
      case GD_SBIT_ENTRY:
        if (update_vectors)
          if (_GD_UpdateInField(D, D->entry[u], rdat, 0, search_meta, mode)) {
            dreturn("%i", -1);
            return -1;
          }
        break;
      case GD_INDEX_ENTRY:
      case GD_RAW_ENTRY:
      case GD_NO_ENTRY:
      case GD_CONST_ENTRY:
      case GD_CARRAY_ENTRY:
      case GD_STRING_ENTRY:
      case GD_ALIAS_ENTRY:
        break;
    }
    if (update_scalars)
      switch (D->entry[u]->field_type) {
        case GD_LINCOM_ENTRY:
          for (i = 0; i < D->entry[u]->EN(lincom,n_fields); ++i)
            if (_GD_UpdateScalar(D, D->entry[u], rdat, i, search_meta, mode) ||
                _GD_UpdateScalar(D, D->entry[u], rdat, i + GD_MAX_LINCOM,
                  search_meta, mode))
            {
              dreturn("%i", -1);
              return -1;
            }
          break;
        case GD_POLYNOM_ENTRY:
          for (i = 0; i <= D->entry[u]->EN(polynom,poly_ord); ++i)
            if (_GD_UpdateScalar(D, D->entry[u], rdat, i, search_meta, mode)) {
              dreturn("%i", -1);
              return -1;
            }
          break;
        case GD_BIT_ENTRY:
        case GD_SBIT_ENTRY:
        case GD_MPLEX_ENTRY:
          if (_GD_UpdateScalar(D,D->entry[u], rdat, 1, search_meta, mode)) {
            dreturn("%i", -1);
            return -1;
          }
          /* Fallthrough */
        case GD_PHASE_ENTRY:
        case GD_RAW_ENTRY:
        case GD_RECIP_ENTRY:
        case GD_WINDOW_ENTRY:
          if (_GD_UpdateScalar(D,D->entry[u], rdat, 0, search_meta, mode)) {
            dreturn("%i", -1);
            return -1;
          }
          break;
        case GD_NO_ENTRY:
        case GD_LINTERP_ENTRY:
        case GD_MULTIPLY_ENTRY:
        case GD_DIVIDE_ENTRY:
        case GD_INDEX_ENTRY:
        case GD_STRING_ENTRY:
        case GD_CONST_ENTRY:
        case GD_CARRAY_ENTRY:
        case GD_ALIAS_ENTRY:
          break;
      }
    if (update_aliases && D->entry[u]->field_type == GD_ALIAS_ENTRY)
      if (_GD_UpdateInField(D, D->entry[u], rdat, 0, search_meta, amode)) {
        dreturn("%i", -1);
        return -1;
      }
  }

  dreturn("%i", 0);
  return 0;
}

/* delete rename data */
void _GD_CleanUpRename(struct gd_rename_data_ *rdat, int abort)
{
  int i;

  dtrace("%p, %i", rdat, abort);

  if (rdat) {
    if (abort) {
      if (rdat->meta_name)
        for (i = 0; i < rdat->n_meta; ++i)
          free(rdat->meta_name[i]);
      if (rdat->code_list)
        for (i = 0; i < rdat->n_code; ++i)
          free(rdat->code_list[i]);
    }
    free(rdat->code_list);
    free(rdat->meta_name);
    free(rdat->old_code);
    free(rdat);
  }

  dreturnvoid();
}

/* perform a database update due to a renamed field */
void _GD_PerformRename(DIRFILE *restrict D,
    struct gd_rename_data_ *restrict rdat)
{
  int i;

  dtrace("%p, %p", D, rdat);

  if (rdat == NULL) {
    dreturnvoid();
    return;
  }

  /* update meta field names */
  for (i = 0; i < rdat->n_meta; ++i) {
    free(rdat->meta_entry[i]->field);
    rdat->meta_entry[i]->field = rdat->meta_name[i];
  }

  /* rename the field */
  free(rdat->E->field);
  rdat->E->field = rdat->new_code;

  /* update derived/client fields/aliases */
  _GD_UpdateInputs(D, rdat, 1);

  /* Update the dot list */
  if (rdat->old_dot && !rdat->new_dot) {
    memmove(D->dot_list + rdat->dot_ind, D->dot_list + rdat->dot_ind + 1,
        sizeof(gd_entry_t*) * (--D->n_dot - rdat->dot_ind));
    rdat->E->flags &= ~GD_EN_DOTTED;
  } else if (rdat->new_dot && !rdat->old_dot) {
    D->dot_list[D->n_dot++] = rdat->E;
    rdat->E->flags |= GD_EN_DOTTED;
  }

  /* re-sort the lists */
  qsort(D->entry, D->n_entries, sizeof(gd_entry_t*), _GD_EntryCmp);
  if (D->dot_list && rdat->new_dot)
    qsort(D->dot_list, D->n_dot, sizeof(gd_entry_t*), _GD_EntryCmp);

  /* Invalidate the field lists */
  if (rdat->E->e->n_meta == -1) {
    rdat->E->e->p.parent->e->value_list_validity = 0;
    rdat->E->e->p.parent->e->entry_list_validity = 0;
  } else {
    D->value_list_validity = 0;
    D->entry_list_validity = 0;
  }

  /* rehash the aliases */
  _GD_UpdateAliases(D, 1);

  /* done */
  _GD_CleanUpRename(rdat, 0);

  dreturnvoid();
}

/* prepare for a database update due to a renamed field */
struct gd_rename_data_ *_GD_PrepareRename(DIRFILE *restrict D,
    char *restrict new_code, gd_entry_t *restrict E, int old_dot,
    unsigned dot_ind, int new_dot, unsigned flags)
{
  int i;
  struct gd_rename_data_ *rdat;

  dtrace("%p, \"%s\", %p, %i, %u, %i, 0x%X", D, new_code, E, old_dot,
      dot_ind, new_dot, flags);

  /* Resize the dot list; this must be done early in case it fails; it's
   * not a big deal if something else fails later: it will just be slightly too
   * big */
  if (new_dot && !old_dot) {
    gd_entry_t** ptr = _GD_Realloc(D, D->dot_list, sizeof(*ptr) *
        (D->n_dot + 1));

    if (ptr == NULL) {
      dreturn("%p", NULL);
      return NULL;
    }

    D->dot_list = ptr;
  }

  rdat = _GD_Malloc(D, sizeof(*rdat));
  if (rdat == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }
  memset(rdat, 0, sizeof(*rdat));

  rdat->n_meta = E->e->n_meta;
  rdat->meta_entry = E->e->p.meta_entry;
  rdat->flags = flags;
  rdat->E = E;
  rdat->old_code = _GD_Strdup(D, E->field);
  rdat->old_len = strlen(E->field);
  rdat->new_code = new_code;
  rdat->new_len = strlen(new_code);
  rdat->new_dot = new_dot;
  rdat->old_dot = old_dot;
  rdat->dot_ind = dot_ind;

  /* resolve field type */
  if (E->field_type == GD_ALIAS_ENTRY && E->e->entry[0])
    rdat->type = E->e->entry[0]->field_type;
  else
    rdat->type = E->field_type;

  /* update other fields' metadata */
  if (_GD_UpdateInputs(D, rdat, 0)) {
    _GD_CleanUpRename(rdat, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  if (rdat->n_meta > 0) {
    /* compose all the new meta field names under a top-level field.  We must do
     * this now in a temporary location in case it fails and/or subsequent stuff
     * fails */
    rdat->meta_name = _GD_Malloc(D, sizeof(*rdat->meta_name) * rdat->n_meta);
    if (!rdat->meta_name) {
      _GD_CleanUpRename(rdat, 1);
      dreturn("%p", NULL);
      return NULL;
    }

    memset(rdat->meta_name, 0, sizeof(char *) * rdat->n_meta);
    for (i = 0; i < rdat->n_meta; ++i) {
      rdat->meta_name[i] = _GD_Malloc(D, strlen(rdat->meta_entry[i]->field)
          + rdat->new_len - rdat->old_len + 1);
      if (rdat->meta_name[i] == NULL)
        break;
      sprintf(rdat->meta_name[i], "%s/%s", new_code, rdat->meta_entry[i]->field
          + rdat->old_len + 1);
    }
  }

  if (D->error) {
    _GD_CleanUpRename(rdat, 1);
    dreturn("%p", NULL);
    return NULL;
  }

  dreturn("%p", rdat);
  return rdat;
}

static int _GD_Rename(DIRFILE *D, gd_entry_t *E, const char *new_name,
    int old_dot, unsigned dot_ind, unsigned flags)
{
  gd_entry_t *Q;
  char *name;
  int new_dot;
  struct gd_rename_data_ *rdat = NULL;

  dtrace("%p, %p, \"%s\", %i, %u, 0x%X", D, E, new_name, old_dot, dot_ind,
      flags);

  if (_GD_ValidateField(new_name, D->standards, 1, GD_VF_CODE, &new_dot)) {
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
    sprintf(name, "%s/%s", E->e->p.parent->field, new_name);
  } else {
    /* Verify prefix and suffix */
    if (_GD_CheckCodeAffixes(D, new_name, E->fragment_index, 1)) {
      dreturn("%i", -1);
      return -1;
    }

    name = _GD_Strdup(D, new_name);
    if (name == NULL) {
      dreturn("%i", -1);
      return -1;
    }
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

  /* prep for metadata update */
  rdat = _GD_PrepareRename(D, name, E, old_dot, dot_ind, new_dot, flags);

  if (rdat == NULL) {
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

    if (flags & GD_REN_DATA) {
      struct gd_raw_file_ temp;

      /* check data protection */
      if (D->fragment[E->fragment_index].protection & GD_PROTECT_DATA) {
        _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_DATA, NULL, 0,
            D->fragment[E->fragment_index].cname);
        free(name);
        free(filebase);
        dreturn("%i", -1);
        return -1;
      }

      if (!_GD_Supports(D, E, GD_EF_NAME | GD_EF_MOVE)) {
        free(name);
        free(filebase);
        dreturn("%i", -1);
        return -1;
      }

      memcpy(&temp, E->e->u.raw.file, sizeof(struct gd_raw_file_));
      temp.name = NULL;
      if ((*_GD_ef[temp.subenc].name)(D,
            (const char*)D->fragment[E->fragment_index].enc_data, &temp,
            filebase, 0, 0))
      {
        free(name);
        free(filebase);
        dreturn("%i", -1);
        return -1;
      }

      if ((*_GD_ef[temp.subenc].name)(D,
            (const char*)D->fragment[E->fragment_index].enc_data,
            E->e->u.raw.file, E->e->u.raw.filebase, 0, 0))
      {
        free(name);
        free(filebase);
        dreturn("%i", -1);
        return -1;
      }

      if ((*_GD_ef[E->e->u.raw.file[0].subenc].move)(
            D->fragment[E->fragment_index].dirfd, E->e->u.raw.file,
            D->fragment[E->fragment_index].dirfd, temp.name))
      {
        _GD_SetEncIOError(D, GD_E_IO_RENAME, E->e->u.raw.file + 0);
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

  D->fragment[E->fragment_index].modified = 1;

  /* Update database metadata */
  _GD_PerformRename(D, rdat);

  D->flags &= ~GD_HAVE_VERSION;

  dreturn("%i", 0);
  return 0;
}

int gd_rename(DIRFILE *D, const char *old_code, const char *new_name,
    unsigned flags)
{
  gd_entry_t *E = NULL;
  int ret, old_dot = 0;
  unsigned dot_ind = 0;

  dtrace("%p, \"%s\", \"%s\", 0x%X", D, old_code, new_name, flags);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ClearError(D);

  /* check access mode */
  if ((D->flags & GD_ACCMODE) == GD_RDONLY) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* check for a dotted field name */
  if (D->n_dot > 0)
    E = _GD_FindField(D, old_code, D->dot_list, D->n_dot, 0, &dot_ind);

  if (E)
    old_dot = 1;
  else
    E = _GD_FindField(D, old_code, D->entry, D->n_entries, 0, NULL);

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

  /* check metadata protection */
  if (D->fragment[E->fragment_index].protection & GD_PROTECT_FORMAT) {
    _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[E->fragment_index].cname);
    dreturn("%i", -1);
    return -1;
  }

  ret = _GD_Rename(D, E, new_name, old_dot, dot_ind, flags);

  dreturn("%i", ret);
  return ret;
}
