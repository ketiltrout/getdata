/* Copyright (C) 2008-2016 D. V. Wiebe
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

void _GD_FreeFL(struct gd_flist_ *fl)
{
  int i;

  dtrace("%p", fl);

  for (i = 0; i < GD_N_ENTRY_LISTS; ++i)
    free(fl->entry_list[i]);
  free(fl->string_value_list);
  free(fl->const_value_list);

  if (fl->carray_value_list) {
    for (i = 0; fl->carray_value_list[i].n != 0; ++i)
      free(fl->carray_value_list[i].d);
    free(fl->carray_value_list);
  }

  if (fl->sarray_value_list) {
    for (i = 0; fl->sarray_value_list[i] != NULL; ++i)
      free(fl->sarray_value_list[i]);
    free(fl->sarray_value_list);
  }

  dreturnvoid();
}

void _GD_FreeE(DIRFILE *restrict D, gd_entry_t *restrict entry, int priv)
{
  int i, j;
  size_t n;

  /* If priv == 0, then we've been called via gd_free_entry_strings and
   * need to use the caller's free function on the public strings */
  void (*free_)(void*) = priv ? free : _GD_CFree;

  dtrace("%p, %p, %i", D, entry, priv);

  if (!entry || entry->field_type == GD_NO_ENTRY) {
    dreturnvoid();
    return;
  }

  free_(entry->field);

  switch(entry->field_type) {
    case GD_LINCOM_ENTRY:
      /* Don't trust the caller */
      j = entry->EN(lincom,n_fields);
      if (j > GD_MAX_LINCOM)
        j = GD_MAX_LINCOM;

      for (i = 0; i < j; ++i) {
        free_(entry->in_fields[i]);
        free_(entry->scalar[i]);
        free_(entry->scalar[i + GD_MAX_LINCOM]);
      }
      break;
    case GD_LINTERP_ENTRY:
      free_(entry->in_fields[0]);
      free_(entry->EN(linterp,table));
      if (priv) {
        if (entry->e->u.linterp.table_dirfd > 0)
          _GD_ReleaseDir(D, entry->e->u.linterp.table_dirfd);
        free(entry->e->u.linterp.table_file);
        free(entry->e->u.linterp.lut);
      } else
        entry->EN(linterp,table) = NULL;
      break;
    case GD_DIVIDE_ENTRY:
    case GD_MULTIPLY_ENTRY:
    case GD_INDIR_ENTRY:
    case GD_SINDIR_ENTRY:
      free_(entry->in_fields[1]);
      free_(entry->in_fields[0]);
      break;
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
      free_(entry->scalar[0]);
      free_(entry->scalar[1]);
      free_(entry->in_fields[0]);
      break;
    case GD_PHASE_ENTRY:
    case GD_RECIP_ENTRY:
      free_(entry->scalar[0]);
      free_(entry->in_fields[0]);
      break;
    case GD_POLYNOM_ENTRY:
      free_(entry->in_fields[0]);

      /* Don't trust the caller */
      j = entry->EN(polynom,poly_ord);
      if (j > GD_MAX_POLYORD)
        j = GD_MAX_POLYORD;

      for (i = 0; i <= j; ++i)
        free_(entry->scalar[i]);
      break;
    case GD_STRING_ENTRY:
      if (priv)
        free(entry->e->u.string);
      break;
    case GD_SARRAY_ENTRY:
      if (priv) {
        for (n = 0; n < entry->EN(scalar,array_len); ++n)
          free(((char **)entry->e->u.scalar.d)[n]);
        free(entry->e->u.scalar.d);
      }
      break;
    case GD_CONST_ENTRY:
    case GD_CARRAY_ENTRY:
      if (priv) {
        free(entry->e->u.scalar.client);
        free(entry->e->u.scalar.d);
      }
      break;
    case GD_RAW_ENTRY:
      free_(entry->scalar[0]);
      if (priv) {
        free(entry->e->u.raw.filebase);
        free(entry->e->u.raw.file[0].name);
        free(entry->e->u.raw.file[1].name);
      }
      break;
    case GD_WINDOW_ENTRY:
      free_(entry->scalar[0]);
      free_(entry->in_fields[0]);
      free_(entry->in_fields[1]);
      break;
    case GD_MPLEX_ENTRY:
      free_(entry->scalar[0]);
      free_(entry->scalar[1]);
      free_(entry->in_fields[0]);
      free_(entry->in_fields[1]);
      break;
    case GD_ALIAS_ENTRY:
      free_(entry->in_fields[0]);
      break;
    case GD_INDEX_ENTRY:
    case GD_NO_ENTRY:
      break;
  }

  if (priv) {
    free(entry->e->alias_list);
    _GD_FreeFL(&entry->e->fl);

    if (entry->e->n_meta > -1)
      free(entry->e->p.meta_entry);
    free(entry->e);
    free(entry);
  } else {
    entry->field = NULL;
    memset(entry->in_fields, 0, sizeof(char*) * GD_MAX_LINCOM);
    memset(entry->scalar, 0, sizeof(char*) * GD_MAX_LINCOM * 2);
  }

  dreturnvoid();
  return;
}

gd_entry_t* gd_free_entry_strings(gd_entry_t* entry) gd_nothrow
{
  dtrace("%p", entry);

  _GD_FreeE(NULL, entry, 0);

  dreturn("%p", entry);
  return entry;
}

int _GD_GetScalar(DIRFILE *restrict D, const char *restrict scalar,
    int *index_in, gd_type_t type, void *restrict data, gd_entry_t *E)
{
  gd_entry_t* C = NULL;
  int repr, e = 0;
  int index = *index_in;

  dtrace("%p, \"%s\", %p(%i), 0x%02X, %p, %p", D, scalar, index_in, *index_in,
      type, data, E);

  C = _GD_FindFieldAndRepr(D, scalar, &repr, NULL, 0);

  if (C == NULL)
    e = GD_E_SCALAR_CODE;
  else if (C->field_type != GD_CONST_ENTRY &&
      C->field_type != GD_CARRAY_ENTRY)
  {
    e = GD_E_SCALAR_TYPE;
  } else {
    if (C->field_type == GD_CONST_ENTRY) {
      /* Ignore and reset index */
      if (index != -1)
        *index_in = -1;
      index = 0;
    } else if (index < 0) {
      /* a CARRAY masquerading as a CONST; default to the first element */
      index = *index_in = 0;
    }

    _GD_DoField(D, C, repr, index, 1, type, data);

    if (E && (D->flags & GD_ACCMODE) == GD_RDWR) {
      void *ptr = _GD_Realloc(D, C->e->u.scalar.client,
          (C->e->u.scalar.n_client + 1) * sizeof(gd_entry_t*));

      if (ptr) {
        C->e->u.scalar.client = (gd_entry_t **)ptr;
        C->e->u.scalar.client[C->e->u.scalar.n_client++] = E;
      }
    }
  }

  dreturn("%i", e);
  return e;
}

/* Like _GD_GetScalar, but with more error checking.  And a different calling
 * convention */
static int _GD_GetScalar2(DIRFILE *restrict D, gd_entry_t *restrict E, int i,
    gd_type_t type, void *restrict data, int err)
{
  int e = 0;

  dtrace("%p, %p, %i, 0x%02X, %p, %i", D, E, i, type, data, err);

  if (E->scalar[i]) {
    e = _GD_GetScalar(D, E->scalar[i], E->scalar_ind + i, type, data, E);
    if (e && err)
      _GD_SetError(D, GD_E_BAD_SCALAR, e, E->field, 0, E->scalar[i]);
  }

  dreturn("%i", (D->error != GD_E_OK) || e);
  return (D->error != GD_E_OK) || e;
}

/* resolve non-literal scalars */
int _GD_CalculateEntry(DIRFILE *restrict D, gd_entry_t *restrict E, int err)
{
  int i, e = 0, cs = 0;

  dtrace("%p, %p, %i", D, E, err);

  switch(E->field_type) {
    case GD_RAW_ENTRY:
      e = _GD_GetScalar2(D, E, 0, GD_UINT_TYPE, &E->EN(raw,spf), err);
      break;
    case GD_POLYNOM_ENTRY:
      for (i = 0; i <= E->EN(polynom,poly_ord); ++i) {
        if (_GD_GetScalar2(D, E, i, GD_COMPLEX128, &E->EN(polynom,ca)[i], err))
          e = 1;
        else {
          E->EN(polynom,a)[i] = creal(E->EN(polynom,ca)[i]);

          if (!cs && cimag(E->EN(polynom,ca)[i]))
            cs = 1;
        }

        if (D->error)
          break;
      }
      break;
    case GD_LINCOM_ENTRY:
      for (i = 0; i < E->EN(lincom,n_fields); ++i) {
        if (_GD_GetScalar2(D, E, i, GD_COMPLEX128, &E->EN(lincom,cm)[i], err))
          e = 1;
        else {
          E->EN(lincom,m)[i] = creal(E->EN(lincom,cm)[i]);

          if (!cs && cimag(E->EN(lincom,cm)[i]))
            cs = 1;
        }

        if (_GD_GetScalar2(D, E, i + GD_MAX_LINCOM, GD_COMPLEX128,
            &E->EN(lincom,cb)[i], err))
        {
          e = 1;
        } else {
          E->EN(lincom,b)[i] = creal(E->EN(lincom,cb)[i]);

          if (!cs && cimag(E->EN(lincom,cb)[i]))
            cs = 1;
        }

        if (D->error)
          break;
      }
      break;
    case GD_RECIP_ENTRY:
      if (_GD_GetScalar2(D, E, 0, GD_COMPLEX128, &E->EN(recip,cdividend), err))
        e = 1;
      else {
        E->EN(recip,dividend) = creal(E->EN(recip,cdividend));
        if (cimag(E->EN(recip,cdividend)))
          cs = 1;
      }
      break;
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
      e = _GD_GetScalar2(D, E, 0, GD_INT_TYPE, &E->EN(bit,bitnum), err);
      e |= _GD_GetScalar2(D, E, 1, GD_INT_TYPE, &E->EN(bit,numbits), err);
      break;
    case GD_PHASE_ENTRY:
      e = _GD_GetScalar2(D, E, 0, GD_INT64, &E->EN(phase,shift), err);
      break;
    case GD_WINDOW_ENTRY:
      switch (E->EN(window,windop)) {
        case GD_WINDOP_EQ:
        case GD_WINDOP_NE:
          e = _GD_GetScalar2(D, E, 0, GD_INT64, &E->EN(window,threshold.i),
              err);
          break;
        case GD_WINDOP_SET:
        case GD_WINDOP_CLR:
          e = _GD_GetScalar2(D, E, 0, GD_UINT64, &E->EN(window,threshold.u),
              err);
          break;
        default:
          e = _GD_GetScalar2(D, E, 0, GD_FLOAT64, &E->EN(window,threshold.r),
              err);
          break;
      }
      break;
    case GD_MPLEX_ENTRY:
      e = _GD_GetScalar2(D, E, 0, GD_INT_TYPE, &E->EN(mplex,count_val), err);
      e |= _GD_GetScalar2(D, E, 1, GD_INT_TYPE, &E->EN(mplex,period), err);
      break;
    case GD_NO_ENTRY:
    case GD_LINTERP_ENTRY:
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
    case GD_STRING_ENTRY:
    case GD_CONST_ENTRY:
    case GD_CARRAY_ENTRY:
    case GD_SARRAY_ENTRY:
    case GD_INDIR_ENTRY:
    case GD_SINDIR_ENTRY:
    case GD_INDEX_ENTRY:
    case GD_ALIAS_ENTRY:
      break;
  }

  if (!e)
    E->flags |= GD_EN_CALC;

  if (cs)
    E->flags |= GD_EN_COMPSCAL;
  else
    E->flags &= ~GD_EN_COMPSCAL;

  dreturn("%i", !e);
  return !e;
}

char* gd_raw_filename(DIRFILE* D, const char* field_code) gd_nothrow
{
  char *filename0, *filename;
  gd_entry_t *E;

  dtrace("%p, \"%s\"", D, field_code);

  GD_RETURN_IF_INVALID(D, "%p", NULL);

  /* Check field */
  E = _GD_FindEntry(D, field_code);

  if (D->error) {
    dreturn("%p", NULL);
    return NULL;
  }

  if (E->field_type != GD_RAW_ENTRY) {
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
    dreturn("%p", NULL);
    return NULL;
  }

  if (E->e->u.raw.file[0].name == NULL) {
    /* ensure encoding subtype is known */
    if (!_GD_Supports(D, E, GD_EF_NAME)) {
      dreturn("%p", NULL);
      return NULL;
    }

    if (E->e->u.raw.file[0].subenc == GD_ENC_UNKNOWN) {
      _GD_SetError(D, GD_E_UNKNOWN_ENCODING, GD_E_UNENC_UNDET, NULL, 0, NULL);
      dreturn("%p", NULL);
      return NULL;
    } else if ((*_GD_ef[E->e->u.raw.file[0].subenc].name)(D,
          (const char*)D->fragment[E->fragment_index].enc_data,
          E->e->u.raw.file, E->e->u.raw.filebase, 0, 0))
    {
      dreturn("%p", NULL);
      return NULL;
    }
  }

  filename0 = _GD_MakeFullPath(D, D->fragment[E->fragment_index].dirfd,
      E->e->u.raw.file->name, 1);

  if (D->error) {
    dreturn("%p", NULL);
    return NULL;
  }

  /* move to caller's heap */
  if (_GD_CMalloc != malloc) {
    filename = _GD_CStrdup(filename0);
    free(filename0);
    if (filename == NULL)
      _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
  } else
    filename = filename0;

  dreturn("%p", filename);
  return filename;
}

int gd_entry(DIRFILE* D, const char* field_code, gd_entry_t* entry) gd_nothrow
{
  int i;
  gd_entry_t *E;

  dtrace("%p, \"%s\", %p", D, field_code, entry);

  GD_RETURN_ERR_IF_INVALID(D);

  E = _GD_FindEntry(D, field_code);

  if (D->error) 
    GD_RETURN_ERROR(D);

  /* Calculate the entry, if necessary */
  if (!(E->flags & GD_EN_CALC))
    _GD_CalculateEntry(D, E, 0);

  /* now copy to the user supplied buffer */
  memcpy(entry, E, sizeof(gd_entry_t));
  entry->e = NULL;

  /* duplicate strings using the caller's memory manager */
  entry->field = _GD_CStrdup(E->field);

  switch(E->field_type) {
    case GD_LINCOM_ENTRY:
      for (i = 0; i < E->EN(lincom,n_fields); ++i) {
        entry->in_fields[i] = _GD_CStrdup(E->in_fields[i]);
        if (E->scalar[i])
          entry->scalar[i] = _GD_CStrdup(E->scalar[i]);
        if (E->scalar[i + GD_MAX_LINCOM])
          entry->scalar[i + GD_MAX_LINCOM] = _GD_CStrdup(E->scalar[i +
              GD_MAX_LINCOM]);
      }
      break;
    case GD_LINTERP_ENTRY:
      entry->in_fields[0] = _GD_CStrdup(E->in_fields[0]);
      entry->EN(linterp,table) = _GD_CStrdup(E->EN(linterp,table));
      break;
    case GD_POLYNOM_ENTRY:
      entry->in_fields[0] = _GD_CStrdup(E->in_fields[0]);
      for (i = 0; i <= E->EN(polynom,poly_ord); ++i)
        if (E->scalar[i])
          entry->scalar[i] = _GD_CStrdup(E->scalar[i]);
      break;
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
    case GD_INDIR_ENTRY:
    case GD_SINDIR_ENTRY:
      entry->in_fields[0] = _GD_CStrdup(E->in_fields[0]);
      entry->in_fields[1] = _GD_CStrdup(E->in_fields[1]);
      break;
    case GD_RECIP_ENTRY:
    case GD_PHASE_ENTRY:
      entry->in_fields[0] = _GD_CStrdup(E->in_fields[0]);
      if (E->scalar[0])
        entry->scalar[0] = _GD_CStrdup(E->scalar[0]);
      break;
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
      entry->in_fields[0] = _GD_CStrdup(E->in_fields[0]);
      if (E->scalar[0])
        entry->scalar[0] = _GD_CStrdup(E->scalar[0]);
      if (E->scalar[1])
        entry->scalar[1] = _GD_CStrdup(E->scalar[1]);
      break;
    case GD_RAW_ENTRY:
      if (E->scalar[0])
        entry->scalar[0] = _GD_CStrdup(E->scalar[0]);
      break;
    case GD_WINDOW_ENTRY:
      entry->in_fields[0] = _GD_CStrdup(E->in_fields[0]);
      entry->in_fields[1] = _GD_CStrdup(E->in_fields[1]);
      if (E->scalar[0])
        entry->scalar[0] = _GD_CStrdup(E->scalar[0]);
      break;
    case GD_MPLEX_ENTRY:
      entry->in_fields[0] = _GD_CStrdup(E->in_fields[0]);
      entry->in_fields[1] = _GD_CStrdup(E->in_fields[1]);
      if (E->scalar[0])
        entry->scalar[0] = _GD_CStrdup(E->scalar[0]);
      if (E->scalar[1])
        entry->scalar[1] = _GD_CStrdup(E->scalar[1]);
      break;
    case GD_INDEX_ENTRY:
    case GD_CONST_ENTRY:
    case GD_CARRAY_ENTRY:
    case GD_SARRAY_ENTRY:
    case GD_STRING_ENTRY:
    case GD_NO_ENTRY:
    case GD_ALIAS_ENTRY:
      break;
  }

  dreturn("%i", 0);
  return 0;
}

const char *gd_alias_target(DIRFILE *D, const char *field_code) gd_nothrow
{
  gd_entry_t *E;

  dtrace("%p, \"%s\"", D, field_code);

  GD_RETURN_IF_INVALID(D, "%p", NULL);

  E = _GD_FindField(D, field_code, strlen(field_code), D->entry, D->n_entries,
      0, NULL);

  if (E == NULL) {
    _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_MISSING, NULL, 0, field_code);
    dreturn("%p", NULL);
    return NULL;
  }

  if (E->field_type != GD_ALIAS_ENTRY) {
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
    dreturn("%p", NULL);
    return NULL;
  }

  dreturn("\"%s\"", E->in_fields[0]);
  return E->in_fields[0];
}

const char **gd_aliases(DIRFILE *D, const char *field_code) gd_nothrow
{
  gd_entry_t *E;
  unsigned u, n = 1, len = 10;

  dtrace("%p, \"%s\"", D, field_code);

  GD_RETURN_IF_INVALID(D, "%p", NULL);

  E = _GD_FindEntry(D, field_code);

  if (E == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  free(E->e->alias_list);
  E->e->alias_list = _GD_Malloc(D, sizeof(const char *) * len);

  if (D->error) {
    dreturn("%p", NULL);
    return NULL;
  }

  E->e->alias_list[0] = E->field;

  for (u = 0; u < D->n_entries; ++u)
    if (D->entry[u]->field_type == GD_ALIAS_ENTRY &&
        D->entry[u]->e->entry[0] == E)
    {
      if (n == len - 1) {
        const char **ptr = _GD_Realloc(D, E->e->alias_list,
            sizeof(const char*) * (len *= 2));
        if (ptr == NULL) {
          free(E->e->alias_list);
          E->e->alias_list = NULL;
          dreturn("%p", NULL);
          return NULL;
        }
        E->e->alias_list = ptr;
      }
      E->e->alias_list[n++] = D->entry[u]->field;
    }

  /* terminate */
  E->e->alias_list[n] = NULL;

  dreturn("%p", E->e->alias_list);
  return E->e->alias_list;
}

unsigned int gd_naliases(DIRFILE *D, const char *field_code) gd_nothrow
{
  gd_entry_t *E;
  unsigned u, n = 1;

  dtrace("%p, \"%s\"", D, field_code);

  GD_RETURN_IF_INVALID(D, "%u", 0);

  E = _GD_FindEntry(D, field_code);

  if (E == NULL) {
    dreturn("%u", 0);
    return 0;
  }

  for (u = 0; u < D->n_entries; ++u)
    if (D->entry[u]->field_type == GD_ALIAS_ENTRY &&
        D->entry[u]->e->entry[0] == E)
    {
      n++;
    }

  dreturn("%u", n);
  return n;
}

gd_entype_t gd_entry_type(DIRFILE* D, const char* field_code) gd_nothrow
{
  gd_entry_t* E;

  dtrace("%p, \"%s\"", D, field_code);

  GD_RETURN_IF_INVALID(D, "%i", GD_NO_ENTRY);

  E = _GD_FindEntry(D, field_code);

  if (D->error) {
    dreturn("%i", GD_NO_ENTRY);
    return GD_NO_ENTRY;
  }

  dreturn("%i", E->field_type);
  return E->field_type;
}

int gd_fragment_index(DIRFILE *D, const char *field_code) gd_nothrow
{
  gd_entry_t *E;

  dtrace("%p, \"%s\"", D, field_code);

  GD_RETURN_ERR_IF_INVALID(D);

  E = _GD_FindField(D, field_code, strlen(field_code), D->entry, D->n_entries,
      0, NULL);

  if (E == NULL)
    GD_SET_RETURN_ERROR(D, GD_E_BAD_CODE, GD_E_CODE_MISSING, NULL, 0,
        field_code);

  dreturn("%i", E->fragment_index);
  return E->fragment_index;
}

int gd_hide(DIRFILE *D, const char *field_code) gd_nothrow
{
  gd_entry_t *E;

  dtrace("%p, \"%s\"", D, field_code);

  GD_RETURN_ERR_IF_INVALID(D);

  if ((D->flags & GD_ACCMODE) != GD_RDWR)
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
  else {
    E = _GD_FindField(D, field_code, strlen(field_code), D->entry, D->n_entries,
        0, NULL);

    if (E == NULL)
      _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_MISSING, NULL, 0, field_code);
    else if (D->fragment[E->fragment_index].protection & GD_PROTECT_FORMAT)
      _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
          D->fragment[E->fragment_index].cname);
    else if (!(E->flags & GD_EN_HIDDEN)) {
      E->flags |= GD_EN_HIDDEN;
      D->fragment[E->fragment_index].modified = 1;
    }
  }

  GD_RETURN_ERROR(D);
}

int gd_hidden(DIRFILE *D, const char *field_code) gd_nothrow
{
  gd_entry_t *E;

  dtrace("%p, \"%s\"", D, field_code);

  GD_RETURN_ERR_IF_INVALID(D);

  E = _GD_FindField(D, field_code, strlen(field_code), D->entry, D->n_entries,
      0, NULL);

  if (E == NULL)
    GD_SET_RETURN_ERROR(D, GD_E_BAD_CODE, GD_E_CODE_MISSING, NULL, 0,
        field_code);

  dreturn("%i", (E->flags & GD_EN_HIDDEN) ? 1 : 0);
  return (E->flags & GD_EN_HIDDEN) ? 1 : 0;
}

int gd_unhide(DIRFILE *D, const char *field_code) gd_nothrow
{
  gd_entry_t *E;

  dtrace("%p, \"%s\"", D, field_code);

  GD_RETURN_ERR_IF_INVALID(D);

  if ((D->flags & GD_ACCMODE) != GD_RDWR)
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
  else {
    E = _GD_FindField(D, field_code, strlen(field_code), D->entry, D->n_entries,
        0, NULL);

    if (E == NULL)
      _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_MISSING, NULL, 0, field_code);
    else if (D->fragment[E->fragment_index].protection & GD_PROTECT_FORMAT)
      _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
          D->fragment[E->fragment_index].cname);
    else if (E->flags & GD_EN_HIDDEN) {
      E->flags &= ~GD_EN_HIDDEN;
      D->fragment[E->fragment_index].modified = 1;
    }
  }

  GD_RETURN_ERROR(D);
}

/* This function silently drops a representation suffix from field_code. */
int gd_validate(DIRFILE *D, const char *field_code) gd_nothrow
{
  int repr;
  gd_entry_t* E;

  dtrace("%p, \"%s\"", D, field_code);

  GD_RETURN_ERR_IF_INVALID(D);

  E = _GD_FindFieldAndRepr(D, field_code, &repr, NULL, 1);

  if (D->error)
    GD_RETURN_ERROR(D);

  /* calculate scalars */
  if (!(E->flags & GD_EN_CALC))
    _GD_CalculateEntry(D, E, 1);

  if (D->error)
    GD_RETURN_ERROR(D);

  /* check input fields */
  _GD_FindInputs(D, E, 1);

  GD_RETURN_ERROR(D);
}

/* Ensure that an input field has been identified (with error checking) */
static int _GD_BadInput(DIRFILE *D, const gd_entry_t *E, int i, gd_entype_t t,
    int err)
{
  dtrace("%p, %p, %i, 0x%X, %i", D, E, i, t, err);

  if (E->e->entry[i]) { /* Job done */
    dreturn("%i", 0);
    return 0;
  }

  if (E->e->entry[i] == NULL) {
    E->e->entry[i] = _GD_FindFieldAndRepr(D, E->in_fields[i], &E->e->repr[i],
        NULL, err);

    if (E->e->entry[i] == NULL) {
      dreturn("%i", 1);
      return 1;
    }
  }

  /* check field type */
  if (t == GD_NO_ENTRY) {
    /* scalar entries not allowed */
    if (E->e->entry[i]->field_type & GD_SCALAR_ENTRY_BIT) {
      _GD_SetError(D, GD_E_DIMENSION, GD_E_DIM_FORMAT, E->field, 0,
          E->e->entry[i]->field);
      E->e->entry[i] = NULL;
      dreturn("%i", 1);
      return 1;
    }
  } else if (E->e->entry[i]->field_type != t) {
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_FORMAT, E->field, 0,
        E->e->entry[i]->field);
    E->e->entry[i] = NULL;
    dreturn("%i", 1);
    return 1;
  }

  dreturn("%i", 0);
  return 0;
}

int _GD_FindInputs(DIRFILE *D, gd_entry_t *E, int err) gd_nothrow
{
  int i;

  dtrace("%p, %p, %i", D, E, err);

  switch (E->field_type) {
    case GD_LINCOM_ENTRY:
      for (i = 0; i < E->EN(lincom,n_fields); ++i)
        _GD_BadInput(D, E, i, GD_NO_ENTRY, err);
      break;
    case GD_INDIR_ENTRY:
      _GD_BadInput(D, E, 0, GD_NO_ENTRY, err);
      _GD_BadInput(D, E, 1, GD_CARRAY_ENTRY, err);
      break;
    case GD_SINDIR_ENTRY:
      _GD_BadInput(D, E, 0, GD_NO_ENTRY, err);
      _GD_BadInput(D, E, 1, GD_SARRAY_ENTRY, err);
      break;
    case GD_DIVIDE_ENTRY:
    case GD_MULTIPLY_ENTRY:
    case GD_WINDOW_ENTRY:
    case GD_MPLEX_ENTRY:
      _GD_BadInput(D, E, 1, GD_NO_ENTRY, err);
      /* fallthrough */
    case GD_LINTERP_ENTRY:
    case GD_BIT_ENTRY:
    case GD_PHASE_ENTRY:
    case GD_POLYNOM_ENTRY:
    case GD_SBIT_ENTRY:
    case GD_RECIP_ENTRY:
      _GD_BadInput(D, E, 0, GD_NO_ENTRY, err);
      /* Fallthrough */
    case GD_RAW_ENTRY:
    case GD_CONST_ENTRY:
    case GD_CARRAY_ENTRY:
    case GD_SARRAY_ENTRY:
    case GD_STRING_ENTRY:
    case GD_INDEX_ENTRY:
    case GD_NO_ENTRY:
    case GD_ALIAS_ENTRY:
      break;
  }

  GD_RETURN_ERROR(D);
}

char *gd_linterp_tablename(DIRFILE *D, const char *field_code) gd_nothrow
{
  gd_entry_t *E;
  char *table0, *table;

  dtrace("%p, \"%s\"", D, field_code);

  GD_RETURN_IF_INVALID(D, "%p", NULL);

  E = _GD_FindEntry(D, field_code);

  if (D->error) {
    dreturn("%p", NULL);
    return NULL;
  }

  if (E->field_type != GD_LINTERP_ENTRY) {
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
    dreturn("%p", NULL);
    return NULL;
  }

  /* initialise */
  if (E->e->u.linterp.table_file == NULL)
    if (_GD_SetTablePath(D, E, E->e)) {
      dreturn("%p", NULL);
      return NULL;
    }

  table0 = _GD_MakeFullPath(D, E->e->u.linterp.table_dirfd,
      E->e->u.linterp.table_file, 1);

  if (D->error) {
    dreturn("%p", NULL);
    return NULL;
  }

  /* move to caller's heap */
  if (_GD_CMalloc != malloc) {
    table = _GD_CStrdup(table0);
    free(table0);
    if (table == NULL)
      _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
  } else
    table = table0;

  dreturn("%s", table);
  return table;
}
