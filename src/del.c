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

static int _GD_ClearInput(DIRFILE *restrict D, gd_entry_t *restrict E,
    const gd_entry_t *restrict C, int i, int check)
{
  dtrace("%p, %p, %p, %i, %i", D, E, C, i, check);

  /* If we're not checking and entry[i] is NULL, job's already done.
   * Otherwise, find this entry, if it doesn't exist. */
  if (check && E->e->entry[i] == NULL)
    E->e->entry[i] = _GD_FindFieldAndRepr(D, E->in_fields[i], &E->e->repr[i],
        NULL, 0);

  if (E->e->entry[i] == C) {
    if (check)
      GD_SET_RETURN_ERROR(D, GD_E_DELETE, GD_E_DEL_DERIVED, E->field, 0,
          C->field);
    else
      E->e->entry[i] = NULL;
  }

  dreturn("%i", 0);
  return 0;
}

static void _GD_ClearDerived(DIRFILE *restrict D, gd_entry_t *restrict E,
    const gd_entry_t *restrict C, int check)
{
  int i;

  dtrace("%p, %p, %p, %i", D, E, C, check);

  switch(E->field_type) {
    case GD_LINCOM_ENTRY:
      for (i = 0; i < E->EN(lincom,n_fields); ++i)
        if (_GD_ClearInput(D, E, C, i, check))
          break;
      break;
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
    case GD_WINDOW_ENTRY:
    case GD_MPLEX_ENTRY:
    case GD_INDIR_ENTRY:
    case GD_SINDIR_ENTRY:
      if (_GD_ClearInput(D, E, C, 1, check))
        break;
      /* Fallthrough */
    case GD_RECIP_ENTRY:
    case GD_LINTERP_ENTRY:
    case GD_BIT_ENTRY:
    case GD_PHASE_ENTRY:
    case GD_POLYNOM_ENTRY:
    case GD_SBIT_ENTRY:
      _GD_ClearInput(D, E, C, 0, check);
      break;
    case GD_NO_ENTRY:
    case GD_RAW_ENTRY:
    case GD_INDEX_ENTRY:
    case GD_CONST_ENTRY:
    case GD_CARRAY_ENTRY:
    case GD_STRING_ENTRY:
    case GD_SARRAY_ENTRY:
      break;
    case GD_ALIAS_ENTRY:
      if (E->e->entry[0] == C) {
        if (check)
          _GD_SetError(D, GD_E_DELETE, GD_E_DEL_ALIAS, E->field, 0, C->field);
        else
          E->e->entry[0] = NULL;
      }
  }

  dreturnvoid();
}

static int _GD_DeReferenceOne(DIRFILE *restrict D, gd_entry_t *restrict E,
    gd_entry_t *restrict C, int check, int i, gd_type_t type,
    void *restrict data)
{
  int repr;
  size_t len;

  dtrace("%p, %p, %p, %i, %i, 0x%03x, %p", D, E, C, check, i, type, data);

  if (E->scalar[i] != NULL) {
    len = strlen(E->scalar[i]);
    repr = _GD_GetRepr(E->scalar[i], &len);

    if (len == C->e->len && memcmp(C->field, E->scalar[i], len) == 0) {
      if (check) {
        _GD_SetError(D, GD_E_DELETE, GD_E_DEL_CONST, E->field, 0, C->field);

        dreturn("%i", 1);
        return 1;
      } else {
        _GD_DoField(D, C, repr, (C->field_type == GD_CONST_ENTRY) ? 0 :
            E->scalar_ind[i], 1, type, data);
        free(E->scalar[i]);
        E->scalar[i] = NULL;
      }
    }
  }

  dreturn("%i", 0);
  return 0;
}

static void _GD_DeReference(DIRFILE *restrict D, gd_entry_t *restrict E,
    gd_entry_t *restrict C, int check)
{
  int i;

  dtrace("%p, %p, %p, %i", D, E, C, check);

  switch(E->field_type) {
    case GD_RAW_ENTRY:
      _GD_DeReferenceOne(D, E, C, check, 0, GD_UINT_TYPE, &E->EN(raw,spf));
      break;
    case GD_POLYNOM_ENTRY:
      for (i = 0; i <= E->EN(polynom,poly_ord); ++i) {
        if (_GD_DeReferenceOne(D, E, C, check, i, GD_COMPLEX128,
              &E->EN(polynom,ca)[i]))
          break;

        if (!check)
          E->EN(polynom,a)[i] = creal(E->EN(polynom,ca)[i]);
      }
      break;
    case GD_LINCOM_ENTRY:
      for (i = 0; i < E->EN(lincom,n_fields); ++i) {
        if (_GD_DeReferenceOne(D, E, C, check, i, GD_COMPLEX128,
              &E->EN(lincom,cm)[i]))
          break;

        if (!check)
          E->EN(lincom,m)[i] = creal(E->EN(lincom,cm)[i]);

        if (_GD_DeReferenceOne(D, E, C, check, i + GD_MAX_LINCOM, GD_COMPLEX128,
              &E->EN(lincom,cb)[i]))
          break;

        if (!check)
          E->EN(lincom,b)[i] = creal(E->EN(lincom,cb)[i]);
      }
      break;
    case GD_RECIP_ENTRY:
      _GD_DeReferenceOne(D, E, C, check, 0, GD_COMPLEX128,
          &E->EN(recip,cdividend));

      if (!check)
        E->EN(recip,dividend) = creal(E->EN(recip,cdividend));
      break;
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
      if (_GD_DeReferenceOne(D, E, C, check, 0, GD_INT_TYPE,
            &E->EN(bit,bitnum)))
      {
        break;
      }

      _GD_DeReferenceOne(D, E, C, check, 1, GD_INT_TYPE, &E->EN(bit,numbits));
      break;
    case GD_PHASE_ENTRY:
      _GD_DeReferenceOne(D, E, C, check, 0, GD_INT64, &E->EN(phase,shift));
      break;
    case GD_WINDOW_ENTRY:
      switch (E->EN(window,windop)) {
        case GD_WINDOP_EQ:
        case GD_WINDOP_NE:
          _GD_DeReferenceOne(D, E, C, check, 0, GD_INT64,
              &E->EN(window,threshold.i));
          break;
        case GD_WINDOP_SET:
        case GD_WINDOP_CLR:
          _GD_DeReferenceOne(D, E, C, check, 0, GD_UINT64,
              &E->EN(window,threshold.u));
          break;
        default:
          _GD_DeReferenceOne(D, E, C, check, 0, GD_FLOAT64,
              &E->EN(window,threshold.r));
          break;
      }
      break;
    case GD_MPLEX_ENTRY:
      _GD_DeReferenceOne(D, E, C, check, 0, GD_INT_TYPE,
          &E->EN(mplex,count_val));
      _GD_DeReferenceOne(D, E, C, check, 1, GD_INT_TYPE,
          &E->EN(mplex,period));
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

  dreturnvoid();
}

static void _GD_Delete(DIRFILE *restrict D, gd_entry_t *restrict E,
    unsigned int index, unsigned int flags)
{
  int n_del, i;
  unsigned int j;
  char **new_ref = NULL;
  gd_entry_t *reference = NULL;
  gd_entry_t **del_list;

  dtrace("%p, %p, %u, 0x%X", D, E, index, flags);

  if ((D->flags & GD_ACCMODE) != GD_RDWR)
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
  else if (D->fragment[E->fragment_index].protection & GD_PROTECT_FORMAT)
    _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[E->fragment_index].cname);
  else if (E->e->n_meta > 0 && ~flags & GD_DEL_META)
    /* This field has metafields and we weren't asked to delete those too:
     * complain */
    _GD_SetError(D, GD_E_DELETE, GD_E_DEL_META, NULL, 0, E->field);

  if (D->error) {
    dreturnvoid();
    return;
  }

  /* gather a list of fields to delete (the target, plus all its metafields */
  del_list = _GD_Malloc(D, sizeof(*del_list) * (((E->e->n_meta == -1) ? 0 :
          E->e->n_meta) + 1));

  if (del_list == NULL) {
    dreturnvoid();
    return;
  }

  del_list[0] = E;
  n_del = 1;

  for (i = 0; i < E->e->n_meta; ++i)
    del_list[n_del++] = E->e->p.meta_entry[i];

  /* Check for clients, derived fields, and inbound aliases */
  if (~flags & GD_DEL_FORCE)
    for (j = 0; j < D->n_entries; ++j)
      for (i = 0; i < n_del; ++i) {
        if ((del_list[i]->field_type == GD_CONST_ENTRY ||
              del_list[i]->field_type == GD_CARRAY_ENTRY) &&
            ~flags & GD_DEL_DEREF)
        {
          _GD_DeReference(D, D->entry[j], del_list[i], 1);
        }
        if (!D->error)
          _GD_ClearDerived(D, D->entry[j], del_list[i], 1);

        if (D->error) {
          free(del_list);
          dreturnvoid();
          return;
        }
      }

  /* If this is a raw field, and we were asked to delete the data, do so */
  if (E->field_type == GD_RAW_ENTRY) {
    /* close data file, if open */
    if (_GD_FiniRawIO(D, E, E->fragment_index, GD_FINIRAW_DISCARD)) {
      free(del_list);
      dreturnvoid();
      return;
    }

    if (flags & GD_DEL_DATA) {
      /* check protection */
      if (D->fragment[E->fragment_index].protection & GD_PROTECT_DATA) {
        _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_DATA, NULL, 0,
            D->fragment[E->fragment_index].cname);
        free(del_list);
        dreturnvoid();
        return;
      }

      if (!_GD_Supports(D, E, GD_EF_NAME | GD_EF_UNLINK)) {
        free(del_list);
        dreturnvoid();
        return;
      }

      if ((*_GD_ef[E->e->u.raw.file[0].subenc].name)(D,
            (const char*)D->fragment[E->fragment_index].enc_data,
            E->e->u.raw.file, E->e->u.raw.filebase, 0, 0))
      {
        free(del_list);
        dreturnvoid();
        return;
      }

      if ((*_GD_ef[E->e->u.raw.file[0].subenc].unlink)(
            D->fragment[E->fragment_index].dirfd, E->e->u.raw.file))
      {
        if (errno != ENOENT) {
          _GD_SetEncIOError(D, GD_E_IO_UNLINK, E->e->u.raw.file + 0);
          free(del_list);
          dreturnvoid();
          return;
        }
      }
    }
  }

  /* Fix up reference fields */
  if (E->field_type == GD_RAW_ENTRY) {
    new_ref = _GD_Malloc(D, sizeof(*new_ref) * D->n_fragment);
    if (new_ref == NULL) {
      free(del_list);
      dreturnvoid();
      return;
    }
    memset(new_ref, 0, sizeof(char*) * D->n_fragment);

    for (i = 0; i < D->n_fragment; ++i)
      if (D->fragment[i].ref_name != NULL &&
          strcmp(D->fragment[i].ref_name, E->field) == 0)
      {
        /* Flag for replacement */
        new_ref[i] = (char *)E;
        /* Search for a replacement */
        for (j = 0; j < D->n_entries; ++j)
          if (j != index && D->entry[j]->field_type == GD_RAW_ENTRY) {
            /* Is this field in scope? */
            int in_scope = 0;
            int f;
            for (f = D->entry[j]->fragment_index; f != -1;
                f = D->fragment[f].parent)
              if (f == i) {
                in_scope = 1;
                break;
              }

            if (in_scope) {
              new_ref[i] = _GD_Strdup(D, D->entry[j]->field);
              if (new_ref[i] == NULL) {
                for (f = 0; f < i; ++f)
                  free(new_ref[f]);
                free(new_ref);
                free(del_list);
                dreturnvoid();
                return;
              }

              if (i == 0)
                reference = D->entry[j];

              break;
            }
          }
      }
  }

  /* Nothing from now on may fail */

  /* Fix up reference fields */
  if (reference != NULL)
    D->reference_field = reference;

  if (new_ref != NULL) {
    for (i = 0; i < D->n_fragment; ++i)
      if (new_ref[i] != NULL) {
        free(D->fragment[i].ref_name);
        D->fragment[i].ref_name = (new_ref[i] == (char *)E) ? NULL : new_ref[i];
      }
    free(new_ref);
  }

  /* Clear clients and derived fields */
  for (j = 0; j < D->n_entries; ++j)
    for (i = 0; i < n_del; ++i)
      if ((del_list[i]->field_type == GD_CONST_ENTRY ||
            del_list[i]->field_type == GD_CARRAY_ENTRY) && flags & GD_DEL_DEREF)
        _GD_DeReference(D, D->entry[j], del_list[i], 0);
      else
        _GD_ClearDerived(D, D->entry[j], del_list[i], 0);

  if (E->e->n_meta >= 0) {
    if (n_del > 1) {
      /* Sort the del list for easier searching */
      qsort(del_list + 1, n_del - 1, sizeof del_list[0], _GD_EntryCmp);

      /* Remove all meta fields -- there are no RAW fields here */
      for (i = 1, j = 0; i < n_del && j < D->n_entries; ++j) {
        if (D->entry[j] == del_list[i]) {
          _GD_FreeE(D, D->entry[j], 1);
          memmove(D->entry + j, D->entry + j + 1,
              sizeof(gd_entry_t *) * (D->n_entries - j - 1));
          D->n_entries--;
          i++;
        }
      }
    }

    /* Invalidate the field lists */
    D->fl.entry_list_validity = 0;
    D->fl.value_list_validity = 0;
  } else {
    /* If this is a metafield, update its parent's lists */
    struct gd_private_entry_ *Pe = E->e->p.parent->e;

    /* search and destroy */
    for (i = 0; i < Pe->n_meta; ++i)
      if (Pe->p.meta_entry[i] == E) {
        Pe->p.meta_entry[i] = Pe->p.meta_entry[Pe->n_meta - 1];
        break;
      }

    /* Decrement entry count */
    Pe->n_meta--;

    /* Invalidate the field lists */
    Pe->fl.entry_list_validity = 0;
    Pe->fl.value_list_validity = 0;
  }

  free(del_list);

  /* Remove the entry from the list -- we need not worry about the way we've
   * already modified D->entry, since E is guaranteed to be before the stuff
   * we've already removed */
  D->fragment[E->fragment_index].modified = 1;
  _GD_FreeE(D, E, 1);

  memmove(D->entry + index, D->entry + index + 1,
      sizeof(gd_entry_t *) * (D->n_entries - index - 1));
  D->n_entries--;

  dreturnvoid();
}

int gd_delete(DIRFILE *D, const char *field_code, unsigned int flags)
{
  unsigned index;
  gd_entry_t *E;

  dtrace("%p, \"%s\", 0x%X", D, field_code, flags);

  GD_RETURN_ERR_IF_INVALID(D);

  E = _GD_FindField(D, field_code, strlen(field_code), D->entry, D->n_entries,
      0, &index);

  if (E == NULL) 
    _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_MISSING, NULL, 0, field_code);
  else
    _GD_Delete(D, E, index, flags);

  GD_RETURN_ERROR(D);
}
