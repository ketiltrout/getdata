/* Copyright (C) 2008-2012 D. V. Wiebe
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

static void _GD_ClearDerived(DIRFILE* D, gd_entry_t* E, const gd_entry_t* C,
    int check)
{
  int i;

  dtrace("%p, %p, %p, %i", D, E, C, check);

  switch(E->field_type) {
    case GD_LINCOM_ENTRY:
      for (i = 0; i < E->EN(lincom,n_fields); ++i)
        if (strcmp(E->in_fields[i], C->field) == 0) {
          if (check)
            _GD_SetError(D, GD_E_DELETE, GD_E_DEL_DERIVED, E->field, 0,
                C->field);
          else
            E->e->entry[i] = NULL;
        }
      break;
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
    case GD_WINDOW_ENTRY:
    case GD_MPLEX_ENTRY:
      if (strcmp(E->in_fields[1], C->field) == 0) {
        if (check)
          _GD_SetError(D, GD_E_DELETE, GD_E_DEL_DERIVED, E->field, 0,
              C->field);
        else
          E->e->entry[1] = NULL;
      }
      break;
      /* Fallthrough */
    case GD_RECIP_ENTRY:
    case GD_LINTERP_ENTRY:
    case GD_BIT_ENTRY:
    case GD_PHASE_ENTRY:
    case GD_POLYNOM_ENTRY:
    case GD_SBIT_ENTRY:
      if (strcmp(E->in_fields[0], C->field) == 0) {
        if (check)
          _GD_SetError(D, GD_E_DELETE, GD_E_DEL_DERIVED, E->field, 0,
              C->field);
        else
          E->e->entry[0] = NULL;
      }
      break;
    case GD_NO_ENTRY:
    case GD_RAW_ENTRY:
    case GD_INDEX_ENTRY:
    case GD_CONST_ENTRY:
    case GD_CARRAY_ENTRY:
    case GD_STRING_ENTRY:
      break;
    default:
      if (E->field_type == GD_ALIAS_ENTRY)
        if (E->e->entry[0] == C) {
          if (check)
            _GD_SetError(D, GD_E_DELETE, GD_E_DEL_ALIAS, E->field, 0, C->field);
          else
            E->e->entry[0] = NULL;
        }
  }

  dreturnvoid();
}

static int _GD_DeReferenceOne(DIRFILE* D, gd_entry_t* E, gd_entry_t* C,
    int check, int i, gd_type_t type, void *data)
{
  int repr;
  char *field_code;

  dtrace("%p, %p, %p, %i, %i, 0x%03x, %p", D, E, C, check, i, type, data);

  if (E->scalar[i] != NULL) {
    repr = _GD_GetRepr(D, E->scalar[i], &field_code, 1);

    if (D->error) {
      dreturn("%i", 1);
      return 1;
    }

    if (strcmp(C->field, field_code) == 0) {
      if (field_code != E->scalar[i])
        free(field_code);

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
    } else if (field_code != E->scalar[i])
      free(field_code);

  }

  dreturn("%i", 0);
  return 0;
}

static void _GD_DeReference(DIRFILE* D, gd_entry_t* E, gd_entry_t* C,
    int check)
{
  int i;

  dtrace("%p, %p, %p, %i", D, E, C, check);

  switch(E->field_type) {
    case GD_RAW_ENTRY:
      _GD_DeReferenceOne(D, E, C, check, 0, GD_UINT16, &E->EN(raw,spf));
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
      if (_GD_DeReferenceOne(D, E, C, check, 0, GD_INT16, &E->EN(bit,bitnum)))
        break;

      _GD_DeReferenceOne(D, E, C, check, 1, GD_INT16, &E->EN(bit,numbits));
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
      _GD_DeReferenceOne(D, E, C, check, 0, GD_UINT16, &E->EN(mplex,count_val));
      _GD_DeReferenceOne(D, E, C, check, 1, GD_UINT16, &E->EN(mplex,count_max));
      break;
    case GD_NO_ENTRY:
    case GD_LINTERP_ENTRY:
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
    case GD_STRING_ENTRY:
    case GD_CONST_ENTRY:
    case GD_CARRAY_ENTRY:
    case GD_INDEX_ENTRY:
      break;
  }

  dreturnvoid();
}

static int _GD_Delete(DIRFILE *D, gd_entry_t *E, unsigned int index,
    unsigned int flags)
{
  unsigned int first, last = 0;
  int n_del, i, len;
  unsigned int j;
  char **new_ref = NULL;
  gd_entry_t *reference = NULL;
  gd_entry_t **del_list;

  dtrace("%p, %p, %u, 0x%X", D, E, index, flags);

  if ((D->flags & GD_ACCMODE) != GD_RDWR) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  len = strlen(E->field);

  /* check protection */
  if (D->fragment[E->fragment_index].protection & GD_PROTECT_FORMAT) {
    _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[E->fragment_index].cname);
    dreturn("%i", -1);
    return -1;
  }

  /* If this field has metafields, and we weren't asked to delete those too,
   * complain */
  if (E->e->n_meta > 0 && ~flags & GD_DEL_META) {
    _GD_SetError(D, GD_E_DELETE, GD_E_DEL_META, NULL, 0, E->field);
    dreturn("%i", -1);
    return -1;
  } else if (E->e->n_meta > 0) {
    /* find one of the meta fields -- it's not true that metafields are
     * necessarily sorted directly after their parent */
    if (_GD_FindField(D, E->e->p.meta_entry[0]->field, D->entry, D->n_entries,
          0, &first) == NULL)
    {
      _GD_InternalError(D);
      dreturn("%i", -1);
      return -1;
    }
    last = first;

    /* The remaining meta fields will be contiguous with this one, so just
     * search linearly in both directions until we find something that isn't a
     * meta field of our parent */
    while (first > 0)
      if (strncmp(D->entry[first - 1]->field, E->field, len) == 0 &&
          D->entry[first - 1]->field[len] == '/')
        first--;
      else
        break;

    while (last < D->n_entries - 1)
      if (strncmp(D->entry[last + 1]->field, E->field, len) == 0 &&
          D->entry[last + 1]->field[len] == '/')
        last++;
      else
        break;
  }

  /* gather a list of fields */
  del_list = (gd_entry_t **)_GD_Malloc(D, sizeof(gd_entry_t*) *
        (((E->e->n_meta == -1) ?  0 : E->e->n_meta) + 1));

  if (del_list == NULL) {
    dreturn("%i", -1);
    return -1;
  }

  del_list[0] = E;
  n_del = 1;

  for (i = 0; i < E->e->n_meta; ++i)
    del_list[n_del++] = E->e->p.meta_entry[i];

  /* Check for clients and derived fields */
  if (~flags & GD_DEL_FORCE)
    for (j = 0; j < D->n_entries; ++j)
      for (i = 0; i < n_del; ++i) {
        if ((del_list[i]->field_type == GD_CONST_ENTRY ||
              del_list[i]->field_type == GD_CARRAY_ENTRY) &&
            ~flags & GD_DEL_DEREF)
          _GD_DeReference(D, D->entry[j], del_list[i], 1);
        else if (del_list[i]->field_type != GD_STRING_ENTRY)
          _GD_ClearDerived(D, D->entry[j], del_list[i], 1);

        if (D->error) {
          free(del_list);
          dreturn("%i", -1);
          return -1;
        }
      }

  /* If this is a raw field, and we were asked to delete the data, do so */
  if (E->field_type == GD_RAW_ENTRY && flags & GD_DEL_DATA) {
    /* check protection */
    if (D->fragment[E->fragment_index].protection & GD_PROTECT_DATA) {
      _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_DATA, NULL, 0,
          D->fragment[E->fragment_index].cname);
      free(del_list);
      dreturn("%i", -1);
      return -1;
    }

    if (!_GD_Supports(D, E, GD_EF_NAME | GD_EF_UNLINK)) {
      free(del_list);
      dreturn("%i", -1);
      return -1;
    }

    if ((*_gd_ef[E->e->u.raw.file[0].subenc].name)(D,
          D->fragment[E->fragment_index].enc_data, E->e->u.raw.file,
          E->e->u.raw.filebase, 0, 0))
    {
      free(del_list);
      dreturn("%i", -1);
      return -1;
    }

    if ((*_gd_ef[E->e->u.raw.file[0].subenc].unlink)(
          D->fragment[E->fragment_index].dirfd, E->e->u.raw.file))
    {
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.raw.file[0].name, errno, NULL);
      free(del_list);
      dreturn("%i", -1);
      return -1;
    }
  } else if (E->field_type == GD_RAW_ENTRY && E->e->u.raw.file->idata >= 0) {
    if (_GD_FiniRawIO(D, E, E->fragment_index, GD_FINIRAW_DISCARD)) {
      free(del_list);
      dreturn("%i", -1);
      return -1;
    }
  }

  /* Fix up reference fields */
  if (E->field_type == GD_RAW_ENTRY) {
    new_ref = (char **)_GD_Malloc(D, sizeof(char*) * D->n_fragment);
    if (new_ref == NULL) {
      free(del_list);
      dreturn("%i", -1);
      return -1;
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
                dreturn("%i", -1);
                return -1;
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
      else if (del_list[i]->field_type != GD_STRING_ENTRY)
        _GD_ClearDerived(D, D->entry[j], del_list[i], 0);

  free(del_list);

  /* Remove meta fields, if present */
  if (E->e->n_meta >= 0) {
    if (E->e->n_meta > 0) {
      /* Remove all meta fields -- there are no RAW fields here */
      for (j = first; j <= last; ++j)
        _GD_FreeE(D, D->entry[j], 1);

      memmove(D->entry + first, D->entry + last + 1,
          sizeof(gd_entry_t*) * (D->n_entries - last - 1));
      D->n_meta -= last - first + 1;
      D->n_entries -= last - first + 1;
    }

    /* Decrement entry type count */
    D->n[_GD_EntryIndex(E->field_type)]--;
  } else {
    /* If this is a metafield, update its parent's lists */
    struct _gd_private_entry *Pe = E->e->p.parent->e;

    /* search and destroy */
    for (i = 0; i < Pe->n_meta; ++i)
      if (Pe->p.meta_entry[i] == E) {
        Pe->p.meta_entry[i] = Pe->p.meta_entry[Pe->n_meta - 1];
        break;
      }

    /* Decrement entry type counts */
    Pe->n_meta--;
    Pe->n[_GD_EntryIndex(E->field_type)]--;
  }

  /* Remove the entry from the list -- we need not worry about the way we've
   * already modified D->entry, since E is guaranteed to be before the stuff
   * we've already removed */
  D->fragment[E->fragment_index].modified = 1;
  _GD_FreeE(D, E, 1);

  memmove(D->entry + index, D->entry + index + 1,
      sizeof(gd_entry_t *) * (D->n_entries - index - 1));
  D->n_entries--;

  /* Invalidate the field lists */
  D->list_validity = 0;
  D->type_list_validity = 0;

  dreturn("%i", 0);
  return 0;
}

int gd_delete(DIRFILE *D, const char *field_code_in, unsigned int flags)
{
  unsigned index;
  int repr, ret;
  char *field_code;
  gd_entry_t *E;

  dtrace("%p, \"%s\", 0x%X", D, field_code_in, flags);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ClearError(D);

  E = _GD_FindFieldAndRepr(D, field_code_in, &field_code, &repr, &index, 1, 1);

  if (field_code != field_code_in)
    free(field_code);

  if (D->error) {
    dreturn("%i", -1);
    return -1;
  }

  ret = _GD_Delete(D, E, index, flags);

  dreturn("%i", ret);
  return ret;
}


int gd_delete_alias(DIRFILE *D, const char *field_code, unsigned int flags)
  gd_nothrow
{
  unsigned index;
  int ret;
  gd_entry_t *E;

  dtrace("%p, \"%s\", 0x%X", D, field_code, flags);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ClearError(D);

  E = _GD_FindField(D, field_code, D->entry, D->n_entries, 0, &index);

  if (!E) {
    _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_MISSING, NULL, 0, field_code);
    dreturn("%i", -1);
    return -1;
  }

  if (E->field_type != GD_ALIAS_ENTRY) {
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
    dreturn("%i", -1);
    return -1;
  }

  ret = _GD_Delete(D, E, index, flags);

  dreturn("%i", ret);
  return ret;
}
