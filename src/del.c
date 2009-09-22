/* (C) 2008-2009 D. V. Wiebe
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
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#endif

static void _GD_ClearDerived(DIRFILE* D, gd_entry_t* E, const gd_entry_t* C,
    int check)
{
  int i;

  dtrace("%p, %p, %p, %i", D, E, C, check);

  switch(E->field_type) {
    case GD_LINCOM_ENTRY:
      for (i = 0; i < E->n_fields; ++i)
        if (strcmp(E->in_fields[i], C->field) == 0) {
          if (check)
            _GD_SetError(D, GD_E_DELETE, GD_E_DEL_DERIVED, E->field, 0,
                C->field);
          else
            E->e->entry[i] = NULL;
        }
      break;
    case GD_MULTIPLY_ENTRY:
      if (strcmp(E->in_fields[1], C->field) == 0) {
        if (check)
          _GD_SetError(D, GD_E_DELETE, GD_E_DEL_DERIVED, E->field, 0,
              C->field);
        else
          E->e->entry[1] = NULL;
      }
      /* Fallthrough */
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
    case GD_STRING_ENTRY:
      break;
  }

  dreturnvoid();
}

static void _GD_DeReference(DIRFILE* D, gd_entry_t* E, const gd_entry_t* C,
    int check)
{
  int i;

  dtrace("%p, %p, %p, %i", D, E, C, check);

  switch(E->field_type) {
    case GD_RAW_ENTRY:
      if (E->e->scalar[0] != NULL && strcmp(C->field, E->e->scalar[0]) == 0) {
        if (check) {
          _GD_SetError(D, GD_E_DELETE, GD_E_DEL_CONST, E->field, 0, C->field);
          break;
        } else {
          _GD_DoConst(D, C, GD_UINT32, &E->spf);
          free(E->e->scalar[0]);
          E->e->scalar[0] = NULL;
        }
      }
      break;
    case GD_POLYNOM_ENTRY:
      for (i = 0; i <= E->poly_ord; ++i) {
        if (E->e->scalar[i] != NULL && strcmp(C->field, E->e->scalar[i]) == 0) {
          if (check) {
            _GD_SetError(D, GD_E_DELETE, GD_E_DEL_CONST, E->field, 0, C->field);
            break;
          } else {
            if (E->comp_scal)
              _GD_DoConst(D, C, GD_COMPLEX128, &E->ca[i]);
            else
              _GD_DoConst(D, C, GD_FLOAT64, &E->a[i]);
            free(E->e->scalar[i]);
            E->e->scalar[i] = NULL;
          }
        }
      }
      break;
    case GD_LINCOM_ENTRY:
      for (i = 0; i < E->n_fields; ++i) {
        if (E->e->scalar[i * 2] != NULL &&
            strcmp(C->field, E->e->scalar[i * 2]) == 0)
        {
          if (check) {
            _GD_SetError(D, GD_E_DELETE, GD_E_DEL_CONST, E->field, 0, C->field);
            break;
          } else {
            if (E->comp_scal)
              _GD_DoConst(D, C, GD_COMPLEX128, &E->cb[i]);
            else 
              _GD_DoConst(D, C, GD_FLOAT64, &E->b[i]);
            free(E->e->scalar[i * 2]);
            E->e->scalar[i * 2] = NULL;
          }
        }
        if (E->e->scalar[i * 2 + 1] != NULL &&
            strcmp(C->field, E->e->scalar[i * 2 + 1]) == 0)
        {
          if (check) {
            _GD_SetError(D, GD_E_DELETE, GD_E_DEL_CONST, E->field, 0, C->field);
            break;
          } else {
            if (E->comp_scal)
              _GD_DoConst(D, C, GD_COMPLEX128, &E->cb[i]);
            else 
              _GD_DoConst(D, C, GD_FLOAT64, &E->b[i]);
            free(E->e->scalar[i * 2 + 1]);
            E->e->scalar[i * 2 + 1] = NULL;
          }
        }
      }
      break;
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
      if (E->e->scalar[0] != NULL && strcmp(C->field, E->e->scalar[0]) == 0) {
        if (check) {
          _GD_SetError(D, GD_E_DELETE, GD_E_DEL_CONST, E->field, 0, C->field);
          break;
        } else {
          _GD_DoConst(D, C, GD_UINT32, &E->bitnum);
          free(E->e->scalar[0]);
          E->e->scalar[0] = NULL;
        }
      }
      if (E->e->scalar[1] != NULL && strcmp(C->field, E->e->scalar[1]) == 0) {
        if (check) {
          _GD_SetError(D, GD_E_DELETE, GD_E_DEL_CONST, E->field, 0, C->field);
          break;
        } else {
          _GD_DoConst(D, C, GD_UINT32, &E->numbits);
          free(E->e->scalar[0]);
          E->e->scalar[1] = NULL;
        }
      }
      break;
    case GD_PHASE_ENTRY:
      if (E->e->scalar[0] != NULL && strcmp(C->field, E->e->scalar[0]) == 0) {
        if (check) {
          _GD_SetError(D, GD_E_DELETE, GD_E_DEL_CONST, E->field, 0, C->field);
          break;
        } else {
          _GD_DoConst(D, C, GD_UINT32, &E->shift);
          free(E->e->scalar[0]);
          E->e->scalar[0] = NULL;
        }
      }
      break;
    case GD_NO_ENTRY:
    case GD_LINTERP_ENTRY:
    case GD_MULTIPLY_ENTRY:
    case GD_STRING_ENTRY:
    case GD_CONST_ENTRY:
    case GD_INDEX_ENTRY:
      break;
  }

  dreturnvoid();
}

int dirfile_delete(DIRFILE* D, const char* field_code, int flags)
{
  unsigned int index;
  unsigned int first, last = 0;
  const int len = strlen(field_code);
  int n_del, i;
  unsigned int j;

  dtrace("%p, \"%s\", %x", D, field_code, flags);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if ((D->flags & GD_ACCMODE) != GD_RDWR) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%zi", -1);
    return -1;
  }

  _GD_ClearError(D);

  gd_entry_t *E = _GD_FindField(D, field_code, &index);

  if (E == NULL) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
    dreturn("%i", -1);
    return -1;
  }

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
    _GD_SetError(D, GD_E_DELETE, GD_E_DEL_META, NULL, 0, field_code);
    dreturn("%i", -1);
    return -1;
  } else if (E->e->n_meta > 0) {
    /* find one of the meta fields -- it's not true that metafields are
     * necessarily sorted directly after their parent */
    if (_GD_FindField(D, E->e->meta_entry[0]->field, &first) == NULL) {
      _GD_InternalError(D);
      dreturn("%i", -1);
      return -1;
    }
    last = first;

    /* The remaining meta fields will be contiguous with this one, so just
     * search linearly in both directions until we find something that isn't a
     * meta field of our parent */
    while (first > 0)
      if (D->entry[first - 1]->field[len] == '/' &&
          strncmp(D->entry[first - 1]->field, field_code, len) == 0)
        first--;
      else
        break;

    while (last < D->n_entries - 1)
      if (D->entry[last + 1]->field[len] == '/' &&
          strncmp(D->entry[last + 1]->field, field_code, len) == 0)
        last++;
      else
        break;
  }

  /* gather a list of fields */
  gd_entry_t **del_list = malloc(sizeof(gd_entry_t*) * (1 + E->e->n_meta));

  if (del_list == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%zi", -1);
    return -1;
  }

  del_list[0] = E;
  n_del = 1;

  for (i = 0; i < E->e->n_meta; ++i)
    del_list[n_del++] = E->e->meta_entry[i];

  /* Check for clients and derived fields */
  if (~flags & GD_DEL_FORCE)
    for (j = 0; j < D->n_entries; ++j)
      for (i = 0; i < n_del; ++i) {
        if (del_list[i]->field_type == GD_CONST_ENTRY && ~flags & GD_DEL_DEREF)
          _GD_DeReference(D, D->entry[j], del_list[i], 1);
        else if (~del_list[i]->field_type & GD_SCALAR_ENTRY)
          _GD_ClearDerived(D, D->entry[j], del_list[i], 1);

        if (D->error) {
          free(del_list);
          dreturn("%zi", -1);
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

    if (!_GD_Supports(D, E, GD_EF_UNLINK)) {
      free(del_list);
      dreturn("%zi", -1);
      return -1;
    }

    if (_GD_SetEncodedName(D, E->e->file, E->e->filebase, 0)) {
      free(del_list);
      dreturn("%zi", -1);
      return -1;
    }

    if ((*_gd_ef[E->e->file[0].encoding].unlink)(E->e->file)) {
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[0].name, errno, NULL);
      free(del_list);
      dreturn("%zi", -1);
      return -1;
    }
  } else if (E->field_type == GD_RAW_ENTRY && E->e->file->fp != -1) {
    if ((*_gd_ef[E->e->file[0].encoding].close)(E->e->file)) {
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[0].name, errno, NULL);
      free(del_list);
      dreturn("%zi", -1);
      return -1;
    }
  }

  /* Fix up reference fields */
  char** new_ref = NULL;
  gd_entry_t* reference = NULL;

  if (E->field_type == GD_RAW_ENTRY) {
    new_ref = malloc(sizeof(char*) * D->n_fragment);
    if (new_ref == NULL) {
      free(del_list);
      _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      dreturn("%zi", -1);
      return -1;
    }
    memset(new_ref, 0, sizeof(char*) * D->n_fragment);

    for (i = 0; i < D->n_fragment; ++i)
      if (D->fragment[i].ref_name != NULL &&
          strcmp(D->fragment[i].ref_name, E->field) == 0)
      {
        /* Flag for replacement */
        new_ref[i] = (char*)E;
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
              new_ref[i] = strdup(D->entry[j]->field);
              if (new_ref == NULL) {
                for (f = 0; f < i; ++f)
                  free(new_ref[f]);
                free(new_ref);
                free(del_list);
                _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
                dreturn("%zi", -1);
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
        D->fragment[i].ref_name = (new_ref[i] == (char*)E) ? NULL : new_ref[i];
      }
    free(new_ref);
  }

  /* Clear clients and derived fields */
  for (j = 0; j < D->n_entries; ++j)
    for (i = 0; i < n_del; ++i)
      if (del_list[i]->field_type == GD_CONST_ENTRY && flags & GD_DEL_DEREF)
        _GD_DeReference(D, D->entry[j], del_list[i], 0);
      else if (~del_list[i]->field_type & GD_SCALAR_ENTRY)
        _GD_ClearDerived(D, D->entry[j], del_list[i], 0);

  free(del_list);

  /* Remove meta fields, if present */
  if (E->e->n_meta > 0) {
    /* Remove all meta fields -- there are no RAW fields here */
    for (j = first; j <= last; ++j)
      _GD_FreeE(D->entry[j], 1);

    memmove(D->entry + first, D->entry + last + 1,
        sizeof(gd_entry_t*) * (D->n_entries - last - 1));
    D->n_meta -= last - first + 1;
    D->n_entries -= last - first + 1;
  }

  /* Remove the entry from the list -- we need not worry about the way we've
   * already modified D->entry, since E is guaranteed to be before the stuff
   * we've already removed */
  if (E->field_type == GD_CONST_ENTRY)
    D->n_const--;
  else if (E->field_type == GD_STRING_ENTRY)
    D->n_string--;

  _GD_FreeE(E, 1);

  memmove(D->entry + index, D->entry + index + 1,
      sizeof(gd_entry_t*) * (D->n_entries - index - 1));
  D->n_entries--;

  dreturn("%i", 0);
  return 0;
}
