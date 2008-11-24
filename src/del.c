/* (C) 2008 D. V. Wiebe
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
    case GD_LINCOM_ENTRY:
      for (i = 0; i < E->n_fields; ++i) {
        if (E->e->scalar[i * 2] != NULL &&
            strcmp(C->field, E->e->scalar[i * 2]) == 0)
        {
          if (check) {
            _GD_SetError(D, GD_E_DELETE, GD_E_DEL_CONST, E->field, 0, C->field);
            break;
          } else {
            _GD_DoConst(D, C, GD_FLOAT64, &E->m[i]);
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
            _GD_DoConst(D, C, GD_FLOAT64, &E->b[i]);
            free(E->e->scalar[i * 2 + 1]);
            E->e->scalar[i * 2 + 1] = NULL;
          }
        }
      }
      break;
    case GD_BIT_ENTRY:
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
  int index;
  int first, last;
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
  } else {
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
      if (E->e->meta_entry[first - 1]->field[len] == '/' &&
          strncmp(E->e->meta_entry[first - 1]->field, field_code, len) == 0)
        first--;
      else
        break;

    while (last < (int)D->n_entries - 2)
      if (E->e->meta_entry[last + 1]->field[len] == '/' &&
          strncmp(E->e->meta_entry[last + 1]->field, field_code, len) == 0)
        last++;
      else
        break;
  }

  /* gather a list of fields */
  gd_entry_t **del_list = malloc(sizeof(gd_entry_t*) * (1 + E->e->n_meta));

  if (del_list == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%zi", 1);
    return 1;
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
          dreturn("%zi", 1);
          return 1;
        }
      }

  /* If this is a raw field, and we were asked to delete the data, do so */
  if (E->field_type == GD_RAW_ENTRY && flags & GD_DEL_DATA) {
    /* check protection */
    if (D->fragment[E->fragment_index].protection & GD_PROTECT_DATA) {
      _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_DATA, NULL, 0,
          D->fragment[E->fragment_index].cname);
      dreturn("%i", -1);
      return -1;
    }

    if (!_GD_Supports(D, E, GD_EF_UNLINK)) {
      dreturn("%zi", -1);
      return -1;
    }

    if ((*D->ef[E->e->file[0].encoding].unlink)(E->e->file, E->e->filebase)) {
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[0].name, errno, NULL);
      dreturn("%zi", -1);
      return -1;
    }
  }

  /* Everything from now on must not fail */

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
    memmove(D->entry + first, D->entry + last + 1,
        sizeof(gd_entry_t*) * D->n_entries - last - 1);
    D->n_entries -= last - first + 1;
  }

  /* Remove the entry from the list -- we need not worry about the way we've
   * already modified D->entry, since E is guaranteed to be before the stuff
   * we've already removed */
  memmove(D->entry + index, D->entry + index + 1,
      sizeof(gd_entry_t*) * D->n_entries - index - 1);
  D->n_entries--;

  dreturn("%i", 0);
  return 0;
}
