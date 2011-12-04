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

/* Check for a valid field name -- returns input on error */
char* _GD_ValidateField(const gd_entry_t* parent, const char* field_code,
    int standards, int strict, int* is_dot)
{
  const size_t len = strlen(field_code);
  size_t i;
  char* ptr;

  dtrace("%p, \"%s\", %i, %i, %p", parent, field_code, standards, strict,
      is_dot);

  if (field_code[0] == '\0' || (strict && ((len > 50 && standards < 5) ||
          (len > 16 && standards < 3))))
  {
    dreturn("%p", field_code);
    return (char *)field_code;
  }

  *is_dot = 0;
  for (i = 0; i < len; ++i)
    if (field_code[i] == '/') {
      /* fields may never contain '/', regardless of version and strictness */
      dreturn("%p", field_code);
      return (char *)field_code;
    } else if (field_code[i] < 0x20) {
      dreturn("%p", field_code);
      return (char *)field_code;
    } else if (strict && ((standards >= 5 && (field_code[i] == '<' ||
            field_code[i] == '>' || field_code[i] == ';' ||
            field_code[i] == '|' || field_code[i] == '&')) ||
        (standards == 5 && (field_code[i] == '\\' || field_code[i] == '#'))))
    {
      dreturn("%p", field_code);
      return (char *)field_code;
    } else if (field_code[i] == '.') {
      if (standards >= 6 && strict) {
        dreturn("%p", field_code);
        return (char *)field_code;
      } else
        *is_dot = 1;
    }

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
      dreturn("%p", field_code);
      return (char *)field_code;
    }

  if (parent != NULL) {
    ptr = (char *)malloc(strlen(parent->field) + strlen(field_code) + 2);
    sprintf(ptr, "%s/%s", parent->field, field_code);
  } else
    ptr = strdup(field_code);

  dreturn("\"%s\"", ptr);
  return ptr;
}

int gd_rename(DIRFILE *D, const char *old_code, const char *new_name,
    int move_data)
{
  gd_entry_t *E, *Q;
  char* name;
  int new_dot, old_dot = 0;
  unsigned int dot_ind;

  dtrace("%p, \"%s\", \"%s\", %i", D, old_code, new_name, move_data);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* check access mode */
  if ((D->flags & GD_ACCMODE) == GD_RDONLY) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* check for a dotted field name */
  E = _GD_FindField(D, old_code, D->dot_list, D->n_dot, &dot_ind);

  if (E)
    old_dot = 1;
  else
    E = _GD_FindField(D, old_code, D->entry, D->n_entries, NULL);

  if (E == NULL) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, old_code);
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

  name = _GD_ValidateField(E->e->p.parent, new_name, D->standards, 1, &new_dot);
  if (name == new_name) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, new_name);
    dreturn("%i", -1);
    return -1;
  } else if (name == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* Duplicate check */
  Q = _GD_FindField(D, name, D->entry, D->n_entries, NULL);

  if (Q == E) {
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
    char *filebase = strdup(new_name);

    if (filebase == NULL) {
      _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
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
      gd_entry_t** ptr = (gd_entry_t **)realloc(D->dot_list,
          sizeof(gd_entry_t*) * (D->n_dot + 1));

      if (ptr == NULL) {
        _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
        free(name);
        dreturn("%i", -1);
        return -1;
      }

      D->dot_list = ptr;
    }

    if (move_data) {
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

  /* Update the dot list */
  if (old_dot && !new_dot)
    memmove(D->dot_list + dot_ind, D->dot_list + dot_ind + 1,
        sizeof(gd_entry_t*) * (--D->n_dot - dot_ind));
  else if (new_dot && !old_dot)
    D->dot_list[D->n_dot++] = E;

  /* re-sort the lists */
  qsort(D->entry, D->n_entries, sizeof(gd_entry_t*), entry_cmp);
  if (new_dot)
    qsort(D->dot_list, D->n_dot, sizeof(gd_entry_t*), entry_cmp);

  /* Invalidate the field lists */
  D->list_validity = 0;
  D->type_list_validity = 0;

  dreturn("%i", 0);
  return 0;
}
