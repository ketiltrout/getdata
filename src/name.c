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

/* Munge a field code or field name using the prefix and suffix of the current
 * fragment.  Returns a newly malloc'd munged code, or NULL on error */
char *_GD_MungeCode(DIRFILE *D, const gd_entry_t *P, int me, const char *code,
    int *offset)
{
  char *ptr, *slash;
  size_t len = strlen(code);
  size_t slen;
  size_t plen;

  dtrace("%p, %p, %i, \"%s\", %p", D, P, me, code, offset);

  if (P) {
    plen = strlen(P->field);
    ptr = (char*)_GD_Malloc(D, len + plen + 2);
    if (ptr) {
      strcpy(ptr, P->field);
      ptr[plen] = '/';
      strcpy(ptr + plen + 1, code);
    }
    *offset = plen + 1;
  } else {
    *offset = 0;
    slen = (D->fragment[me].suffix) ? strlen(D->fragment[me].suffix) : 0;
    plen = (D->fragment[me].prefix) ? strlen(D->fragment[me].prefix) : 0;
    ptr = (char*)_GD_Malloc(D, len + slen + plen + 1);
    if (ptr) {
      /* look for a /, which could indicate this is a field code, not just a
       * field name.  If it is just an illegal name with a / in it, mungeing
       * will fail, but validation will catch the illegal name later anyways.
       */
      if ((slash = strchr(code, '/')))
        len = slash++ - code;

      if (plen > 0)
        strcpy(ptr, D->fragment[me].prefix);

      strncpy(ptr + plen, code, len + 1);

      if (slen > 0)
        strcpy(ptr + plen + len, D->fragment[me].suffix);

      if (slash) {
        ptr[plen + len + slen] = '/';
        strcpy(ptr + plen + len + slen + 1, slash);
      }
    }
  }

  dreturn("\"%s\" (%i)", ptr, *offset);
  return ptr;
}

/* Check for a valid field name -- returns 1 on error */
int _GD_ValidateField(const char* field_code, int standards, int strict,
    int affix, int* is_dot)
{
  const size_t len = strlen(field_code);
  size_t i, local_dot = 0;

  dtrace("\"%s\", %i, %i, %i, %p", field_code, standards, strict, affix,
      is_dot);

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
      if (affix || (standards >= 6 && strict)) {
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

    *is_dot = local_dot;
  }

  dreturn("%i", 0);
  return 0;
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
    name = _GD_DeMungeCode(D->fragment[E->fragment_index].prefix,
        D->fragment[E->fragment_index].suffix, new_name);
    if (name == NULL) {
      _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_INVALID, NULL, 0, new_name);
      dreturn("%i", -1);
      return -1;
    }
  }

  /* Duplicate check */
  Q = _GD_FindField(D, name, D->entry, D->n_entries, NULL);

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
