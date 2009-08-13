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

static int entry_cmp(const void *a, const void *b)
{
  return strcmp((*(gd_entry_t**)a)->field, (*(gd_entry_t**)b)->field);
}

/* Check for a valid field name -- returns input on error */
char* _GD_ValidateField(const gd_entry_t* parent, const char* field_code,
    int strict)
{
  size_t len = strlen(field_code);
  size_t i;
  char* ptr;

  dtrace("%p, \"%s\", %i", parent, field_code, strict);

  if (field_code[0] == '\0' || len >= GD_MAX_LINE_LENGTH) {
    dreturn("%p", field_code);
    return (char*)field_code;
  }

  for (i = 0; i < len; ++i)
    if (field_code[i] == '/' || field_code[i] == '<' || field_code[i] == '>' ||
        field_code[i] == ';' || field_code[i] == '|' || field_code[i] == '&' ||
        (strict && field_code[i] == '.'))
    {
      dreturn("%p", field_code);
      return (char*)field_code;
    }

  if (strcmp("FRAMEOFFSET", field_code) == 0 ||
      strcmp("ENCODING", field_code) == 0 ||
      strcmp("ENDIAN", field_code) == 0 ||
      strcmp("INCLUDE", field_code) == 0 ||
      strcmp("META", field_code) == 0 ||
      strcmp("VERSION", field_code) == 0)
  {
    dreturn("%p", field_code);
    return (char*)field_code;
  }

  if (!strict && len > 3 && ((len > 4 && field_code[len - 4] == '.') ||
        field_code[len - 3] == '.'))
    for (i = 0; i < GD_N_SUBENCODINGS; ++i)
      if (_gd_ef[i].ext[0] != '\0' && strcmp(field_code + len -
            strlen(_gd_ef[i].ext), _gd_ef[i].ext) == 0)
        {
          dreturn("%p", field_code);
          return (char*)field_code;
        }

  if (parent != NULL) {
    ptr = malloc(strlen(parent->field) + strlen(field_code) + 2);
    sprintf(ptr, "%s/%s", parent->field, field_code);
  } else
    ptr = strdup(field_code);

  dreturn("\"%s\"", ptr);
  return ptr;
}

int dirfile_rename(DIRFILE *D, const char *old_code, const char *new_name,
    int move_data)
{
  gd_entry_t *E, *Q;
  char* name;

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

  E = _GD_FindField(D, old_code, NULL);

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

  name = _GD_ValidateField(E->e->parent, new_name, 1);
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
  Q = _GD_FindField(D, name, NULL);

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
    char* filebase = malloc(FILENAME_MAX);

    if (filebase == NULL) {
      _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      free(name);
      dreturn("%i", -1);
      return -1;
    }

    if (D->fragment[E->fragment_index].sname)
      snprintf(filebase, FILENAME_MAX, "%s/%s/%s", D->name,
          D->fragment[E->fragment_index].sname, new_name);
    else
      snprintf(filebase, FILENAME_MAX, "%s/%s", D->name, new_name);

    /* Close the old file */
    if (E->e->file->fp != -1 && (*_gd_ef[E->e->file[0].encoding].close)(
          E->e->file))
    {
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[0].name, errno, NULL);
      free(name);
      free(filebase);
      dreturn("%i", -1);
      return -1;
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

      memcpy(&temp, E->e->file, sizeof(struct _gd_raw_file));
      temp.name = NULL;
      if (_GD_SetEncodedName(D, &temp, filebase, 0)) {
        free(name);
        free(filebase);
        dreturn("%i", -1);
        return -1;
      }

      if (_GD_SetEncodedName(D, E->e->file, E->e->filebase, 0)) {
        free(name);
        free(filebase);
        dreturn("%i", -1);
        return -1;
      }

      if ((*_gd_ef[E->e->file[0].encoding].move)(E->e->file, temp.name)) {
        _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[0].name, errno, NULL);
        free(filebase);
        dreturn("%i", -1);
        return -1;
      }

      /* Nothing may fail from now on */

    } else {
      free(E->e->file[0].name);
      E->e->file[0].name = NULL;
    }

    free(E->e->filebase);
    E->e->filebase = filebase;
  }

  free(E->field);
  E->field = name;

  /* re-sort the list */
  qsort(D->entry, D->n_entries, sizeof(gd_entry_t*), entry_cmp);

  dreturn("%i", 0);
  return 0;
}
