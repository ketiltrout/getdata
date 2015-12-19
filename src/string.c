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

static int gd_get_sarray_slice(DIRFILE *D, const char *field_code,
    unsigned long start, size_t n, const char **data_out)
gd_nothrow
{
  gd_entry_t *E;

  dtrace("%p, \"%s\", %lu, %" PRIuSIZE ", %p", D, field_code, start, n,
      data_out);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ClearError(D);

  E = _GD_FindField(D, field_code, D->entry, D->n_entries, 1, NULL);

  if (E == NULL) {
    _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_MISSING, NULL, 0, field_code);
    dreturn("%i", -1);
    return -1;
  }

  if (E->field_type != GD_STRING_ENTRY) {
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
    dreturn("%i", -1);
    return -1;
  } else if (start + n > ((E->field_type == GD_STRING_ENTRY) ? 1 :
        E->EN(scalar,array_len)))
  {
    _GD_SetError(D, GD_E_BOUNDS, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* nothing to do */
  if (n == 0) {
    dreturn("%i", 0);
    return 0;
  }

  if (E->field_type == GD_STRING_ENTRY)
    data_out[0] = E->e->u.string;
  else
    memcpy(data_out, ((const char **)E->e->u.scalar.d) + start,
        n * sizeof(const char*));

  dreturn("%i", 0);
  return 0;
}

size_t gd_get_string(DIRFILE *D, const char *field_code, size_t len,
    char *data_out) gd_nothrow
{
  size_t n_read;
  const char *ptr;

  dtrace("%p, \"%s\", %" PRIuSIZE ", %p", D, field_code, len, data_out);

  /* get string */
  if (gd_get_sarray_slice(D, field_code, 0, 1, &ptr)) {
    dreturn("%i", 0);
    return 0;
  }

  /* copy into user buffer */
  if (len > 0 && data_out != NULL)
    strncpy(data_out, ptr, len);

  n_read = strlen(ptr) + 1;

  dreturn("%" PRIuSIZE, n_read);
  return n_read;
}

static size_t _GD_PutSarraySlice(DIRFILE *restrict D, gd_entry_t *restrict E,
    unsigned long first, size_t n, const char **data_in)
{
  size_t len;

  dtrace("%p, %p, %lu, %" PRIuSIZE ", %p", D, E, first, n, data_in);

  if ((D->flags & GD_ACCMODE) != GD_RDWR) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  if (D->fragment[E->fragment_index].protection & GD_PROTECT_FORMAT) {
    _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[E->fragment_index].cname);
    dreturn("%i", 0);
    return 0;
  }

  if (first + n > ((E->field_type == GD_STRING_ENTRY) ? 1 :
        E->EN(scalar,array_len)))
  {
    _GD_SetError(D, GD_E_BOUNDS, 0, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  if (n == 0) {
    /* succesfully did nothing. */
    dreturn("%i", 1);
    return 1;
  }

  if (E->field_type == GD_STRING_ENTRY) {
    char *ptr = E->e->u.string;
    E->e->u.string = _GD_Strdup(D, *data_in);
    if (E->e->u.string == NULL) {
      E->e->u.string = ptr;
      dreturn("%i", 0);
      return 0;
    }
    free(ptr);
  } else {
    /* copy */
    size_t i;
    char **new_data = _GD_Malloc(D, n * sizeof(char*));
    if (new_data == NULL) {
      dreturn("%i", 0);
      return 0;
    }

    memset(new_data, 0, n * sizeof(char*));
    for (i = 0; i < n; ++i)
      new_data[i] = _GD_Strdup(D, data_in[i]);

    if (D->error) {
      for (i = 0; i < n; ++i)
        free(new_data[i]);
      free(new_data);
      dreturn("%i", 0);
      return 0;
    }

    /* replace elements */
    for (i = 0; i < n; ++i) {
      free(((char**)E->e->u.scalar.d)[i + first]);
      ((char**)E->e->u.scalar.d)[i + first] = new_data[i];
    }
    free(new_data);
  }

  D->fragment[E->fragment_index].modified = 1;

  len = strlen(data_in[0]) + 1;
  dreturn("%" PRIuSIZE, len);
  return len;
}

size_t gd_put_string(DIRFILE *D, const char *field_code, const char *data_in)
  gd_nothrow
{
  size_t n_wrote = 0;
  gd_entry_t *E;

  dtrace("%p, \"%s\", \"%s\"", D, field_code, data_in);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  _GD_ClearError(D);

  E = _GD_FindField(D, field_code, D->entry, D->n_entries, 1, NULL);

  if (E == NULL)
    _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_MISSING, NULL, 0, field_code);
  else if (E->field_type != GD_STRING_ENTRY) {
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
  } else 
    n_wrote = _GD_PutSarraySlice(D, E, 0, 1, &data_in);

  dreturn("%" PRIuSIZE, n_wrote);
  return n_wrote;
}
/* vim: ts=2 sw=2 et tw=80
*/
