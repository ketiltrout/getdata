/* Copyright (C) 2008, 2010-2016 D. V. Wiebe
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

int gd_get_sarray_slice(DIRFILE *D, const char *field_code, unsigned long start,
    size_t n, const char **data_out) gd_nothrow
{
  gd_entry_t *E;

  dtrace("%p, \"%s\", %lu, %" PRIuSIZE ", %p", D, field_code, start, n,
      data_out);

  GD_RETURN_ERR_IF_INVALID(D);

  E = _GD_FindEntry(D, field_code);

  if (E == NULL)
    ; /* Error already set */
  else if (E->field_type != GD_STRING_ENTRY && E->field_type != GD_SARRAY_ENTRY)
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
  else if (start + n > ((E->field_type == GD_STRING_ENTRY) ? 1 :
        E->EN(scalar,array_len)))
  {
    _GD_SetError(D, GD_E_BOUNDS, 0, NULL, 0, NULL);
  } else if (n == 0)
    ; /* nothing to do */
  else if (E->field_type == GD_STRING_ENTRY)
    data_out[0] = E->e->u.string;
  else
    memcpy(data_out, ((const char **)E->e->u.scalar.d) + start,
        n * sizeof(const char*));

  GD_RETURN_ERROR(D);
}

int gd_get_sarray(DIRFILE *D, const char *field_code, const char **data_out)
gd_nothrow
{
  gd_entry_t *E;

  dtrace("%p, \"%s\", %p", D, field_code, data_out);

  GD_RETURN_ERR_IF_INVALID(D);

  E = _GD_FindEntry(D, field_code);

  if (E == NULL)
    GD_RETURN_ERROR(D);

  if (E->field_type == GD_STRING_ENTRY)
    data_out[0] = E->e->u.string;
  else if (E->field_type == GD_SARRAY_ENTRY)
    memcpy(data_out, E->e->u.scalar.d,
        E->EN(scalar,array_len) * sizeof(const char*));
  else
    GD_SET_RETURN_ERROR(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0,
        field_code);

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

static void _GD_PutSarraySlice(DIRFILE *restrict D, gd_entry_t *restrict E,
    unsigned long first, size_t n, const char **data_in)
{
  dtrace("%p, %p, %lu, %" PRIuSIZE ", %p", D, E, first, n, data_in);

  if ((D->flags & GD_ACCMODE) != GD_RDWR) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturnvoid();
    return;
  }

  if (D->fragment[E->fragment_index].protection & GD_PROTECT_FORMAT) {
    _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[E->fragment_index].cname);
    dreturnvoid();
    return;
  }

  if (first + n > ((E->field_type == GD_STRING_ENTRY) ? 1 :
        E->EN(scalar,array_len)))
  {
    _GD_SetError(D, GD_E_BOUNDS, 0, NULL, 0, NULL);
    dreturnvoid();
    return;
  }

  if (n == 0) {
    /* succesfully did nothing. */
    dreturnvoid();
    return;
  }

  if (E->field_type == GD_STRING_ENTRY) {
    char *ptr = E->e->u.string;
    E->e->u.string = _GD_Strdup(D, *data_in);
    if (E->e->u.string == NULL) {
      E->e->u.string = ptr;
      dreturnvoid();
      return;
    }
    free(ptr);
  } else {
    /* copy */
    size_t i;
    char **new_data = _GD_Malloc(D, n * sizeof(char*));
    if (new_data == NULL) {
      dreturnvoid();
      return;
    }

    memset(new_data, 0, n * sizeof(char*));
    for (i = 0; i < n; ++i)
      new_data[i] = _GD_Strdup(D, data_in[i]);

    if (D->error) {
      for (i = 0; i < n; ++i)
        free(new_data[i]);
      free(new_data);
      dreturnvoid();
      return;
    }

    /* replace elements */
    for (i = 0; i < n; ++i) {
      free(((char**)E->e->u.scalar.d)[i + first]);
      ((char**)E->e->u.scalar.d)[i + first] = new_data[i];
    }
    free(new_data);
  }

  D->fragment[E->fragment_index].modified = 1;

  dreturnvoid();
  return;
}

int gd_put_sarray_slice(DIRFILE *D, const char *field_code, unsigned long first,
    size_t n, const char **data_in) gd_nothrow
{
  gd_entry_t *E;

  dtrace("%p, \"%s\", %lu, %" PRIuSIZE ", %p", D, field_code, first, n,
      data_in);

  GD_RETURN_ERR_IF_INVALID(D);

  E = _GD_FindEntry(D, field_code);

  if (E == NULL)
    ; /* Error already set */
  else if (E->field_type != GD_SARRAY_ENTRY && E->field_type != GD_STRING_ENTRY)
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
  else
    _GD_PutSarraySlice(D, E, first, n, data_in);

  GD_RETURN_ERROR(D);
}

int gd_put_sarray(DIRFILE *D, const char *field_code, const char **data_in)
  gd_nothrow
{
  gd_entry_t *E;

  dtrace("%p, \"%s\", %p", D, field_code, data_in);

  GD_RETURN_ERR_IF_INVALID(D);

  E = _GD_FindEntry(D, field_code);

  if (E == NULL) 
    ; /* Error already set */
  else if (E->field_type != GD_SARRAY_ENTRY && E->field_type != GD_STRING_ENTRY)
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
  else
   _GD_PutSarraySlice(D, E, 0, (E->field_type == GD_STRING_ENTRY) ? 1 :
       E->EN(scalar,array_len), data_in);

  GD_RETURN_ERROR(D);
}

int gd_put_string(DIRFILE *D, const char *field_code, const char *data_in)
  gd_nothrow
{
  gd_entry_t *E;

  dtrace("%p, \"%s\", \"%s\"", D, field_code, data_in);

  GD_RETURN_ERR_IF_INVALID(D);

  E = _GD_FindEntry(D, field_code);

  if (E == NULL)
    ; /* Error already set */
  else if (E->field_type != GD_STRING_ENTRY && E->field_type != GD_SARRAY_ENTRY)
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
  else 
    _GD_PutSarraySlice(D, E, 0, 1, &data_in);

  GD_RETURN_ERROR(D);
}
/* vim: ts=2 sw=2 et tw=80
*/
