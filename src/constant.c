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

int gd_get_carray_slice(DIRFILE *D, const char *field_code, unsigned long start,
    size_t n, gd_type_t return_type, void *data_out) gd_nothrow
{
  gd_entry_t *entry;
  int repr;

  dtrace("%p, \"%s\", %lu, %" PRIuSIZE ", 0x%x, %p", D, field_code, start, n,
      return_type, data_out);

  GD_RETURN_ERR_IF_INVALID(D);

  entry = _GD_FindFieldAndRepr(D, field_code, &repr, NULL, 1);

  if (D->error)
    GD_RETURN_ERROR(D);

  if (entry->field_type != GD_CARRAY_ENTRY &&
      entry->field_type != GD_CONST_ENTRY)
  {
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
  } else if (start + n > ((entry->field_type == GD_CONST_ENTRY) ? 1 :
      entry->EN(scalar,array_len)))
  {
    _GD_SetError(D, GD_E_BOUNDS, 0, NULL, 0, NULL);
  } else if (return_type != GD_NULL &&
      _GD_BadType(GD_DIRFILE_STANDARDS_VERSION, return_type))
  {
    _GD_SetError(D, GD_E_BAD_TYPE, 0, NULL, return_type, NULL);
  } else if (!D->error)
    _GD_DoField(D, entry, repr, start, n, return_type, data_out);

  GD_RETURN_ERROR(D);
}

int gd_get_carray(DIRFILE *D, const char *field_code, gd_type_t return_type,
    void *data_out) gd_nothrow
{
  gd_entry_t *entry;
  int repr;

  dtrace("%p, \"%s\", 0x%x, %p", D, field_code, return_type, data_out);

  GD_RETURN_ERR_IF_INVALID(D);

  entry = _GD_FindFieldAndRepr(D, field_code, &repr, NULL, 1);

  if (D->error)
    GD_RETURN_ERROR(D);

  if (entry->field_type != GD_CARRAY_ENTRY &&
      entry->field_type != GD_CONST_ENTRY)
  {
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
  } else if (return_type != GD_NULL &&
      _GD_BadType(GD_DIRFILE_STANDARDS_VERSION, return_type))
  {
    _GD_SetError(D, GD_E_BAD_TYPE, 0, NULL, return_type, NULL);
  } else
    _GD_DoField(D, entry, repr, 0, (entry->field_type == GD_CONST_ENTRY) ? 1 :
        entry->EN(scalar,array_len), return_type, data_out);

  GD_RETURN_ERROR(D);
}

int gd_get_constant(DIRFILE* D, const char *field_code, gd_type_t return_type,
    void *data_out) gd_nothrow
{
  return gd_get_carray_slice(D, field_code, 0, 1, return_type, data_out);
}

size_t gd_array_len(DIRFILE *D, const char *field_code) gd_nothrow
{
  gd_entry_t *entry;
  size_t len = 0;

  dtrace("%p, \"%s\"", D, field_code);

  GD_RETURN_IF_INVALID(D, "%i", 0);

  entry = _GD_FindEntry(D, field_code);

  if (D->error) {
    dreturn("%i", 0);
    return 0;
  }

  if (entry->field_type == GD_CARRAY_ENTRY ||
      entry->field_type == GD_SARRAY_ENTRY)
  {
    len = entry->EN(scalar,array_len);
  } else if (entry->field_type == GD_CONST_ENTRY ||
      entry->field_type == GD_STRING_ENTRY)
  {
    len = 1;
  } else 
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);

  dreturn("%" PRIuSIZE, len);
  return len;
}

/* Deprecated alias */
size_t gd_carray_len(DIRFILE *D, const char *field_code) gd_nothrow
{
  return gd_array_len(D, field_code);
}

static void _GD_PutCarraySlice(DIRFILE* D, gd_entry_t *E, unsigned long first,
    size_t n, gd_type_t data_type, const void *data_in) gd_nothrow
{
  int i;

  dtrace("%p, %p, %lu, %" PRIuSIZE ", 0x%X, %p", D, E, first, n, data_type,
      data_in);

  if ((D->flags & GD_ACCMODE) != GD_RDWR)
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
  else if (first + n > ((E->field_type == GD_CONST_ENTRY) ? 1 :
        E->EN(scalar,array_len)))
  {
    _GD_SetError(D, GD_E_BOUNDS, 0, NULL, 0, NULL);
  } else if (_GD_BadType(GD_DIRFILE_STANDARDS_VERSION, data_type))
    _GD_SetError(D, GD_E_BAD_TYPE, 0, NULL, data_type, NULL);
  else
    _GD_DoFieldOut(D, E, first, n, data_type, data_in);

  if (D->error) {
    dreturnvoid();
    return;
  }

  /* Flag all clients as needing recalculation */
  for (i = 0; i < E->e->u.scalar.n_client; ++i)
    E->e->u.scalar.client[i]->flags &= ~GD_EN_CALC;

  /* Clear the client list */
  free(E->e->u.scalar.client);
  E->e->u.scalar.client = NULL;
  E->e->u.scalar.n_client = 0;

  dreturnvoid();
}

int gd_put_carray_slice(DIRFILE* D, const char *field_code, unsigned long first,
    size_t n, gd_type_t data_type, const void *data_in) gd_nothrow
{
  gd_entry_t *entry;

  dtrace("%p, \"%s\", %lu, %" PRIuSIZE ", 0x%X, %p", D, field_code, first,
      n, data_type, data_in);

  GD_RETURN_ERR_IF_INVALID(D);

  entry = _GD_FindEntry(D, field_code);

  if (entry == NULL)
    ; /* error already set */
  else if (entry->field_type != GD_CARRAY_ENTRY &&
      entry->field_type != GD_CONST_ENTRY)
  {
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
  } else
    _GD_PutCarraySlice(D, entry, first, n, data_type, data_in);

  GD_RETURN_ERROR(D);
}

int gd_put_carray(DIRFILE* D, const char *field_code, gd_type_t data_type,
    const void *data_in) gd_nothrow
{
  gd_entry_t *entry;

  dtrace("%p, \"%s\", 0x%x, %p", D, field_code, data_type, data_in);

  GD_RETURN_ERR_IF_INVALID(D);

  entry = _GD_FindEntry(D, field_code);

  if (entry == NULL)
    ; /* error already set */
  else if (entry->field_type != GD_CARRAY_ENTRY &&
      entry->field_type != GD_CONST_ENTRY)
  {
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
  } else
    _GD_PutCarraySlice(D, entry, 0,
        (entry->field_type == GD_CONST_ENTRY) ? 1 : entry->EN(scalar,array_len),
        data_type, data_in);

  GD_RETURN_ERROR(D);
}

int gd_put_constant(DIRFILE* D, const char *field_code, gd_type_t data_type,
    const void *data_in) gd_nothrow
{
  return gd_put_carray_slice(D, field_code, 0, 1, data_type, data_in);
}

/* vim: ts=2 sw=2 et tw=80
*/
