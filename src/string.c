/* (C) 2008-2010 D. V. Wiebe
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

/* this function is little more than a public boilerplate for _GD_DoField */
size_t get_string(DIRFILE* D, const char *field_code, size_t len,
    char *data_out)
{
  size_t n_read = 0;
  gd_entry_t *entry;

  dtrace("%p, \"%s\", %zi, %p", D, field_code, len, data_out);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%zi", 0);
    return 0;
  }

  _GD_ClearError(D);

  entry = _GD_FindField(D, field_code, D->entry, D->n_entries, NULL);
    
  if (entry == NULL)
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
  else if (entry && entry->field_type != GD_STRING_ENTRY)
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
  else
    n_read = _GD_DoField(D, entry, 0, 0, len, GD_NULL, data_out);

  dreturn("%zi", n_read);
  return n_read;
}

/* this function is little more than a public boilerplate for _GD_DoFieldOut */
size_t put_string(DIRFILE* D, const char *field_code, const char *data_in)
{
  size_t n_wrote = 0;
  gd_entry_t *entry;

  dtrace("%p, \"%s\", \"%s\"", D, field_code, data_in);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%zi", 0);
    return 0;
  }

  if ((D->flags & GD_ACCMODE) != GD_RDWR) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%zi", 0);
    return 0;
  }

  _GD_ClearError(D);

  entry = _GD_FindField(D, field_code, D->entry, D->n_entries, NULL);

  if (entry == NULL)
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
  else if (entry->field_type != GD_STRING_ENTRY)
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
  else 
    n_wrote = _GD_DoFieldOut(D, entry, 0, 0, 0, GD_NULL, data_in);

  dreturn("%zi", n_wrote);
  return n_wrote;
}
/* vim: ts=2 sw=2 et tw=80
*/
