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

/* this function is little more than a public boilerplate for _GD_DoField */
ssize_t get_constant(DIRFILE* D, const char *field_code, gd_type_t return_type,
    void *data_out)
{
  size_t n_read;
  gd_entry_t *entry;

  dtrace("%p, \"%s\", 0x%x, %p", D, field_code, return_type, data_out);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%zi", -1);
    return -1;
  }

  _GD_ClearError(D);

  entry = _GD_GetEntry(D, field_code);
    
  if (D->error != GD_E_OK)
    n_read = 0;
  else if (entry && entry->field_type != GD_CONST_ENTRY) {
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
    n_read = 0;
  } else
    n_read = _GD_DoField(D, entry, field_code, 0, 0, 0, 0, return_type,
        data_out);

  if (D->error) {
    dreturn("%i", -1);
    return -1;
  }

  dreturn("%zi", n_read);
  return (ssize_t)n_read;
}

/* this function is little more than a public boilerplate for _GD_DoFieldOut */
size_t put_constant(DIRFILE* D, const char *field_code, gd_type_t data_type,
    const void *data_in)
{
  size_t n_wrote = 0;
  gd_entry_t *entry;

  dtrace("%p, \"%s\", 0x%x, %p", D, field_code, data_type, data_in);

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

  entry = _GD_GetEntry(D, field_code);

  if (D->error != GD_E_OK)
    n_wrote = 0;
  else if (entry && entry->field_type != GD_CONST_ENTRY) {
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
    n_wrote = 0;
  } else 
    n_wrote = _GD_DoFieldOut(D, entry, field_code, 0, 0, 0, 0, data_type,
        data_in);

  dreturn("%zi", n_wrote);
  return n_wrote;
}

/* vim: ts=2 sw=2 et tw=80
*/
