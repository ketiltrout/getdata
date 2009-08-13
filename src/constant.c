/* (C) 2008 D. V. Wiebe
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
#endif

/* this function is little more than a public boilerplate for _GD_DoField */
int get_constant(DIRFILE* D, const char *field_code, gd_type_t return_type,
    void *data_out)
{
  gd_entry_t *entry;

  dtrace("%p, \"%s\", 0x%x, %p", D, field_code, return_type, data_out);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ClearError(D);

  entry = _GD_FindField(D, field_code, NULL);
    
  if (entry == NULL)
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
  else if (entry->field_type != GD_CONST_ENTRY)
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
  else
    _GD_DoField(D, entry, field_code, 0, 0, 0, 0, return_type, data_out);

  if (D->error) {
    dreturn("%i", -1);
    return -1;
  }

  dreturn("%i", 0);
  return 0;
}

/* this function is little more than a public boilerplate for _GD_DoFieldOut */
int put_constant(DIRFILE* D, const char *field_code, gd_type_t data_type,
    const void *data_in)
{
  int i;
  gd_entry_t *entry;

  dtrace("%p, \"%s\", 0x%x, %p", D, field_code, data_type, data_in);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if ((D->flags & GD_ACCMODE) != GD_RDWR) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ClearError(D);

  entry = _GD_FindField(D, field_code, NULL);

  if (entry == NULL)
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
  else if (entry->field_type != GD_CONST_ENTRY)
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
  else 
    _GD_DoFieldOut(D, entry, field_code, 0, 0, 0, 0, data_type, data_in);

  /* Flag all clients as needing recalculation */
  for (i = 0; i < entry->e->n_client; ++i)
    entry->e->client[i]->e->calculated = 0;

  /* Clear the client list */
  free(entry->e->client);
  entry->e->client = NULL;
  entry->e->n_client = 0;

  dreturn("%i", 0);
  return 0;
}

/* vim: ts=2 sw=2 et tw=80
*/
