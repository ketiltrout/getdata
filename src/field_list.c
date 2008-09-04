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
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef STDC_HEADERS
#include <stdlib.h>
#endif

#include "internal.h"

const char** get_field_list(DIRFILE* D)
{
  dtrace("%p", D);

  unsigned int i;
  char** fl;

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  if (D->n_entries == 0) {
    dreturn("%p", NULL);
    return NULL;
  }

  fl = realloc((char**)D->field_list, sizeof(const char*) * D->n_entries);

  if (fl == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  for (i = 0; i < D->n_entries; ++i) {
    fl[i] = D->entry[i]->field;
  }

  D->field_list = (const char**)fl;

  dreturn("%p", D->field_list);
  return D->field_list;
}
