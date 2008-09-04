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
 * You should have received a copy of the GNU General Public
 * License along with GetData; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 * 02111-1307 USA.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef STDC_HEADERS
#include <string.h>
#include <stdlib.h>
#endif

#include "internal.h"

gd_entry_t* dirfile_free_entry_strings(gd_entry_t* entry)
{
  int i;

  dtrace("%p", entry);

  if (!entry || entry->field_type == GD_NO_ENTRY) {
    dreturn("%p", entry);
    return entry;
  }

  free(entry->field);

  switch(entry->field_type) {
    case GD_LINCOM_ENTRY:
      for (i = 0; i < entry->n_fields; ++i)
        free(entry->in_fields[i]);
      break;
    case GD_LINTERP_ENTRY:
      free(entry->in_fields[0]);
      free(entry->table);
      break;
    case GD_MULTIPLY_ENTRY:
      free(entry->in_fields[1]);
      /* fall through */
    case GD_BIT_ENTRY:
    case GD_PHASE_ENTRY:
      free(entry->in_fields[0]);
      /* fall through */
    case GD_RAW_ENTRY:
    case GD_NO_ENTRY:
      break;
  }

  dreturn("%p", entry);
  return entry;
}

int get_entry(DIRFILE* D, const char* field_code, gd_entry_t* entry)
{
  int i;
  gd_entry_t *E;

  dtrace("%p, \"%s\", %p", D, field_code, entry);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", 1);
    return -1;
  }

  _GD_ClearError(D);

  E = _GD_FindField(D, field_code);

  if (E == NULL) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
    dreturn("%i", 1);
    return -1;
  }

  /* now copy to the user supplied buffer */
  memcpy(entry, E, sizeof(gd_entry_t));

  /* duplicate strings */
  entry->field = strdup(E->field);

  switch(E->field_type) {
    case GD_LINCOM_ENTRY:
      for (i = 0; i < E->n_fields; ++i)
        entry->in_fields[i] = strdup(E->in_fields[i]);
      break;
    case GD_LINTERP_ENTRY:
      entry->in_fields[0] = strdup(E->in_fields[0]);
      entry->table = strdup(E->table);
      break;
    case GD_MULTIPLY_ENTRY:
      entry->in_fields[1] = strdup(E->in_fields[1]);
      /* fall through */
    case GD_BIT_ENTRY:
    case GD_PHASE_ENTRY:
      entry->in_fields[0] = strdup(E->in_fields[0]);
      /* fall through */
    case GD_RAW_ENTRY:
    case GD_NO_ENTRY:
      break;
  }

  dreturn("%i", 0);
  return 0;
}
