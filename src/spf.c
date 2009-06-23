/* (C) 2002-2005 C. Barth Netterfield
 * (C) 2005-2009 D. V. Wiebe
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

#ifdef STDC_HEADERS
#include <string.h>
#endif

/* _GD_GetSPF: Get samples per frame for field
*/
unsigned int _GD_GetSPF(DIRFILE* D, gd_entry_t* E)
{
  unsigned int spf = 0;

  dtrace("%p, %p", D, E);

  if (++D->recurse_level >= GD_MAX_RECURSE_LEVEL) {
    _GD_SetError(D, GD_E_RECURSE_LEVEL, 0, NULL, 0, E->field);
    dreturn("%u", 0);
    D->recurse_level--;
    return 0;
  }

  switch(E->field_type) {
    case GD_RAW_ENTRY:
      if (!E->e->calculated)
        _GD_CalculateEntry(D, E);
      spf = E->spf;
      break;
    case GD_LINCOM_ENTRY:
    case GD_MULTIPLY_ENTRY:
    case GD_BIT_ENTRY:
    case GD_PHASE_ENTRY:
    case GD_LINTERP_ENTRY:
    case GD_POLYNOM_ENTRY:
    case GD_SBIT_ENTRY:
      if (E->e->entry[0] == NULL) {
        E->e->entry[0] = _GD_FindField(D, E->in_fields[0], NULL);

        if (E->e->entry[0] == NULL) {
          _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, E->in_fields[0]);
          break;
        }

        /* scalar entries not allowed */
        if (E->e->entry[0]->field_type & GD_SCALAR_ENTRY) {
          _GD_SetError(D, GD_E_DIMENSION, 0, E->field, 0,
              E->e->entry[0]->field);
          break;
        }
      }

      spf = _GD_GetSPF(D, E->e->entry[0]);
      break;
    case GD_INDEX_ENTRY:
      spf = 1;
      break;
    case GD_CONST_ENTRY:
    case GD_STRING_ENTRY:
    case GD_NO_ENTRY:
      _GD_InternalError(D);
  }

  D->recurse_level--;
  dreturn("%u", spf);
  return spf;
}

/* Get the number of samples for each frame for the given field
*/
unsigned int get_spf(DIRFILE* D, const char *field_code)
{
  unsigned int spf = 0;
  gd_entry_t* entry;

  dtrace("%p, \"%s\"", D, field_code);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%u", 0);
    return 0;
  }

  _GD_ClearError(D);

  entry = _GD_FindField(D, field_code, NULL);

  if (entry == NULL)
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
  else if (entry->field_type & GD_SCALAR_ENTRY)
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
  else 
    spf = _GD_GetSPF(D, entry);

  dreturn("%u", spf);
  return spf;
}
/* vim: ts=2 sw=2 et tw=80
*/
