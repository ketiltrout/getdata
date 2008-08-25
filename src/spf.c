/* (C) 2002-2005 C. Barth Netterfield
 * (C) 2005-2008 D. V. Wiebe
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
#endif

#include "getdata_internal.h"

/* _GD_GetSPF: Get samples per frame for field
*/
unsigned int _GD_GetSPF(const char *field_code, DIRFILE* D)
{
  gd_entry_t* entry;
  unsigned int spf = 0;

  if (D->recurse_level >= GD_MAX_RECURSE_LEVEL) {
    _GD_SetError(D, GD_E_RECURSE_LEVEL, 0, NULL, 0, field_code);
    return 0;
  }

  if ((strcmp(field_code, "FILEFRAM") == 0) ||
      (strcmp(field_code, "INDEX") == 0))
    return 1;

  /* Find the field */
  entry = _GD_FindField(D, field_code);

  if (entry == NULL) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
    return 0;
  }

  D->recurse_level++;
  switch(entry->field_type) {
    case GD_RAW_ENTRY:
      spf = entry->samples_per_frame;
      break;
    case GD_LINCOM_ENTRY:
    case GD_MULTIPLY_ENTRY:
    case GD_BIT_ENTRY:
    case GD_PHASE_ENTRY:
    case GD_LINTERP_ENTRY:
      spf = _GD_GetSPF(entry->in_fields[0], D);
      break;
    default:
      _GD_InternalError(D);
  }
  D->recurse_level--;
  return spf;
}

/* Get the number of samples for each frame for the given field
 */
unsigned int get_samples_per_frame(DIRFILE* D, const char *field_code)
{
  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    return 0;
  }

  _GD_ClearError(D);

  return _GD_GetSPF(field_code, D);
}
/* vim: ts=2 sw=2 et tw=80
*/
