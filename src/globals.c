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

const char* dirfilename(DIRFILE* D)
{
  dtrace("%p", D);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  dreturn("\"%s\"", D->name);
  return D->name;
}

const char* dirfile_reference(DIRFILE* D, const char* field_code)
{
  dtrace("%p, \"%s\"", D, field_code);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }
  
  /* if no field specified, return only the field name */
  if (field_code == NULL) {
    if (D->reference_field == NULL) {
      dreturn("%p", NULL);
      return NULL;
    }

    dreturn("\"%s\"", D->reference_field->field);
    return D->reference_field->field;
  }

  /* check access mode */
  if ((D->flags & GD_ACCMODE) == GD_RDONLY) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%i", NULL);
    return NULL;
  }

  /* Check field */
  gd_entry_t *E = _GD_FindField(D, field_code, NULL);

  if (E == NULL) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
    dreturn("%i", NULL);
    return NULL;
  }

  if (E->field_type != GD_RAW_ENTRY) {
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
    dreturn("%i", NULL);
    return NULL;
  }

  /* Check protection */
  if (D->fragment[0].protection & GD_PROTECT_FORMAT) {
    _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[0].cname);
    dreturn("%i", NULL);
    return NULL;
  }

  /* set the new reference field */
  D->reference_field = E;
  D->fragment[0].ref_name = E->field;
  D->fragment[0].modified = 1;


  dreturn("\"%s\"", D->reference_field->field);
  return D->reference_field->field;
}
