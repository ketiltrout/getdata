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

unsigned int gd_nfields(DIRFILE* D) gd_nothrow
{
  dtrace("%p", D);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%u", 0);
    return 0;
  }

  _GD_ClearError(D);

  dreturn("%u", D->n_entries - D->n_meta);
  return D->n_entries - D->n_meta;
}

unsigned int gd_nvectors(DIRFILE* D) gd_nothrow
{
  dtrace("%p", D);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%u", 0);
    return 0;
  }

  _GD_ClearError(D);

  dreturn("%u", D->n_entries - D->n_meta - D->n_string - D->n_const);
  return D->n_entries - D->n_meta - D->n_string - D->n_const;
}

unsigned int gd_nfields_by_type(DIRFILE* D, gd_entype_t type) gd_nothrow
{
  unsigned int i, r = 0;

  dtrace("%p, %i", D, type);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%u", 0);
    return 0;
  }

  _GD_ClearError(D);

  switch(type) {
    case GD_STRING_ENTRY:
      r = D->n_string;
      break;
    case GD_CONST_ENTRY:
      r = D->n_const;
      break;
    case GD_INDEX_ENTRY:
      r = 1;
      break;
    case GD_NO_ENTRY:
      _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_BAD_ENTRY_TYPE, NULL, type, NULL);
      break;
    default:
      for (i = 0; i < D->n_entries; ++i)
        if (D->entry[i]->field_type == type && D->entry[i]->e->n_meta != -1)
          r++;
      break;
  }

  dreturn("%u", r);
  return r;
}
