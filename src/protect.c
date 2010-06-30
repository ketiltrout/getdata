/* (C) 2008,2010 D. V. Wiebe
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

int gd_get_protection(DIRFILE* D, int fragment_index)
{
  dtrace("%p, %i", D, fragment_index);

  _GD_ClearError(D);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (fragment_index < 0 || fragment_index >= D->n_fragment) {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  dreturn("%i", D->fragment[fragment_index].protection);
  return D->fragment[fragment_index].protection;
}

int gd_protect(DIRFILE *D, int protection_level, int fragment_index)
{
  int i;

  dtrace("%p, %i, %i", D, protection_level, fragment_index);

  _GD_ClearError(D);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if ((D->flags & GD_ACCMODE) != GD_RDWR) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (fragment_index < GD_ALL_FRAGMENTS || fragment_index >= D->n_fragment) {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (protection_level != GD_PROTECT_NONE &&
      protection_level != GD_PROTECT_FORMAT &&
      protection_level != GD_PROTECT_DATA &&
      protection_level != GD_PROTECT_ALL)
  {
    _GD_SetError(D, GD_E_BAD_PROTECTION, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (fragment_index == GD_ALL_FRAGMENTS)
    for (i = 0; i < D->n_fragment; ++i)
      D->fragment[i].protection = protection_level;
  else
    D->fragment[fragment_index].protection = protection_level;

  dreturn("%i", 0);
  return 0;
}
