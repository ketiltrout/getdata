/* Copyright (C) 2008, 2010, 2011, 2013, 2016 D. V. Wiebe
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

int gd_protection(DIRFILE* D, int fragment_index) gd_nothrow
{
  dtrace("%p, %i", D, fragment_index);

  GD_RETURN_ERR_IF_INVALID(D);

  if (fragment_index < 0 || fragment_index >= D->n_fragment)
    GD_SET_RETURN_ERROR(D, GD_E_BAD_INDEX, 0, NULL, fragment_index, NULL);

  dreturn("%i", D->fragment[fragment_index].protection);
  return D->fragment[fragment_index].protection;
}

int gd_alter_protection(DIRFILE *D, int protection_level, int fragment_index)
  gd_nothrow
{
  int i;

  dtrace("%p, %i, %i", D, protection_level, fragment_index);

  GD_RETURN_ERR_IF_INVALID(D);

  if ((D->flags & GD_ACCMODE) != GD_RDWR)
    GD_SET_RETURN_ERROR(D, GD_E_ACCMODE, 0, NULL, 0, NULL);

  if (fragment_index < GD_ALL_FRAGMENTS || fragment_index >= D->n_fragment)
    GD_SET_RETURN_ERROR(D, GD_E_BAD_INDEX, 0, NULL, fragment_index, NULL);

  if (protection_level != GD_PROTECT_NONE &&
      protection_level != GD_PROTECT_FORMAT &&
      protection_level != GD_PROTECT_DATA &&
      protection_level != GD_PROTECT_ALL)
  {
    GD_SET_RETURN_ERROR(D, GD_E_ARGUMENT, GD_E_ARG_PROTECTION, NULL, 0, NULL);
  }

  if (fragment_index == GD_ALL_FRAGMENTS)
    for (i = 0; i < D->n_fragment; ++i) {
      if (protection_level != D->fragment[i].protection) {
        D->fragment[i].protection = protection_level;
        D->fragment[i].modified = 1;
      }
    }
  else if (protection_level != D->fragment[fragment_index].protection) {
    D->fragment[fragment_index].protection = protection_level;
    D->fragment[fragment_index].modified = 1;
  }

  dreturn("%i", 0);
  return 0;
}
