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

#ifdef STDC_HEADERS
#include <stdlib.h>
#endif

const char* get_fragmentname(DIRFILE* D, int index)
{
  dtrace("%p, %i", D, index);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

 if (index < 0 || index >= D->n_include) {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
 }

  dreturn("\"%s\"", D->include_list[index].cname);
  return D->include_list[index].cname;
}

int get_nfragments(DIRFILE* D)
{
  dtrace("%p", D);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  dreturn("%i", D->n_include);
  return D->n_include;
}
