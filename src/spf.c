/* Copyright (C) 2002-2005 C. Barth Netterfield
 * Copyright (C) 2005-2016 D. V. Wiebe
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

/* _GD_GetSPF: Get samples per frame for field
*/
unsigned int _GD_GetSPF(DIRFILE *D, gd_entry_t *E)
{
  unsigned int spf = 0;

  dtrace("%p, %p", D, E);

  if (++D->recurse_level >= GD_MAX_RECURSE_LEVEL) {
    _GD_SetError(D, GD_E_RECURSE_LEVEL, GD_E_RECURSE_CODE, NULL, 0, E->field);
    D->recurse_level--;
    dreturn("%u", 0);
    return 0;
  }

  if (_GD_FindInputs(D, E, 1)) {
    D->recurse_level--;
    dreturn("%u", 0);
    return 0;
  }

  switch(E->field_type) {
    case GD_RAW_ENTRY:
      if (!(E->flags & GD_EN_CALC))
        _GD_CalculateEntry(D, E, 1);
      if (!D->error)
        spf = E->EN(raw,spf);
      break;
    case GD_LINCOM_ENTRY:
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
    case GD_RECIP_ENTRY:
    case GD_BIT_ENTRY:
    case GD_PHASE_ENTRY:
    case GD_LINTERP_ENTRY:
    case GD_POLYNOM_ENTRY:
    case GD_SBIT_ENTRY:
    case GD_WINDOW_ENTRY:
    case GD_MPLEX_ENTRY:
    case GD_INDIR_ENTRY:
    case GD_SINDIR_ENTRY:
      spf = _GD_GetSPF(D, E->e->entry[0]);
      break;
    case GD_INDEX_ENTRY:
      spf = 1;
      break;
    case GD_CONST_ENTRY:
    case GD_CARRAY_ENTRY:
    case GD_STRING_ENTRY:
    case GD_SARRAY_ENTRY:
    case GD_ALIAS_ENTRY:
    case GD_NO_ENTRY:
      _GD_InternalError(D);
  }

  D->recurse_level--;
  dreturn("%u", spf);
  return spf;
}

/* Get the number of samples for each frame for the given field
*/
unsigned int gd_spf(DIRFILE* D, const char *field_code) gd_nothrow
{
  unsigned int spf = 0;
  gd_entry_t* entry;

  dtrace("%p, \"%s\"", D, field_code);

  GD_RETURN_IF_INVALID(D, "%u", 0);

  entry = _GD_FindEntry(D, field_code);

  if (D->error) {
    dreturn("%u", 0);
    return 0;
  }

  if (entry->field_type & GD_SCALAR_ENTRY_BIT)
    _GD_SetError(D, GD_E_DIMENSION, GD_E_DIM_CALLER, NULL, 0, field_code);
  else 
    spf = _GD_GetSPF(D, entry);

  dreturn("%u", spf);
  return spf;
}
/* vim: ts=2 sw=2 et tw=80
*/
