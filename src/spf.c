/* Copyright (C) 2002-2005 C. Barth Netterfield
 * Copyright (C) 2005-2010 D. V. Wiebe
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
gd_spf_t _GD_GetSPF(DIRFILE* D, gd_entry_t* E)
{
  gd_spf_t spf = 0;

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
      if (_GD_BadInput(D, E, 0))
        break;

      spf = _GD_GetSPF(D, E->e->entry[0]);
      break;
    case GD_INDEX_ENTRY:
      spf = 1;
      break;
    case GD_CONST_ENTRY:
    case GD_CARRAY_ENTRY:
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
gd_spf_t gd_spf(DIRFILE* D, const char *field_code_in) gd_nothrow
{
  gd_spf_t spf = 0;
  gd_entry_t* entry;
  char* field_code;
  int repr;

  dtrace("%p, \"%s\"", D, field_code_in);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%u", 0);
    return 0;
  }

  _GD_ClearError(D);

  /* the representation is unimportant: it doesn't change the SPF of the field,
   * yet we have to run the field code through here to potentially remove it
   */
  entry = _GD_FindFieldAndRepr(D, field_code_in, &field_code, &repr, NULL, 1);

  if (D->error) {
    dreturn("%u", 0);
    return 0;
  }

  if (entry->field_type & GD_SCALAR_ENTRY)
    _GD_SetError(D, GD_E_DIMENSION, GD_E_DIM_CALLER, NULL, 0, field_code);
  else 
    spf = _GD_GetSPF(D, entry);

  if (field_code != field_code_in)
    free(field_code);

  dreturn("%u", spf);
  return spf;
}
/* vim: ts=2 sw=2 et tw=80
*/
