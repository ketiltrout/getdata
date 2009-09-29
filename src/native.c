/* (C) 2009 D. V. Wiebe
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
#include <stdlib.h>

gd_type_t _GD_NativeType(DIRFILE* D, gd_entry_t* E, int repr)
{
  gd_type_t type = GD_UNKNOWN;
  int i;

  dtrace("%p, %p, %c", D, E, repr);

  if (++D->recurse_level >= GD_MAX_RECURSE_LEVEL) {
    _GD_SetError(D, GD_E_RECURSE_LEVEL, 0, NULL, 0, E->field);
    dreturn("%u", 0);
    D->recurse_level--;
    return 0;
  }

  if (!E->e->calculated)
    _GD_CalculateEntry(D, E);

  switch(E->field_type) {
    case GD_RAW_ENTRY:
      type = E->data_type;
      break;
    case GD_LINCOM_ENTRY:
      if (E->comp_scal) {
        type = GD_COMPLEX128;
        break;
      }

      for (i = 0; i < E->n_fields; ++i) {
        if (_GD_BadInput(D, E, i))
          break;

        if (_GD_NativeType(D, E->e->entry[i], E->e->repr[i]) & GD_COMPLEX) {
          type = GD_COMPLEX128;
          break;
        }
      }
      type = GD_FLOAT64;
      break;
    case GD_LINTERP_ENTRY:
      type = GD_FLOAT64;
      break;
    case GD_MULTIPLY_ENTRY:
      type = GD_FLOAT64;
      break;
    case GD_BIT_ENTRY:
    case GD_INDEX_ENTRY:
      type = GD_UINT64;
      break;
    case GD_PHASE_ENTRY:
      if (_GD_BadInput(D, E, 0))
        break;

      type = _GD_NativeType(D, E->e->entry[0], E->e->repr[0]);
      break;
    case GD_POLYNOM_ENTRY:
      if (E->comp_scal) {
        type = GD_COMPLEX128;
        break;
      }

      if (_GD_BadInput(D, E, 0))
        break;

      type = (_GD_NativeType(D, E->e->entry[0], E->e->repr[0]) & GD_COMPLEX) ?
        GD_COMPLEX128 : GD_FLOAT64;

      break;
    case GD_SBIT_ENTRY:
      type = GD_INT64;
      break;
    case GD_CONST_ENTRY:
      type = E->const_type;
      break;
    case GD_STRING_ENTRY:
      type = GD_NULL;
      break;
    case GD_NO_ENTRY:
      _GD_InternalError(D);
  }

  /* representation */
  if (repr != GD_REPR_NONE) {
    if (type == GD_COMPLEX128)
      type = GD_FLOAT64;
    else if (type == GD_COMPLEX64)
      type = GD_FLOAT32;
  }

  /* catch errors */
  if (D->error)
    type = GD_UNKNOWN;

  D->recurse_level--;

  dreturn("0x%02x", type);
  return type;
}

gd_type_t get_native_type(DIRFILE* D, const char* field_code_in)
{
  gd_type_t type = GD_UNKNOWN;
  gd_entry_t* entry;
  int repr;
  char* field_code;

  dtrace("%p, \"%s\"", D, field_code_in);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("0x%x", GD_UNKNOWN);
    return GD_UNKNOWN;
  }

  _GD_ClearError(D);

  repr = _GD_GetRepr(D, field_code_in, &field_code);

  if (D->error) {
    dreturn("0x%x", GD_UNKNOWN);
    return GD_UNKNOWN;
  }

  entry = _GD_FindField(D, field_code, NULL);

  if (entry == NULL)
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
  else 
    type = _GD_NativeType(D, entry, repr);

  if (field_code != field_code_in)
    free(field_code);

  dreturn("0x%x", type);
  return type;
}
/* vim: ts=2 sw=2 et tw=80
*/
