/* (C) 2010 D. V. Wiebe
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
#undef gd_get_nsamples64

#ifdef STDC_HEADERS
#include <errno.h>
#include <stdlib.h>
#endif

static off64_t _GD_GetNSamples(DIRFILE *D, gd_entry_t* E)
{
  off64_t ns = 0, ns1;
  gd_spf_t spf1, spf2;
  int i;

  dtrace("%p, \"%s\"", D, field_code_in);

  if (++D->recurse_level >= GD_MAX_RECURSE_LEVEL) {
    _GD_SetError(D, GD_E_RECURSE_LEVEL, 0, NULL, 0, E->field);
    D->recurse_level--;
    dreturn("%i", 0);
    return 0;
  }

  switch (E->field_type) {
    case GD_BIT_ENTRY:
    case GD_LINTERP_ENTRY:
    case GD_SBIT_ENTRY:
    case GD_POLYNOM_ENTRY:
      if (_GD_BadInput(D, E, 0))
        break;

      ns = _GD_GetNSamples(D, E->e->entry[0]);
      break;
    case GD_MULTIPLY_ENTRY:
      if (_GD_BadInput(D, E, 0) || _GD_BadInput(D, E, 1))
        break;

      ns = _GD_GetNSamples(D, E->e->entry[0]);

      if (D->error)
        break;

      spf1 = _GD_GetSPF(D, E->e->entry[0]);
      
      if (D->error) {
        ns = 0;
        break;
      }

      ns1 = _GD_GetNSamples(D, E->e->entry[1]);

      if (D->error) {
        ns = 0;
        break;
      }

      spf2 = _GD_GetSPF(D, E->e->entry[1]);
      
      if (D->error) {
        ns = 0;
        break;
      }

      ns1 = ns1 * spf1 / spf2;
      if (ns1 < ns)
        ns = ns1;
      break;
    case GD_LINCOM_ENTRY:
      if (_GD_BadInput(D, E, 0))
        break;

      ns = _GD_GetNSamples(D, E->e->entry[0]);

      if (D->error) {
        ns = 0;
        break;
      }

      if (E->n_fields > 1) {
        spf1 = _GD_GetSPF(D, E->e->entry[0]);

        if (D->error) {
          ns = 0;
          break;
        }
      }

      for (i = 1; i < E->n_fields; ++i) {
        if (_GD_BadInput(D, E, i)) {
          ns = 0;
          break;
        }

        ns1 = _GD_GetNSamples(D, E->e->entry[i]);

        if (D->error) {
          ns = 0;
          break;
        }

        spf2 = _GD_GetSPF(D, E->e->entry[i]);

        if (D->error) {
          ns = 0;
          break;
        }

        ns1 = ns1 * spf1 / spf2;
        if (ns1 < ns)
          ns = ns1;
      }
      break;
    case GD_PHASE_ENTRY:
      if (_GD_BadInput(D, E, 0))
        break;

      ns = _GD_GetNSamples(D, E->e->entry[0]);
      if (!D->error)
        ns -= E->shift;
      break;
    case GD_RAW_ENTRY:
      if (!_GD_Supports(D, E, GD_EF_SIZE))
        break;

      if (_GD_SetEncodedName(D, E->e->file, E->e->filebase, 0))
        break;

      ns = (*_gd_ef[E->e->file[0].encoding].size)(E->e->file, E->data_type);

      if (ns < 0) {
        _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[0].name, errno, NULL);
        ns = 0;
        break;
      }

      ns += D->fragment[E->fragment_index].frame_offset * E->spf;
      break;
    case GD_CONST_ENTRY:
    case GD_STRING_ENTRY:
    case GD_NO_ENTRY:
    case GD_INDEX_ENTRY:
      _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, E->field);
      break;
  }

  D->recurse_level--;

  dreturn("%lli", (unsigned long long)ns);
  return ns;
}

off64_t gd_get_nsamples64(DIRFILE* D, const char *field_code_in)
{
  off64_t ns;
  gd_entry_t *entry;
  int repr;
  char* field_code;

  dtrace("%p, \"%s\"", D, field_code_in);

  _GD_ClearError(D);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%lli", 0LL);
    return 0;
  }

  entry = _GD_FindFieldAndRepr(D, field_code_in, &field_code, &repr, NULL, 1);

  if (D->error) {
    dreturn("%u", 0);
    return 0;
  }

  ns = _GD_GetNSamples(D, entry);

  if (field_code != field_code_in)
    free(field_code);

  dreturn("%lli", (unsigned long long)ns);
  return ns;
}

/* 32(ish)-bit wrapper for the 64-bit version, when needed */
off_t gd_get_nsamples(DIRFILE* D, const char *field_code)
{
  return (off_t)gd_get_nsamples64(D, field_code);
}
/* vim: ts=2 sw=2 et tw=80
*/

