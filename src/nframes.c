/* (C) 2002-2005 C. Barth Netterfield
 * (C) 2005-2009 D. V. Wiebe
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
#include <errno.h>
#endif

off64_t get_nframes64(DIRFILE* D)
{
  off64_t nf;

  dtrace("%p", D);

  _GD_ClearError(D);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%lli", 0LL);
    return 0;
  }

  if (D->reference_field == NULL) {
    dreturn("%lli", 0LL);
    return 0;
  }

  if (!_GD_Supports(D, D->reference_field, GD_EF_SIZE)) {
    dreturn("%i", 0);
    return 0;
  }

  if (_GD_SetEncodedName(D, D->reference_field->e->file,
        D->reference_field->e->filebase, 0))
  {
    dreturn("%i", 0);
    return 0;
  }

  nf = (*_gd_ef[D->reference_field->e->file[0].encoding].size)
    (D->reference_field->e->file, D->reference_field->data_type);

  if (nf < 0) {
    _GD_SetError(D, GD_E_RAW_IO, 0, D->reference_field->e->file[0].name, errno,
        NULL);
    dreturn("%lli", 0LL);
    return 0;
  }

  nf /= D->reference_field->spf;
  nf += D->fragment[D->reference_field->fragment_index].frame_offset;

  dreturn("%lli", (unsigned long long)nf);
  return nf;
}

/* 32(ish)-bit wrapper for the 64-bit version, when needed */
off_t get_nframes(DIRFILE* D)
{
  return (off_t)get_nframes64(D);
}
/* vim: ts=2 sw=2 et tw=80
*/
