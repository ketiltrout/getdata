/* (C) 2002-2005 C. Barth Netterfield
 * (C) 2005-2008 D. V. Wiebe
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

  /* Figure out encoding scheme, if necessary */
  if ((D->fragment[D->reference_field->fragment_index].flags & GD_ENCODING)
      == GD_AUTO_ENCODED) 
  {
      D->fragment[D->reference_field->fragment_index].flags =
      (D->fragment[D->reference_field->fragment_index].flags & ~GD_ENCODING) |
        _GD_ResolveEncoding(D->reference_field->e->file,
            D->fragment[D->reference_field->fragment_index].flags & GD_ENCODING,
            D->reference_field->e);
  } else if (D->reference_field->e->encoding == GD_ENC_UNKNOWN)
      _GD_ResolveEncoding(D->reference_field->e->file,
          D->fragment[D->reference_field->fragment_index].flags & GD_ENCODING,
          D->reference_field->e);

  if (encode[D->reference_field->e->encoding].size == NULL) {
      _GD_SetError(D, GD_E_UNSUPPORTED, 0, NULL, 0, NULL);
      dreturn("%lli", 0LL);
      return 0;
  }

  nf = (*encode[D->reference_field->e->encoding].size)(
      D->reference_field->e->file, D->reference_field->data_type
      );
  if (nf < 0) {
    _GD_SetError(D, GD_E_RAW_IO, 0, D->reference_field->e->file, errno, NULL);
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
