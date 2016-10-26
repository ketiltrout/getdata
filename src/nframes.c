/* Copyright (C) 2002-2005 C. Barth Netterfield
 * Copyright (C) 2005-2009, 2011-2016 D. V. Wiebe
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
#undef gd_nframes64

off64_t gd_nframes64(DIRFILE* D)
{
  off64_t nf;

  dtrace("%p", D);

  GD_RETURN_ERR_IF_INVALID(D);

  if (D->reference_field == NULL) {
    dreturn("%i", 0);
    return 0;
  }

  if (!_GD_Supports(D, D->reference_field, GD_EF_NAME | GD_EF_SIZE))
    GD_RETURN_ERROR(D);

  if ((*_GD_ef[D->reference_field->e->u.raw.file[0].subenc].name)(D,
        (const char*)D->fragment[D->reference_field->fragment_index].enc_data,
        D->reference_field->e->u.raw.file,
        D->reference_field->e->u.raw.filebase, 0, 0))
  {
    GD_RETURN_ERROR(D);
  }

  /* If the reference field is open for writing, close it first to flush the
   * data
   */
  if (D->reference_field->e->u.raw.file[0].mode & GD_FILE_WRITE) {
    _GD_FiniRawIO(D, D->reference_field, D->reference_field->fragment_index,
        GD_FINIRAW_KEEP);
    if (D->error)
      GD_RETURN_ERROR(D);
  }

  nf = (*_GD_ef[D->reference_field->e->u.raw.file[0].subenc].size)(
      D->fragment[D->reference_field->fragment_index].dirfd,
      D->reference_field->e->u.raw.file, D->reference_field->EN(raw,data_type),
      _GD_FileSwapBytes(D, D->reference_field));

  if (nf < 0) {
    _GD_SetEncIOError(D, GD_E_IO_READ, D->reference_field->e->u.raw.file);
    GD_RETURN_ERROR(D);
  }

  nf /= D->reference_field->EN(raw,spf);
  nf += D->fragment[D->reference_field->fragment_index].frame_offset;

  dreturn("%" PRId64, (int64_t)nf);
  return nf;
}

/* 32(ish)-bit wrapper for the 64-bit version, when needed */
off_t gd_nframes(DIRFILE* D)
{
  return (off_t)gd_nframes64(D);
}
/* vim: ts=2 sw=2 et tw=80
*/
