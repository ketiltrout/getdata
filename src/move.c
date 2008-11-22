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
#include <string.h>
#include <errno.h>
#endif

int _GD_MogrifyFile(DIRFILE* D, gd_entry_t* E, int encoding, int byte_sex,
    off64_t offset, int finalise)
{
  const struct encoding_t* enc = encode + E->e->file[0].encoding;
  const size_t ns = BUFFER_SIZE / E->e->size;
  ssize_t nread, nwrote;
  int old_sex;
  void *buffer;

  dtrace("%p, %p, %i, %i, %lli, %i", D, E, encoding, byte_sex,
      (long long)offset, finalise);

  offset -= D->fragment[E->fragment_index].frame_offset;

  /* Normalise endiannesses */
#if WORDS_BIGENDIAN
  byte_sex = (byte_sex & GD_LITTLE_ENDIAN) ^
    (D->fragment[E->fragment_index].byte_sex & GD_LITTLE_ENDIAN);
#else
  byte_sex = (byte_sex & GD_BIG_ENDIAN) ^
    (D->fragment[E->fragment_index].byte_sex & GD_BIG_ENDIAN);
#endif
  /* Now byte_sex is true if endianness conversion is required. */

  /* If all we're doing is byte swapping, but the encoding or data type doesn't
   * require it, don't do anything */
  if (offset == 0 && encoding == E->e->file[0].encoding && (!byte_sex ||
        enc->ecor == 0 || E->e->size == 1))
  {
    dreturn("%i", 0);
    return 0;
  }

  if ((buffer = malloc(BUFFER_SIZE)) == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (!_GD_Supports(D, E, GD_EF_OPEN | GD_EF_CLOSE | GD_EF_SEEK | GD_EF_READ |
        GD_EF_WRITE | GD_EF_SYNC | GD_EF_UNLINK | GD_EF_TEMP)) {
    dreturn("%i", -1);
    return -1;
  }

  /* check data protection */
  if (D->fragment[E->fragment_index].protection & GD_PROTECT_DATA) {
    _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[E->fragment_index].cname);
    dreturn("%i", -1);
    return -1;
  }
  
  /* Create a temporary file and open it */
  if ((*enc->temp)(E->e->file, GD_TEMP_OPEN)) {
    _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[1].name, errno, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* Open the input file, if necessary */
  if (E->e->file[0].fp == -1 && (*enc->open)(E->e->file, E->e->filebase, 0, 0))
  {
    _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[0].name, errno, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* Adjust for the change in offset */
  if (offset < 0) { /* new offset is less, pad new file */
    if ((*enc->seek)(E->e->file, 0, E->data_type, 1) == -1) {
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[0].name, errno, NULL);
      dreturn("%i", -1);
      return -1;
    }
    if ((*enc->seek)(E->e->file + 1, -offset * E->spf, E->data_type, 1) == -1) {
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[1].name, errno, NULL);
      dreturn("%i", -1);
      return -1;
    }
  } else { /* new offset is more, truncate old file */
    if ((*enc->seek)(E->e->file, offset * E->spf, E->data_type, 0) == -1) {
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[0].name, errno, NULL);
      dreturn("%i", -1);
      return -1;
    }
    if ((*enc->seek)(E->e->file + 1, 0, E->data_type, 1) == -1) {
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[1].name, errno, NULL);
      dreturn("%i", -1);
      return -1;
    }
  }

  /* Now copy the old file to the new file */
  for (;;) {
    nread = (*enc->read)(E->e->file, buffer, E->data_type, ns);

    if (nread < 0) {
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[0].name, errno, NULL);
      break;
    }

    if (nread == 0)
      break;

    /* swap endianness, if required */
    if (byte_sex && enc->ecor)
      _GD_FixEndianness(buffer, E->e->size, ns);

    nwrote = (*enc->write)(E->e->file + 1, buffer, E->data_type, nread);

    if (nwrote < nread) {
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[1].name, errno, NULL);
      break;
    }
  }

  /* Close both files */
  if ((*enc->close)(E->e->file) || (*enc->sync)(E->e->file + 1) ||
      (*enc->close)(E->e->file + 1))
  {
    _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[1].name, errno, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* Finalise the conversion: on error delete the temporary file, otherwise
   * copy it over top of the new one. */
  if (finalise)
    if ((*enc->temp)(E->e->file, (D->error) ? GD_TEMP_DESTROY : GD_TEMP_MOVE))
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[0].name, errno, NULL);

  dreturn("%i", (D->error) ? -1 : 0);
  return (D->error) ? -1 : 0;
}

int dirfile_move(DIRFILE* D, const char* field_code, int new_fragment,
    int move_data)
{
  gd_entry_t *E;

  dtrace("%p, \"%s\", %i, %i", D, field_code, new_fragment, move_data);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* check access mode */
  if ((D->flags & GD_ACCMODE) == GD_RDONLY) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (new_fragment < 0 || new_fragment >= D->n_fragment) {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, new_fragment, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (D->fragment[new_fragment].encoding != GD_UNENCODED &&
      D->fragment[new_fragment].encoding != GD_TEXT_ENCODED &&
      D->fragment[new_fragment].encoding != GD_SLIM_ENCODED)
  {
    _GD_SetError(D, GD_E_UNKNOWN_ENCODING, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ClearError(D);

  E = _GD_FindField(D, field_code, NULL);

  if (E == NULL) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
    dreturn("%i", -1);
    return -1;
  }

  if (E->fragment_index == new_fragment) {
    dreturn("%i", 0);
    return 0;
  }

  if (move_data && E->field_type == GD_RAW_ENTRY &&
      (D->fragment[E->fragment_index].encoding !=
       D->fragment[new_fragment].encoding ||
       D->fragment[E->fragment_index].byte_sex !=
       D->fragment[new_fragment].byte_sex ||
       D->fragment[E->fragment_index].frame_offset !=
       D->fragment[new_fragment].frame_offset))
  {
    if (_GD_MogrifyFile(D, E, D->fragment[new_fragment].encoding,
          D->fragment[new_fragment].byte_sex,
          D->fragment[new_fragment].frame_offset, 1))
    {
      dreturn("%i", -1);
      return -1;
    }
  }

  dreturn("%i", 0);
  return 0;
}
