/* Copyright (C) 2011-2016 D. V. Wiebe
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

off64_t _GD_GetIOPos(DIRFILE *D, gd_entry_t *E, off64_t index_pos)
{
  int i;
  off64_t pos = GD_E_INTERNAL_ERROR, pos2;

  dtrace("%p, %p, %" PRId64, D, E, (int64_t)index_pos);

  if (++D->recurse_level >= GD_MAX_RECURSE_LEVEL) {
    D->recurse_level--;
    GD_SET_RETURN_ERROR(D, GD_E_RECURSE_LEVEL, GD_E_RECURSE_CODE, NULL, 0,
        E->field);
  }

  if (_GD_FindInputs(D, E, 1)) {
    D->recurse_level--;
    dreturn("%i", D->error);
    return D->error;
  }

  switch (E->field_type) {
    case GD_RAW_ENTRY:
      /* We must open the file to know its starting offset */
      if (E->e->u.raw.file[0].idata < 0)
        if (_GD_InitRawIO(D, E, NULL, -1, NULL, 0, GD_FILE_READ,
              _GD_FileSwapBytes(D, E)))
        {
          break;
        }
      pos = E->e->u.raw.file[0].pos + E->EN(raw,spf) *
        D->fragment[E->fragment_index].frame_offset;
      break;
    case GD_LINCOM_ENTRY:
      pos = _GD_GetIOPos(D, E->e->entry[0], -1);
      if (!D->error)
        for (i = 1; i < E->EN(lincom,n_fields); ++i) {
          pos2 = _GD_GetIOPos(D, E->e->entry[i], pos);
          if (pos2 != pos) {
            _GD_SetError(D, GD_E_DOMAIN, GD_E_DOMAIN_MULTIPOS, NULL, 0, NULL);
            break;
          }
        }
      break;
    case GD_LINTERP_ENTRY:
    case GD_BIT_ENTRY:
    case GD_POLYNOM_ENTRY:
    case GD_SBIT_ENTRY:
    case GD_RECIP_ENTRY:
    case GD_INDIR_ENTRY:
    case GD_SINDIR_ENTRY:
      pos = _GD_GetIOPos(D, E->e->entry[0], -1);
      break;
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
    case GD_WINDOW_ENTRY:
    case GD_MPLEX_ENTRY:
      pos = _GD_GetIOPos(D, E->e->entry[0], -1);
      if (D->error)
        break;
      pos2 = _GD_GetIOPos(D, E->e->entry[1], pos);
      if (!D->error && pos != pos2)
        _GD_SetError(D, GD_E_DOMAIN, GD_E_DOMAIN_MULTIPOS, NULL, 0, NULL);
      break;
    case GD_PHASE_ENTRY:
      pos = _GD_GetIOPos(D, E->e->entry[0], -1);
      if (pos >= 0)
        pos += E->EN(phase,shift);
      break;
    case GD_INDEX_ENTRY:
      if (index_pos == -1)
        pos = E->e->u.index_pos;
      else
        pos = index_pos;
      break;
    case GD_NO_ENTRY:
    case GD_CONST_ENTRY:
    case GD_STRING_ENTRY:
    case GD_CARRAY_ENTRY:
    case GD_SARRAY_ENTRY:
    case GD_ALIAS_ENTRY:
      _GD_InternalError(D);
  }

  if (D->error)
    pos = D->error;

  D->recurse_level--;
  dreturn("%" PRId64, (int64_t)pos);
  return pos;
}

/* Get the current I/O position of the given field
*/
off64_t gd_tell64(DIRFILE *D, const char *field_code) gd_nothrow
{
  off64_t pos = -1;
  gd_entry_t* entry;

  dtrace("%p, \"%s\"", D, field_code);

  GD_RETURN_ERR_IF_INVALID(D);

  entry = _GD_FindEntry(D, field_code);

  if (D->error)
    GD_RETURN_ERROR(D);

  if (entry->field_type & GD_SCALAR_ENTRY_BIT)
    GD_SET_RETURN_ERROR(D, GD_E_DIMENSION, GD_E_DIM_CALLER, NULL, 0,
        field_code);
  else
    pos = _GD_GetIOPos(D, entry, -1);

  dreturn("%" PRId64, (int64_t)pos);
  return pos;
}

off_t gd_tell(DIRFILE *D, const char *field_code) gd_nothrow
{
  return (off_t)gd_tell64(D, field_code);
}

off64_t _GD_DoSeek(DIRFILE *D, gd_entry_t *E, const struct encoding_t *enc,
    off64_t offset, unsigned int mode)
{
  off64_t pos;
  const int oop_write = ((enc->flags & GD_EF_OOP) &&
      (E->e->u.raw.file[1].idata >= 0)) ? 1 : 0;
  const int temp = (mode & GD_FILE_TEMP) ? 1 : 0;
  const int which = (oop_write || temp) ? 1 : 0;

  dtrace("%p, %p, %p, %" PRId64 ", 0x%X", D, E, enc, (int64_t)offset, mode);

  /* Yet another overflow check */
  if (GD_SIZE(E->EN(raw,data_type)) > 0 &&
      offset > GD_INT64_MAX / GD_SIZE(E->EN(raw,data_type)))
  {
    GD_SET_RETURN_ERROR(D, GD_E_RANGE, GD_E_OUT_OF_RANGE, NULL, 0, NULL);
  }

  if (oop_write) {
    /* in this case we need to close and then re-open the file */
    if (offset < E->e->u.raw.file[1].pos) {
      if (_GD_FiniRawIO(D, E, E->fragment_index, GD_FINIRAW_KEEP))
        GD_RETURN_ERROR(D);
      else if (_GD_InitRawIO(D, E, NULL, -1, NULL, GD_EF_SEEK, GD_FILE_WRITE,
            _GD_FileSwapBytes(D, E)))
      {
        GD_RETURN_ERROR(D);
      }
    }

    /* If the read-side file is open, and the read and write pointers agree,
     * and the I/O pointers are less than the target offset, then read data
     * from the read-side until we reach the point we're interested in, or
     * we run out of data */
    if (E->e->u.raw.file[0].idata >= 0 &&
        E->e->u.raw.file[0].pos == E->e->u.raw.file[1].pos &&
        offset > E->e->u.raw.file[1].pos)
    {
      char *buffer, *ptr;
      ssize_t n_read, n_wrote;
      const off64_t chunk_size = GD_BUFFER_SIZE / GD_SIZE(E->EN(raw,data_type));

      off64_t count, remaining = offset - E->e->u.raw.file[1].pos;

      buffer = _GD_Malloc(D, GD_BUFFER_SIZE);
      if (buffer == NULL)
        GD_RETURN_ERROR(D);

      /* Read loop */
      while (remaining > 0) {
        if (remaining > chunk_size)
          count = chunk_size;
        else
          count = remaining;

        n_read = (*enc->read)(E->e->u.raw.file, buffer, E->EN(raw,data_type),
            count);

        if (n_read < 0) {
          _GD_SetEncIOError(D, GD_E_IO_READ, E->e->u.raw.file + 0);
          free(buffer);
          GD_RETURN_ERROR(D);
        } else if (n_read == 0) /* early EOF - will need to pad writer */
          break;

        /* Write loop */
        ptr = buffer;
        while (n_read > 0) {
          n_wrote = (*enc->write)(E->e->u.raw.file + 1, ptr,
              E->EN(raw,data_type), n_read);
          if (n_wrote < 0) {
            _GD_SetEncIOError(D, GD_E_IO_WRITE, E->e->u.raw.file + 1);
            free(buffer);
            GD_RETURN_ERROR(D);
          }
          remaining -= n_wrote;
          n_read -= n_wrote;
          ptr += n_wrote;
        }
      }

      free(buffer);

      /* We can exit early if there's nothing remaining, since that means
       * we successfully moved the write pointer to the right place
       */
      if (remaining == 0) {
        dreturn("%" PRId64, (int64_t)offset);
        return offset;
      }
    }
  }

  /* Do the seek.  For OOP writes, this function is only called when we need
   * to pad, since the OOP loop above should have already copied everything
   * appropriate from the read-side to the write-side.
   */
  pos = (*enc->seek)(E->e->u.raw.file + which, offset, E->EN(raw,data_type),
      mode);

  /* Seek error */
  if (pos < 0) {
    _GD_SetEncIOError(D, (mode & GD_FILE_WRITE) ? GD_E_IO_WRITE : GD_E_IO_READ,
        E->e->u.raw.file + 0);
    GD_RETURN_ERROR(D);
  }

  dreturn("%" PRId64, (int64_t)pos);
  return pos;
}

int _GD_Seek(DIRFILE *D, gd_entry_t *E, off64_t offset, unsigned int mode)
{
  int i;

  dtrace("%p, %p, %" PRId64 ", 0x%X", D, E, (int64_t)offset, mode);

  if (++D->recurse_level >= GD_MAX_RECURSE_LEVEL) {
    D->recurse_level--;
    GD_SET_RETURN_ERROR(D, GD_E_RECURSE_LEVEL, GD_E_RECURSE_CODE, NULL, 0,
        E->field);
  }

  _GD_FindInputs(D, E, 1);

  if (D->error) {
    D->recurse_level--;
    GD_RETURN_ERROR(D);
  }

  if (offset < 0)
    GD_SET_RETURN_ERROR(D, GD_E_RANGE, GD_E_OUT_OF_RANGE, NULL, 0, NULL);

  switch (E->field_type) {
    case GD_RAW_ENTRY:
      /* open/create the file, if necessary */
      if (_GD_InitRawIO(D, E, NULL, -1, NULL, GD_EF_SEEK, mode,
            _GD_FileSwapBytes(D, E)))
      {
        break;
      }

      /* The requested offset is before the start of the file, so I guess
       * pretend we've repositioned it...
       */
      if (E->EN(raw,spf) * D->fragment[E->fragment_index].frame_offset >
          offset) {
        E->e->u.raw.file[0].pos = offset - E->EN(raw,spf) *
          D->fragment[E->fragment_index].frame_offset;
        break;
      }

      _GD_DoSeek(D, E, _GD_ef + E->e->u.raw.file[0].subenc, offset -
          E->EN(raw,spf) * D->fragment[E->fragment_index].frame_offset, mode);
      break;
    case GD_LINCOM_ENTRY:
      for (i = 0; i < E->EN(lincom,n_fields); ++i)
        _GD_Seek(D, E->e->entry[i], offset, mode);
      break;
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
    case GD_WINDOW_ENTRY:
    case GD_MPLEX_ENTRY:
      if (_GD_Seek(D, E->e->entry[1], offset, mode))
        break;
      /* fallthrough */
    case GD_LINTERP_ENTRY:
    case GD_BIT_ENTRY:
    case GD_POLYNOM_ENTRY:
    case GD_SBIT_ENTRY:
    case GD_RECIP_ENTRY:
    case GD_INDIR_ENTRY:
    case GD_SINDIR_ENTRY:
      _GD_Seek(D, E->e->entry[0], offset, mode);
      break;
    case GD_PHASE_ENTRY:
      _GD_Seek(D, E->e->entry[0], offset - E->EN(phase,shift), mode);
      break;
    case GD_INDEX_ENTRY:
      E->e->u.index_pos = offset;
      break;
    case GD_NO_ENTRY:
    case GD_CONST_ENTRY:
    case GD_STRING_ENTRY:
    case GD_CARRAY_ENTRY:
    case GD_SARRAY_ENTRY:
    case GD_ALIAS_ENTRY:
      _GD_InternalError(D);
  }

  D->recurse_level--;
  GD_RETURN_ERROR(D);
}

/* Set the I/O position of the given field
*/
off64_t gd_seek64(DIRFILE *D, const char *field_code, off64_t frame_num,
    off64_t sample_num, int whence)
{
  unsigned int spf = 0;
  off64_t pos = 0;
  gd_entry_t* entry;
  int is_index = 0;
  unsigned int mode = (whence & GD_SEEK_WRITE) ? GD_FILE_WRITE : GD_FILE_READ;

  dtrace("%p, \"%s\", %" PRId64 ", %" PRId64 ", 0x%X", D, field_code,
      (int64_t)frame_num, (int64_t)sample_num, whence);

  GD_RETURN_ERR_IF_INVALID(D);

  entry = _GD_FindEntry(D, field_code);

  if (D->error)
    GD_RETURN_ERROR(D);

  if (entry->field_type & GD_SCALAR_ENTRY_BIT)
    GD_SET_RETURN_ERROR(D, GD_E_DIMENSION, GD_E_DIM_CALLER, NULL, 0,
        field_code);

  if (frame_num) {
    spf = _GD_GetSPF(D, entry);

    if (D->error)
      GD_RETURN_ERROR(D);

    /* don't overflow */
    if ((frame_num > 0 && sample_num > GD_INT64_MAX - spf * frame_num) ||
        (frame_num < 0 && sample_num < -GD_INT64_MAX - spf * frame_num))
    {
      GD_SET_RETURN_ERROR(D, GD_E_RANGE, GD_E_OUT_OF_RANGE, NULL, 0, NULL);
    }
    sample_num += frame_num * spf;
  }

  whence &= (GD_SEEK_SET | GD_SEEK_CUR | GD_SEEK_END);
  if (whence == GD_SEEK_SET)
    pos = 0;
  else if (whence == GD_SEEK_CUR)
    pos = _GD_GetIOPos(D, entry, -1);
  else if (whence == GD_SEEK_END) {
    pos = _GD_GetEOF(D, entry, NULL, &is_index);
    if (is_index)
      _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
  } else
    _GD_SetError(D, GD_E_ARGUMENT, GD_E_ARG_WHENCE, NULL, 0, NULL);

  if (!D->error) {
    /* Check for overflow again */
    if ((sample_num > 0 && pos > GD_INT64_MAX - sample_num) ||
        (sample_num < 0 && pos < -GD_INT64_MAX - sample_num))
    {
      _GD_SetError(D, GD_E_RANGE, GD_E_OUT_OF_RANGE, NULL, 0, NULL);
    } else
      _GD_Seek(D, entry, sample_num + pos, mode);
  }

  if (D->error)
    GD_RETURN_ERROR(D);

  pos = _GD_GetIOPos(D, entry, -1);

  dreturn("%" PRId64, (int64_t)pos);
  return pos;
}

off_t gd_seek(DIRFILE *D, const char *field_code, off_t frame_num,
    off_t sample_num, int whence)
{
  return (off_t)gd_seek64(D, field_code, frame_num, sample_num, whence);
}
/* vim: ts=2 sw=2 et tw=80
*/
