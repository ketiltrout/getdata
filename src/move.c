/* Copyright (C) 2008-2016 D. V. Wiebe
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

int _GD_MogrifyFile(DIRFILE* D, gd_entry_t* E, unsigned long encoding,
    unsigned long byte_sex, off64_t offset, int finalise, int new_fragment,
    char* new_filebase)
{
  const struct encoding_t* enc_in;
  const struct encoding_t* enc_out;
  const size_t ns = GD_BUFFER_SIZE / E->e->u.raw.size;
  ssize_t nread, nwrote;
  int subencoding = GD_ENC_UNKNOWN;
  int i, ef_swap;
  int arm_fix = 0, endian_fix = 0;
  void *buffer;

  dtrace("%p, %p, %lu, %lu, %" PRId64 ", %i, %i, %p", D, E, encoding, byte_sex,
      (int64_t)offset, finalise, new_fragment, new_filebase);

  if (new_fragment == -1)
    new_fragment = E->fragment_index;

  if (new_filebase == NULL) {
    new_filebase = _GD_Strdup(D, E->e->u.raw.filebase);
    if (new_filebase == NULL) {
      dreturn("%i", -1);
      return -1;
    }
  }

  offset -= D->fragment[E->fragment_index].frame_offset;

  /* Figure out the new subencoding scheme */
  if (encoding == D->fragment[E->fragment_index].encoding &&
      E->e->u.raw.file[0].subenc != GD_ENC_UNKNOWN)
  {
    subencoding = E->e->u.raw.file[0].subenc;
  } else
    for (i = 0; _GD_ef[i].scheme != GD_ENC_UNSUPPORTED; i++) {
      if (_GD_ef[i].scheme == encoding) {
        subencoding = i;
        break;
      }
    }

  if (subencoding == GD_ENC_UNKNOWN) {
    _GD_SetError(D, GD_E_UNKNOWN_ENCODING, GD_E_UNENC_TARGET, NULL, 0, NULL);
    free(new_filebase);
    dreturn("%i", -1);
    return -1;
  }

  enc_out = _GD_ef + subencoding;

  /* Check output encoding */
  if (_GD_MissingFramework(subencoding, GD_EF_CLOSE | GD_EF_SEEK | GD_EF_WRITE |
        GD_EF_SYNC))
  {
    _GD_SetError(D, GD_E_UNSUPPORTED, 0, NULL, 0, NULL);
    free(new_filebase);
    dreturn("%i", -1);
    return -1;
  }

  /* input encoding check */
  if (!_GD_Supports(D, E, GD_EF_NAME | GD_EF_OPEN | GD_EF_CLOSE | GD_EF_SEEK |
        GD_EF_READ | GD_EF_UNLINK))
  {
    free(new_filebase);
    dreturn("%i", -1);
    return -1;
  }

  enc_in = _GD_ef + E->e->u.raw.file[0].subenc;

  /* if neither encoding scheme does internal byte swapping, and the data
   * type can't be endianness swapped, sex differences can't matter */
  if (GD_SIZE(E->e->u.raw.size) != 1 || (enc_in->flags & GD_EF_SWAP) ||
      (enc_out->flags & GD_EF_SWAP))
  {
    /* figure out whether endianness correction is required */
    if ((enc_in->flags & GD_EF_ECOR) || (enc_in->flags & GD_EF_ECOR)) {
      unsigned in_sex = D->fragment[E->fragment_index].byte_sex;
      unsigned out_sex = byte_sex;

      /* fix endian flags for encoding behaviour */
      if (!(enc_in->flags & (GD_EF_SWAP | GD_EF_ECOR))) {
        in_sex = (in_sex & ~(GD_LITTLE_ENDIAN | GD_BIG_ENDIAN)) |
          (out_sex & (GD_LITTLE_ENDIAN | GD_BIG_ENDIAN));
        if (!(enc_in->flags & GD_EF_ECOR))
          in_sex = (in_sex & ~GD_ARM_FLAG) | (out_sex & GD_ARM_FLAG);
      }

      if (!(enc_out->flags & (GD_EF_SWAP | GD_EF_ECOR))) {
        out_sex = (out_sex & ~(GD_LITTLE_ENDIAN | GD_BIG_ENDIAN)) |
          (in_sex & (GD_LITTLE_ENDIAN | GD_BIG_ENDIAN));
        if (!(enc_out->flags & GD_EF_ECOR))
          out_sex = (out_sex & ~GD_ARM_FLAG) | (in_sex | GD_ARM_FLAG);
      }

      endian_fix = _GD_CheckByteSex(E->EN(raw,data_type), in_sex, out_sex, 0,
          &arm_fix);
    }
  }

  /* If all that's changing is the byte sex, but we don't need to do
   * endianness conversion, don't do anything */
  if (offset == 0 && encoding == D->fragment[E->fragment_index].encoding &&
      !endian_fix && !arm_fix && strcmp(new_filebase, E->e->u.raw.filebase) == 0
      && D->fragment[new_fragment].dirfd ==
      D->fragment[E->fragment_index].dirfd)
  {
    free(new_filebase);
    dreturn("%i", 0);
    return 0;
  }

  /* check data protection */
  if (D->fragment[E->fragment_index].protection & GD_PROTECT_DATA ||
      D->fragment[new_fragment].protection & GD_PROTECT_DATA)
  {
    _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_DATA, NULL, 0,
        D->fragment[E->fragment_index].cname);
    free(new_filebase);
    dreturn("%i", -1);
    return -1;
  }

  /* Open the input file, if necessary */
  if (_GD_InitRawIO(D, E, NULL, -1, NULL, 0, GD_FILE_READ,
        _GD_FileSwapBytes(D, E)))
  {
    free(new_filebase);
    dreturn("%i", -1);
    return -1;
  }

  /* set ef_swap, the output encoding in-framework endian correction flag */
  ef_swap = _GD_CheckByteSex(E->EN(raw,data_type), byte_sex, 0, 0, NULL);

  /* Create the output file and open it. If we're changing encodings, we
   * could write to the new file directly.  However, we use a temporary file
   * anyway just to keep things clean. */
  E->e->u.raw.file[1].subenc = subencoding;

  if (_GD_InitRawIO(D, E, new_filebase, new_fragment, enc_out, 0,
        GD_FILE_WRITE | GD_FILE_TEMP, ef_swap))
  {
    free(new_filebase);
    dreturn("%i", -1);
    return -1;
  }

  /* Adjust for the change in offset */
  if (offset < 0) { /* new offset is less, pad new file */
    if ((*enc_in->seek)(E->e->u.raw.file, 0, E->EN(raw,data_type),
          GD_FILE_WRITE) == -1)
    {
      _GD_SetEncIOError(D, GD_E_IO_WRITE, E->e->u.raw.file + 0);
    } else
      _GD_DoSeek(D, E, enc_out, -offset * E->EN(raw,spf), GD_FILE_WRITE
          | GD_FILE_TEMP);
  } else { /* new offset is more, truncate old file */
    if ((*enc_in->seek)(E->e->u.raw.file, offset * E->EN(raw,spf),
          E->EN(raw,data_type), GD_FILE_READ) == -1)
    {
      _GD_SetEncIOError(D, GD_E_IO_WRITE, E->e->u.raw.file + 0);
    } else
      _GD_DoSeek(D, E, enc_out, 0, GD_FILE_WRITE | GD_FILE_TEMP);
  }

  if (D->error) {
    free(new_filebase);
    dreturn("%i", -1);
    return -1;
  }

  if ((buffer = _GD_Malloc(D, GD_BUFFER_SIZE)) == NULL) {
    free(new_filebase);
    dreturn("%i", -1);
    return -1;
  }

  /* Now copy the old file to the new file */
  for (;;) {
    nread = (*enc_in->read)(E->e->u.raw.file, buffer, E->EN(raw,data_type),
        ns);

    if (nread < 0) {
      _GD_SetEncIOError(D, GD_E_IO_READ, E->e->u.raw.file + 0);
      break;
    }

    if (nread == 0)
      break;

    /* swap endianness, if required */
    _GD_FixEndianness(buffer, nread, E->EN(raw,data_type),
        D->fragment[E->fragment_index].byte_sex, byte_sex);

    nwrote = _GD_WriteOut(E, enc_out, buffer, E->EN(raw,data_type), nread, 1);

    if (nwrote < nread) {
      _GD_SetEncIOError(D, GD_E_IO_WRITE, E->e->u.raw.file + 1);
      break;
    }
  }

  free(buffer);


  if (finalise) {
    /* Finalise the conversion: on error delete the temporary file, otherwise
     * copy it over top of the new one. */
    if (D->error) {
      /* An error occurred, delete the temporary file (the old
       * file can stay open) */
      _GD_FiniRawIO(D, E, new_fragment, GD_FINIRAW_CLOTEMP
          | GD_FINIRAW_DISCARD);
    } else {
      struct gd_raw_file_ temp;
      memcpy(&temp, E->e->u.raw.file, sizeof(temp));

      /* discard the old file */
      _GD_FiniRawIO(D, E, E->fragment_index, GD_FINIRAW_DISCARD);

      E->e->u.raw.file[0].name = NULL;
      E->e->u.raw.file[0].subenc = subencoding;

      if ((*_GD_ef[E->e->u.raw.file[0].subenc].name)(D,
            (const char*)D->fragment[E->fragment_index].enc_data,
            E->e->u.raw.file, new_filebase, 0, 0))
      {
        E->e->u.raw.file[0].name = temp.name;
        E->e->u.raw.file[0].subenc = temp.subenc;
      } else if (_GD_FiniRawIO(D, E, new_fragment, GD_FINIRAW_KEEP |
            GD_FINIRAW_CLOTEMP))
      {
        E->e->u.raw.file[0].name = temp.name;
        E->e->u.raw.file[0].subenc = temp.subenc;
      } else if ((subencoding != temp.subenc || strcmp(E->e->u.raw.filebase,
              new_filebase) || D->fragment[new_fragment].dirfd !=
            D->fragment[E->fragment_index].dirfd) && (*enc_in->unlink)(
              D->fragment[E->fragment_index].dirfd, &temp))
      {
        _GD_SetError(D, GD_E_IO, GD_E_IO_UNLINK, temp.name, 0, NULL);
        E->e->u.raw.file[0].name = temp.name;
        E->e->u.raw.file[0].subenc = temp.subenc;
      } else {
        free(temp.name);
        free(E->e->u.raw.filebase);
        E->e->u.raw.filebase = new_filebase;
      }
    }
  } else {
    free(new_filebase);

    /* Close both files */
    _GD_FiniRawIO(D, E, E->fragment_index, GD_FINIRAW_DEFER);
    _GD_FiniRawIO(D, E, new_fragment, GD_FINIRAW_DEFER | GD_FINIRAW_CLOTEMP);
  }

  if (D->error) {
    dreturn("%i", -1);
    return -1;
  }

  dreturn("%i", 0);
  return 0;
}

int _GD_StrCmpNull(const char *s1, const char *s2)
{
  int r;

  dtrace("%p, %p", s1, s2);

  if (s1 == NULL && s2 == NULL) {
    dreturn("%i", 0);
    return 0;
  }

  if (s1 == NULL) {
    dreturn("%i", -1);
    return -1;
  }

  if (s2 == NULL) {
    dreturn("%i", 1);
    return 1;
  }

  r = strcmp(s1, s2);

  dreturn("%i", r);
  return r;
}

static int _GD_Move(DIRFILE *D, gd_entry_t *E, int new_fragment, unsigned flags)
{
  char *new_filebase, *new_code;
  size_t new_len;
  struct gd_rename_data_ *rdat = NULL;
  int i;

  dtrace("%p, %p, %i, 0x%X", D, E, new_fragment, flags);

  if ((D->flags & GD_ACCMODE) == GD_RDONLY)
    GD_SET_RETURN_ERROR(D, GD_E_ACCMODE, 0, NULL, 0, NULL);

  /* check metadata protection */
  if (D->fragment[E->fragment_index].protection & GD_PROTECT_FORMAT ||
      D->fragment[new_fragment].protection & GD_PROTECT_FORMAT)
  {
    GD_SET_RETURN_ERROR(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[E->fragment_index].cname);
  }

  /* Compose the field's new name */

  /* remove the old affixes */
  new_filebase = _GD_StripCode(D, E->fragment_index, E->field, GD_CO_NSALL 
      | GD_CO_ASSERT);
  
  if (!new_filebase) /* Alloc error */
    GD_RETURN_ERROR(D);

  /* add the new affixes */
  new_code = _GD_BuildCode(D, new_fragment, NULL, 0, new_filebase,
      E->flags & GD_EN_EARLY, NULL);
  new_len = strlen(new_code);

  if (new_len != E->e->len || memcmp(new_code, E->field, new_len)) {
    /* duplicate check */
    if (_GD_FindField(D, new_code, new_len, D->entry, D->n_entries, 1, NULL)) {
      _GD_SetError(D, GD_E_DUPLICATE, 0, NULL, 0, new_code);
      free(new_filebase);
      free(new_code);
      GD_RETURN_ERROR(D);
    }

    rdat = _GD_PrepareRename(D, new_code, new_len, E, new_fragment, flags);
    if (rdat == NULL) {
      free(new_filebase);
      GD_RETURN_ERROR(D);
    }
  } else {
    free(new_code);
    new_code = NULL;
  }

  if ((flags & GD_REN_DATA) && E->field_type == GD_RAW_ENTRY &&
      (D->fragment[E->fragment_index].encoding !=
       D->fragment[new_fragment].encoding ||
       D->fragment[E->fragment_index].byte_sex !=
       D->fragment[new_fragment].byte_sex ||
       D->fragment[E->fragment_index].frame_offset !=
       D->fragment[new_fragment].frame_offset ||
       _GD_StrCmpNull(D->fragment[E->fragment_index].sname,
         D->fragment[new_fragment].sname)))
  {
    if (_GD_MogrifyFile(D, E, D->fragment[new_fragment].encoding,
          D->fragment[new_fragment].byte_sex,
          D->fragment[new_fragment].frame_offset, 1, new_fragment,
          new_filebase))
    {
      _GD_CleanUpRename(rdat, 1);
      GD_RETURN_ERROR(D);
    }
  } else
    free(new_filebase);

  /* nothing from now on may fail */
  D->fragment[E->fragment_index].modified = 1;
  D->fragment[new_fragment].modified = 1;
  D->flags &= ~GD_HAVE_VERSION;

  /* update metadata */
  E->fragment_index = new_fragment;
  for (i = 0; i < E->e->n_meta; ++i)
    E->e->p.meta_entry[i]->fragment_index = new_fragment;

  if (rdat)
    _GD_PerformRename(D, rdat);

  /* resort */
  if (new_code)
    qsort(D->entry, D->n_entries, sizeof(gd_entry_t*), _GD_EntryCmp);

  dreturn("%i", 0);
  return 0;
}

int gd_move(DIRFILE *D, const char *field_code, int new_fragment,
    unsigned flags)
{
  gd_entry_t *E;
  int ret;

  dtrace("%p, \"%s\", %i, 0x%X", D, field_code, new_fragment, flags);

  GD_RETURN_ERR_IF_INVALID(D);

  E = _GD_FindField(D, field_code, strlen(field_code), D->entry, D->n_entries,
      0, NULL);

  if (E == NULL)
    GD_SET_RETURN_ERROR(D, GD_E_BAD_CODE, GD_E_CODE_MISSING, NULL, 0,
        field_code);

  if (E->field_type == GD_INDEX_ENTRY)
    GD_SET_RETURN_ERROR(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0,
        "INDEX");

  if (new_fragment < 0 || new_fragment >= D->n_fragment) 
    GD_SET_RETURN_ERROR(D, GD_E_BAD_INDEX, 0, NULL, new_fragment, NULL);

  if (E->fragment_index == new_fragment) {
    dreturn("%i", 0);
    return 0;
  }

  ret = _GD_Move(D, E, new_fragment, flags);

  dreturn("%i", ret);
  return ret;
}
