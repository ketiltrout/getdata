/* Copyright (C) 2008-2011 D. V. Wiebe
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
  const size_t ns = BUFFER_SIZE / E->e->u.raw.size;
  ssize_t nread, nwrote;
  int subencoding = GD_ENC_UNKNOWN;
  int i, ef_swap;
  int arm_endianise;
  void *buffer;

  dtrace("%p, %p, %lu, %lu, %lli, %i, %i, %p", D, E, encoding, byte_sex,
      (long long)offset, finalise, new_fragment, new_filebase);

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
    for (i = 0; _gd_ef[i].scheme != GD_ENC_UNSUPPORTED; i++) {
      if (_gd_ef[i].scheme == encoding) {
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

  enc_out = _gd_ef + subencoding;

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
  if (!_GD_Supports(D, E, GD_EF_OPEN | GD_EF_CLOSE | GD_EF_SEEK | GD_EF_READ
        | GD_EF_UNLINK))
  {
    free(new_filebase);
    dreturn("%i", -1);
    return -1;
  }

  enc_in = _gd_ef + E->e->u.raw.file[0].subenc;

  /* Need to do the ARM thing? */
  arm_endianise = (((byte_sex & GD_ARM_FLAG) && (enc_out->flags & GD_EF_ECOR)) ^
      ((D->fragment[E->fragment_index].byte_sex & GD_ARM_FLAG) &&
       (enc_in->flags & GD_EF_ECOR))) && (E->EN(raw,data_type) == GD_FLOAT64 ||
       E->EN(raw,data_type) == GD_COMPLEX128);

  /* Normalise endiannesses */
#ifdef WORDS_BIGENDIAN
  ef_swap = (byte_sex & GD_LITTLE_ENDIAN) ? 1 : 0;
  byte_sex = ((byte_sex & GD_LITTLE_ENDIAN) &&
      (enc_out->flags & (GD_EF_ECOR | GD_EF_SWAP))) ^
    ((D->fragment[E->fragment_index].byte_sex & GD_LITTLE_ENDIAN) &&
     (enc_in->flags & (GD_EF_ECOR | GD_EF_SWAP)));
#else
  ef_swap = (byte_sex & GD_BIG_ENDIAN) ? 1 : 0;
  byte_sex = ((byte_sex & GD_BIG_ENDIAN) &&
      (enc_out->flags & (GD_EF_ECOR | GD_EF_SWAP))) ^
    ((D->fragment[E->fragment_index].byte_sex & GD_BIG_ENDIAN) &&
     (enc_in->flags & (GD_EF_ECOR | GD_EF_SWAP)));
#endif
  /* Now byte_sex is true if endianness conversion is required. */

  /* If all that's changing is the byte sex, but we don't need to do
   * endianness conversion, don't do anything */
  if (offset == 0 && encoding == D->fragment[E->fragment_index].encoding &&
      !byte_sex && !arm_endianise && strcmp(new_filebase,
        E->e->u.raw.filebase) == 0 && D->fragment[new_fragment].dirfd ==
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
  if (_GD_InitRawIO(D, E, NULL, 0, NULL, 0, GD_FILE_READ, 0)) {
    free(new_filebase);
    dreturn("%i", -1);
    return -1;
  }

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
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.raw.file[0].name, errno, NULL);
    } else
      _GD_WriteSeek(D, E, enc_out, -offset * E->EN(raw,spf), GD_FILE_WRITE
          | GD_FILE_TEMP);
  } else { /* new offset is more, truncate old file */
    if ((*enc_in->seek)(E->e->u.raw.file, offset * E->EN(raw,spf),
          E->EN(raw,data_type), GD_FILE_READ) == -1)
    {
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.raw.file[0].name, errno, NULL);
    } else
      _GD_WriteSeek(D, E, enc_out, 0, GD_FILE_WRITE | GD_FILE_TEMP);
  }

  if (D->error) {
    free(new_filebase);
    dreturn("%i", -1);
    return -1;
  }

  if ((buffer = _GD_Malloc(D, BUFFER_SIZE)) == NULL) {
    free(new_filebase);
    dreturn("%i", -1);
    return -1;
  }

  /* Now copy the old file to the new file */
  for (;;) {
    nread = (*enc_in->read)(E->e->u.raw.file, buffer, E->EN(raw,data_type),
        ns);

    if (nread < 0) {
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.raw.file[0].name, errno, NULL);
      break;
    }

    if (nread == 0)
      break;

    /* fix army-ness, if required */
    if (arm_endianise)
      _GD_ArmEndianise((uint64_t *)buffer, E->EN(raw,data_type) & GD_COMPLEX,
          nread);

    /* swap endianness, if required */
    if (byte_sex) {
      if (E->EN(raw,data_type) & GD_COMPLEX)
        _GD_FixEndianness((char *)buffer, E->e->u.raw.size / 2, nread * 2);
      else
        _GD_FixEndianness((char *)buffer, E->e->u.raw.size, nread);
    }

    nwrote = _GD_WriteOut(D, E, enc_out, buffer, E->EN(raw,data_type), nread,
        1);

    if (nwrote < nread) {
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.raw.file[1].name, errno, NULL);
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
      struct _gd_raw_file temp;
      memcpy(&temp, E->e->u.raw.file, sizeof(temp));

      /* discard the old file */
      _GD_FiniRawIO(D, E, E->fragment_index, GD_FINIRAW_DISCARD);

      E->e->u.raw.file[0].name = NULL;
      E->e->u.raw.file[0].subenc = subencoding;

      if (_GD_SetEncodedName(D, E->e->u.raw.file, new_filebase, 0)) {
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
        _GD_SetError(D, GD_E_RAW_IO, 0, temp.name, errno, NULL);
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

int _GD_Move(DIRFILE *D, gd_entry_t *E, int new_fragment, int move_data)
{
  char *new_filebase, *new_code;
  char **new_meta = NULL;
  int i, dummy;

  dtrace("%p, %p, %i, %i", D, E, new_fragment, move_data);

  /* check access mode */
  if ((D->flags & GD_ACCMODE) == GD_RDONLY) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* check metadata protection */
  if (D->fragment[E->fragment_index].protection & GD_PROTECT_FORMAT ||
      D->fragment[new_fragment].protection & GD_PROTECT_FORMAT)
  {
    _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[E->fragment_index].cname);
    dreturn("%i", -1);
    return -1;
  }

  /* Compose the field's new name */
  new_filebase = _GD_MungeCode(D, NULL, D->fragment[E->fragment_index].prefix,
      D->fragment[E->fragment_index].suffix, NULL, NULL, E->field, &dummy);
  
  if (!new_filebase) {
    _GD_InternalError(D); /* the prefix/suffix wasn't found */
    dreturn("%i", -1);
    return -1;
  }

  new_code = _GD_MungeFromFrag(D, NULL, new_fragment, new_filebase, &dummy);

  if (strcmp(new_code, E->field)) {
    /* duplicate check */
    if (_GD_FindField(D, new_code, D->entry, D->n_entries, 1, NULL)) {
      _GD_SetError(D, GD_E_DUPLICATE, 0, NULL, 0, new_code);
      free(new_filebase);
      dreturn("%i", -1);
      return -1;
    }

    /* compose all the new meta field names.  We must do this now in 
     * a temporary location in case it fails and/or subsequent stuff fails */
    if (E->e->n_meta > 0) {
      int nlen = strlen(new_code);
      int olen = strlen(E->field);
      new_meta = _GD_Malloc(D, sizeof(char *) * E->e->n_meta);
      if (!new_meta) {
        free(new_filebase);
        free(new_code);
        dreturn("%i", -1);
        return -1;
      }

      memset(new_meta, 0, sizeof(char *) * E->e->n_meta);
      for (i = 0; i < E->e->n_meta; ++i) {
        new_meta[i] = _GD_Malloc(D,
            strlen(E->e->p.meta_entry[i]->field) + nlen - olen + 1);
        if (new_meta[i] == NULL)
          break;
        sprintf(new_meta[i], "%s/%s", new_code,
            E->e->p.meta_entry[i]->field + olen + 1);
      }

      if (D->error) {
        for (i = 0; i < E->e->n_meta; ++i)
          free(new_meta[i]);
        free(new_meta);
        free(new_filebase);
        free(new_code);
        dreturn("%i", -1);
        return -1;
      }
    }
  } else {
    free(new_code);
    new_code = NULL;
  }

  if (move_data && E->field_type == GD_RAW_ENTRY &&
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
      if (new_meta) {
        for (i = 0; i < E->e->n_meta; ++i)
          free(new_meta[i]);
        free(new_meta);
      }
      free(new_code);
      dreturn("%i", -1);
      return -1;
    }
  } else
    free(new_filebase);

  /* nothing from now on may fail */
  D->fragment[E->fragment_index].modified = 1;
  D->fragment[new_fragment].modified = 1;
  D->flags &= ~GD_HAVE_VERSION;
  E->fragment_index = new_fragment;

  /* update meta fields */
  for (i = 0; i < E->e->n_meta; ++i) {
    E->e->p.meta_entry[i]->fragment_index = new_fragment;
    if (new_meta) {
      free(E->e->p.meta_entry[i]->field);
      E->e->p.meta_entry[i]->field = new_meta[i];
    }
  }

  if (new_code) {
    free(new_meta);
    free(E->field);
    E->field = new_code;

    /* resort */
    qsort(D->entry, D->n_entries, sizeof(gd_entry_t*), _GD_EntryCmp);
    qsort(D->dot_list, D->n_dot, sizeof(gd_entry_t*), _GD_EntryCmp);
  }

  dreturn("%i", 0);
  return 0;
}

int gd_move(DIRFILE *D, const char *field_code, int new_fragment, int move_data)
{
  gd_entry_t *E;
  int ret;

  dtrace("%p, \"%s\", %i, %i", D, field_code, new_fragment, move_data);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ClearError(D);

  E = _GD_FindField(D, field_code, D->entry, D->n_entries, 1, NULL);

  if (E == NULL) {
    _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_MISSING, NULL, 0, field_code);
    dreturn("%i", -1);
    return -1;
  }

  if (E->field_type == GD_INDEX_ENTRY) {
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, "INDEX");
    dreturn("%i", -1);
    return -1;
  }

  if (new_fragment < 0 || new_fragment >= D->n_fragment) {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, new_fragment, NULL);
    dreturn("%i", -1);
    return -1;
  } else if (E->fragment_index == new_fragment) {
    dreturn("%i", 0);
    return 0;
  }

  ret = _GD_Move(D, E, new_fragment, move_data);

  dreturn("%i", ret);
  return ret;
}

int gd_move_alias(DIRFILE *D, const char *field_code, int new_fragment)
  gd_nothrow
{
  gd_entry_t *E;
  int ret;

  dtrace("%p, \"%s\", %i", D, field_code, new_fragment);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ClearError(D);

  E = _GD_FindField(D, field_code, D->entry, D->n_entries, 0, NULL);

  if (E == NULL) {
    _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_MISSING, NULL, 0, field_code);
    dreturn("%i", -1);
    return -1;
  }

  if (E->field_type != GD_ALIAS_ENTRY) {
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
    dreturn("%i", -1);
    return -1;
  }

  if (new_fragment < 0 || new_fragment >= D->n_fragment) {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, new_fragment, NULL);
    dreturn("%i", -1);
    return -1;
  } else if (E->fragment_index == new_fragment) {
    dreturn("%i", 0);
    return 0;
  }

  ret = _GD_Move(D, E, new_fragment, 0);

  dreturn("%i", ret);
  return ret;
}
