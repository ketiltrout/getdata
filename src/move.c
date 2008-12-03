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

int _GD_MogrifyFile(DIRFILE* D, gd_entry_t* E, unsigned int encoding,
    unsigned int byte_sex, off64_t offset, int finalise, int new_fragment,
    char* new_filebase)
{
  const struct encoding_t* enc_in;
  const struct encoding_t* enc_out;
  const size_t ns = BUFFER_SIZE / E->e->size;
  ssize_t nread, nwrote;
  int subencoding = GD_ENC_UNKNOWN;
  int i;
  void *buffer;

  dtrace("%p, %p, %u, %u, %lli, %i, %i, %p", D, E, encoding, byte_sex,
      (long long)offset, finalise, new_fragment, new_filebase);

  if (new_fragment == -1)
    new_fragment = E->fragment_index;

  if (new_filebase == NULL) {
    new_filebase = strdup(E->e->filebase);
    if (new_filebase == NULL) {
      _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      dreturn("%i", -1);
      return -1;
    }
  }

  offset -= D->fragment[E->fragment_index].frame_offset;

  /* Figure out the new subencoding scheme */
  if (encoding == D->fragment[E->fragment_index].encoding &&
      E->e->file[0].encoding != GD_ENC_UNKNOWN) {
    subencoding = E->e->file[0].encoding;
  } else
    for (i = 0; ef[i].scheme != GD_ENC_UNSUPPORTED; i++) {
      if (ef[i].scheme == encoding) {
        subencoding = i;
        break;
      }
    }

  if (subencoding == GD_ENC_UNKNOWN) {
    _GD_SetError(D, GD_E_UNKNOWN_ENCODING, 0, NULL, 0, NULL);
    free(new_filebase);
    dreturn("%i", -1);
    return -1;
  }

  enc_out = ef + subencoding;

  /* Check output encoding */
  if (_GD_MissingFramework(subencoding, GD_EF_CLOSE | GD_EF_SEEK | GD_EF_WRITE |
        GD_EF_SYNC | GD_EF_TEMP))
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

  enc_in = ef + E->e->file[0].encoding;

  /* Normalise endiannesses */
#if WORDS_BIGENDIAN
  byte_sex = ((byte_sex & GD_LITTLE_ENDIAN) && enc_out->ecor) ^
    ((D->fragment[E->fragment_index].byte_sex & GD_LITTLE_ENDIAN) &&
     enc_in->ecor);
#else
  byte_sex = ((byte_sex & GD_BIG_ENDIAN) && enc_out->ecor) ^
    ((D->fragment[E->fragment_index].byte_sex & GD_BIG_ENDIAN) && enc_in->ecor);
#endif
  /* Now byte_sex is true if endianness conversion is required. */

  /* If all that's changing is the byte sex, but we don't need to do
   * endianness conversion, don't do anything */
  if (offset == 0 && encoding == D->fragment[E->fragment_index].encoding &&
      !byte_sex)
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

  /* Create the output file and open it. If we're changing encodings, we
   * could write to the new file directly.  However, we use a temporary file
   * anyway just to keep things clean. */
  E->e->file[1].encoding = subencoding;

  if (_GD_SetEncodedName(D, E->e->file + 1, new_filebase, 1))
    ; /* error already set */
  else if ((*enc_out->temp)(E->e->file, GD_TEMP_OPEN))
    _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[1].name, errno, NULL);

  /* Open the input file, if necessary */
  else if (_GD_SetEncodedName(D, E->e->file, E->e->filebase, 0))
    ; /* error already set */
  else if (E->e->file[0].fp == -1 && (*enc_in->open)(E->e->file, 0, 0))
    _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[0].name, errno, NULL);

  if (D->error) {
    free(new_filebase);
    dreturn("%i", -1);
    return -1;
  }

  /* Adjust for the change in offset */
  if (offset < 0) { /* new offset is less, pad new file */
    if ((*enc_in->seek)(E->e->file, 0, E->data_type, 1) == -1)
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[0].name, errno, NULL);
    else if ((*enc_out->seek)(E->e->file + 1, -offset * E->spf, E->data_type, 1)
        == -1)
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[1].name, errno, NULL);
  } else { /* new offset is more, truncate old file */
    if ((*enc_in->seek)(E->e->file, offset * E->spf, E->data_type, 0) == -1)
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[0].name, errno, NULL);
    else if ((*enc_out->seek)(E->e->file + 1, 0, E->data_type, 1) == -1)
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[1].name, errno, NULL);
  }

  if (D->error) {
    free(new_filebase);
    dreturn("%i", -1);
    return -1;
  }

  if ((buffer = malloc(BUFFER_SIZE)) == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    free(new_filebase);
    dreturn("%i", -1);
    return -1;
  }

  /* Now copy the old file to the new file */
  for (;;) {
    nread = (*enc_in->read)(E->e->file, buffer, E->data_type, ns);

    if (nread < 0) {
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[0].name, errno, NULL);
      break;
    }

    if (nread == 0)
      break;

    /* swap endianness, if required */
    if (byte_sex)
      _GD_FixEndianness(buffer, E->e->size, ns);

    nwrote = (*enc_out->write)(E->e->file + 1, buffer, E->data_type, nread);

    if (nwrote < nread) {
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[1].name, errno, NULL);
      break;
    }
  }

  free(buffer);

  /* Close both files */
  if ((*enc_in->close)(E->e->file))
    _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[0].name, errno, NULL);

  if ((*enc_out->sync)(E->e->file + 1) || (*enc_out->close)(E->e->file + 1))
    _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[1].name, errno, NULL);

  if (finalise) {
    /* Finalise the conversion: on error delete the temporary file, otherwise
     * copy it over top of the new one. */
    if (D->error)
      (*enc_out->temp)(E->e->file, GD_TEMP_DESTROY);
    else {
      struct _gd_raw_file temp;
      memcpy(&temp, E->e->file, sizeof(temp));

      E->e->file[0].name = NULL;
      E->e->file[0].encoding = subencoding;

      if (_GD_SetEncodedName(D, E->e->file, E->e->filebase, 0)) {
        E->e->file[0].name = temp.name;
        E->e->file[0].encoding = temp.encoding;
      } else if ((*enc_out->temp)(E->e->file, GD_TEMP_MOVE)) {
        _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[0].name, errno, NULL);
        E->e->file[0].name = temp.name;
        E->e->file[0].encoding = temp.encoding;
      } else if (subencoding != temp.encoding && (*enc_in->unlink)(&temp)) {
        _GD_SetError(D, GD_E_RAW_IO, 0, temp.name, errno, NULL);
        E->e->file[0].name = temp.name;
        E->e->file[0].encoding = temp.encoding;
      } else {
        free(temp.name);
        free(E->e->filebase);
        E->e->filebase = new_filebase;
      }
    }
  }

  if (D->error) {
    free(new_filebase);
    dreturn("%i", -1);
    return -1;
  }

  dreturn("%i", 0);
  return 0;
}

int dirfile_move(DIRFILE* D, const char* field_code, int new_fragment,
    int move_data)
{
  gd_entry_t *E;
  char *new_filebase;

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

  /* check metadata protection */
  if (D->fragment[E->fragment_index].protection & GD_PROTECT_FORMAT ||
      D->fragment[new_fragment].protection & GD_PROTECT_FORMAT)
  {
    _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[E->fragment_index].cname);
    dreturn("%i", -1);
    return -1;
  }

  if (move_data && E->field_type == GD_RAW_ENTRY &&
      (D->fragment[E->fragment_index].encoding !=
       D->fragment[new_fragment].encoding ||
       D->fragment[E->fragment_index].byte_sex !=
       D->fragment[new_fragment].byte_sex ||
       D->fragment[E->fragment_index].frame_offset !=
       D->fragment[new_fragment].frame_offset))
  {
    new_filebase = malloc(FILENAME_MAX);
    if (new_filebase == NULL) {
      _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      dreturn("%i", -1);
      return -1;
    }

    if (D->fragment[new_fragment].sname)
      snprintf(new_filebase, FILENAME_MAX, "%s/%s/%s", D->name,
          D->fragment[new_fragment].sname, E->field);
    else
      snprintf(new_filebase, FILENAME_MAX, "%s/%s", D->name, E->field);

    if (_GD_MogrifyFile(D, E, D->fragment[new_fragment].encoding,
          D->fragment[new_fragment].byte_sex,
          D->fragment[new_fragment].frame_offset, 1, new_fragment,
          new_filebase))
    {
      dreturn("%i", -1);
      return -1;
    }
  }

  /* nothing from now on may fail */
  D->fragment[E->fragment_index].modified = 1;
  D->fragment[new_fragment].modified = 1;
  E->fragment_index = new_fragment;

  dreturn("%i", 0);
  return 0;
}
