/* (C) 2003-2005 C. Barth Netterfield
 * (C) 2003-2005 Theodore Kisner
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
#include <inttypes.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#endif

static size_t _GD_DoRawOut(DIRFILE *D, gd_entry_t *E,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t data_type, const void *data_in)
{
  off64_t s0;
  size_t ns, n_wrote;
  void *databuffer;

  dtrace("%p, %p, %lli, %lli, %zi, %zi, 0x%x, %p", D, E, first_frame,
      first_samp, num_frames, num_samp, data_type, data_in);

  s0 = first_samp + first_frame * E->spf;
  ns = num_samp + num_frames * E->spf;

  if (s0 < 0) {
    _GD_SetError(D, GD_E_RANGE, 0, NULL, 0, NULL);
    dreturn("%zi", 0);
    return 0;
  }

  databuffer = _GD_Alloc(D, E->data_type, ns);

  _GD_ConvertType(D, data_in, data_type, databuffer, E->data_type, ns);

  if (D->error) { /* bad input type */
    free(databuffer);
    dreturn("%zi", 0);
    return 0;
  }

  if (D->include_list[E->fragment_index].flags &
#ifdef WORDS_BIGENDIAN
      GD_LITTLE_ENDIAN
#else
      GD_BIG_ENDIAN
#endif
     )
    _GD_FixEndianness(databuffer, E->size, ns);

  /* write data to file.  Note that if the first sample is beyond     */
  /* the current end of file, a gap will result (see lseek(2)) */

  /* Figure out the dirfile encoding type, if required */
  if ((D->include_list[E->fragment_index].flags & GD_ENCODING) ==
      GD_AUTO_ENCODED)
    D->include_list[E->fragment_index].flags =
      (D->include_list[E->fragment_index].flags & ~GD_ENCODING) |
      _GD_ResolveEncoding(E->e->file, D->include_list[E->fragment_index].flags,
          E->e);

  /* If the encoding is still unknown, none of the candidate files exist;
   * as a result, we don't know the intended encoding type */
  if ((D->include_list[E->fragment_index].flags & GD_ENCODING) ==
      GD_AUTO_ENCODED)
  {
    _GD_SetError(D, GD_E_UNKNOWN_ENCODING, 0, NULL, 0, NULL);
    dreturn("%zi", 0);
    return 0;
  }

  /* Figure out the encoding subtype, if required */
  if (E->e->encoding == GD_ENC_UNKNOWN)
    _GD_ResolveEncoding(E->e->file, D->include_list[E->fragment_index].flags,
        E->e);

  if (E->e->fp < 0) {
    /* open file for reading / writing if not already opened */

    if (encode[E->e->encoding].open == NULL) {
      _GD_SetError(D, GD_E_UNSUPPORTED, 0, NULL, 0, NULL);
      dreturn("%zi", 0);
      return 0;
    } else if ((*encode[E->e->encoding].open)(E->e, E->e->file,
          D->flags & GD_ACCMODE, 1))
    {
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file, errno, NULL);
      dreturn("%zi", 0);
      return 0;
    }
  }

  if (encode[E->e->encoding].seek == NULL) {
    _GD_SetError(D, GD_E_UNSUPPORTED, 0, NULL, 0, NULL);
    dreturn("%zi", 0);
    return 0;
  }

  (*encode[E->e->encoding].seek)(E->e, s0, E->data_type, 1);

  if (encode[E->e->encoding].write == NULL) {
    _GD_SetError(D, GD_E_UNSUPPORTED, 0, NULL, 0, NULL);
    dreturn("%zi", 0);
    return 0;
  }

  n_wrote = (*encode[E->e->encoding].write)(E->e, databuffer, E->data_type, ns);
  n_wrote /= E->size;

  free(databuffer);

  dreturn("%zi", n_wrote);
  return n_wrote;
}

static size_t _GD_DoLinterpOut(DIRFILE* D, gd_entry_t *E,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t data_type, const void *data_in)
{
  int spf;
  size_t ns;
  size_t n_wrote;

  dtrace("%p, %p, %lli, %lli, %zi, %zi, 0x%x, %p", D, E, first_frame,
      first_samp, num_frames, num_samp, data_type, data_in);

  if (E->e->table_len < 0) {
    _GD_ReadLinterpFile(D, E);
    if (D->error != GD_E_OK) {
      dreturn("%zi", 0);
      return 0;
    }
  }

  /* Interpolate X(y) instead of Y(x) */

  if (E->e->entry[0] == NULL) {
    E->e->entry[0] = _GD_FindField(D, E->in_fields[0], NULL);

    if (E->e->entry[0] == NULL) {
      _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, E->in_fields[0]);
      dreturn("%zi", 0);
      return 0;
    }

    /* scalar entries not allowed */
    if (E->e->entry[0]->field_type & GD_SCALAR_ENTRY) {
      _GD_SetError(D, GD_E_DIMENSION, 0, E->field, 0, E->e->entry[0]->field);
      dreturn("%zi", 0);
      return 0;
    }
  }

  spf = _GD_GetSPF(D, E->e->entry[0]);
  ns = num_samp + num_frames * (int)spf;

  _GD_LinterpData(D, data_in, data_type, ns, E->e->y, E->e->x, E->e->table_len);

  if (D->error != GD_E_OK) {
    dreturn("%zi", 0);
    return 0;
  }

  n_wrote = _GD_DoFieldOut(D, E->e->entry[0], E->in_fields[0], first_frame,
      first_samp, num_frames, num_samp, data_type, data_in);

  dreturn("%zi", n_wrote);
  return n_wrote;
}

static size_t _GD_DoLincomOut(DIRFILE* D, gd_entry_t *E,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t data_type, const void *data_in)
{
  int spf;
  size_t ns, n_wrote;
  void* tmpbuf;

  dtrace("%p, %p, %lli, %lli, %zi, %zi, 0x%x, %p", D, E, first_frame,
      first_samp, num_frames, num_samp, data_type, data_in);

  /* we cannot write to LINCOM fields that are a linear combination */
  /* of more than one raw field (no way to know how to split data). */

  if (E->n_fields > 1) {
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_PUT, NULL, 0, E->field);
    dreturn("%zi", 0);
    return 0;
  }

  if (E->e->entry[0] == NULL) {
    E->e->entry[0] = _GD_FindField(D, E->in_fields[0], NULL);

    if (E->e->entry[0] == NULL) {
      _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, E->in_fields[0]);
      dreturn("%zi", 0);
      return 0;
    }

    /* scalar entries not allowed */
    if (E->e->entry[0]->field_type & GD_SCALAR_ENTRY) {
      _GD_SetError(D, GD_E_DIMENSION, 0, E->field, 0, E->e->entry[0]->field);
      dreturn("%zi", 0);
      return 0;
    }
  }

  /* do the inverse scaling */
  spf = _GD_GetSPF(D, E->e->entry[0]);
  ns = num_samp + num_frames * (int)spf;

  if (D->error != GD_E_OK) {
    dreturn("%zi", 0);
    return 0;
  }

  /* writeable copy */
  tmpbuf = _GD_Alloc(D, data_type, ns);

  if (tmpbuf == NULL) {
    dreturn("%zi", 0);
    return 0;
  }

  memcpy(tmpbuf, data_in, ns * GD_SIZE(data_type));

  _GD_ScaleData(D, tmpbuf, data_type, ns, 1 / E->m[0], -E->b[0] / E->m[0]);

  if (D->error != GD_E_OK) {
    free(tmpbuf);
    dreturn("%zi", 0);
    return 0;
  }

  n_wrote = _GD_DoFieldOut(D, E->e->entry[0], E->in_fields[0], first_frame,
      first_samp, num_frames, num_samp, data_type, tmpbuf);
  free(tmpbuf);

  dreturn("%zi", n_wrote);
  return n_wrote;
}

static size_t _GD_DoBitOut(DIRFILE* D, gd_entry_t *E,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t data_type, const void *data_in)
{
  uint64_t *tmpbuf;
  uint64_t *readbuf;
  size_t i, n_wrote;
  int spf;
  size_t ns;

  dtrace("%p, %p, %lli, %lli, %zi, %zi, 0x%x, %p", D, E, first_frame,
      first_samp, num_frames, num_samp, data_type, data_in);

  const uint64_t mask = (E->numbits == 64) ? 0xffffffffffffffffULL :
    ((uint64_t)1 << E->numbits) - 1;

#ifdef GETDATA_DEBUG
  fprintf(stdout,"DoBitOut:  bitnum = %d numbits = %d mask = %llx\n",
      E->bitnum, E->numbits, mask);
#endif

  if (E->e->entry[0] == NULL) {
    E->e->entry[0] = _GD_FindField(D, E->in_fields[0], NULL);
    
    if (E->e->entry[0] == NULL) {
      _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, E->in_fields[0]);
      dreturn("%zi", 0);
      return 0;
    }

    /* scalar entries not allowed */
    if (E->e->entry[0]->field_type & GD_SCALAR_ENTRY) {
      _GD_SetError(D, GD_E_DIMENSION, 0, E->field, 0, E->e->entry[0]->field);
      dreturn("%zi", 0);
      return 0;
    }
  }

  spf = _GD_GetSPF(D, E->e->entry[0]);

  if (D->error != GD_E_OK) {
    dreturn("%zi", 0);
    return 0;
  }

  ns = num_samp + num_frames * (int)spf;

  tmpbuf = _GD_Alloc(D, GD_UINT64, ns);
  readbuf = _GD_Alloc(D, GD_UINT64, ns);

  if (tmpbuf == NULL || readbuf == NULL) {
    dreturn("%zi", 0);
    return 0;
  }

  memset(tmpbuf, 0, sizeof(uint64_t) * ns);
  memset(readbuf, 0, sizeof(uint64_t) * ns);

  _GD_ConvertType(D, data_in, data_type, (void*)tmpbuf, GD_UINT64, ns);

  /* first, READ the field in so that we can change the bits    */
  /* do not check error code, since the field may not exist yet */

#ifdef GETDATA_DEBUG
  fprintf(stdout,"DoBitOut:  reading in bitfield %s\n",E->in_fields[0]);
#endif

  _GD_DoField(D, E->e->entry[0], E->in_fields[0], first_frame, first_samp, 0,
      ns, GD_UINT64, readbuf);

  /* error encountered, abort */
  if (D->error != GD_E_OK) {
    dreturn("%zi", 0);
    return 0;
  }

  /* now go through and set the correct bits in each field value */
  for (i = 0; i < ns; i++)
    readbuf[i] = (readbuf[i] & ~(mask << E->bitnum)) |
      (tmpbuf[i] & mask) << E->bitnum;

  /* write the modified data out */
  n_wrote = _GD_DoFieldOut(D, E->e->entry[0], E->in_fields[0], first_frame,
      first_samp, num_frames, num_samp, GD_UINT64, (void*)readbuf);

  free(readbuf);
  free(tmpbuf);

  dreturn("%zi", n_wrote);
  return n_wrote;
}

static size_t _GD_DoPhaseOut(DIRFILE* D, gd_entry_t *E,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t data_type, const void *data_in)
{
  size_t n_wrote;

  dtrace("%p, %p, %lli, %lli, %zi, %zi, 0x%x, %p", D, E, first_frame,
      first_samp, num_frames, num_samp, data_type, data_in);

  if (E->e->entry[0] == NULL) {
    E->e->entry[0] = _GD_FindField(D, E->in_fields[0], NULL);

    if (E->e->entry[0] == NULL) {
      _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, E->in_fields[0]);
      dreturn("%zi", 0);
      return 0;
    }

    /* scalar entries not allowed */
    if (E->e->entry[0]->field_type & GD_SCALAR_ENTRY) {
      _GD_SetError(D, GD_E_DIMENSION, 0, E->field, 0, E->e->entry[0]->field);
      dreturn("%zi", 0);
      return 0;
    }
  }

  n_wrote = _GD_DoFieldOut(D, E->e->entry[0], E->in_fields[0], first_frame,
      first_samp + E->shift, num_frames, num_samp, data_type, data_in);

  dreturn("%zi", n_wrote);

  return n_wrote;
}

static size_t _GD_DoConstOut(DIRFILE* D, gd_entry_t *E, gd_type_t data_type,
    const void *data_in)
{
  dtrace("%p, %p, 0x%x, %p", D, E, data_type, data_in);

  if (E->const_type & GD_SIGNED)
    _GD_ConvertType(D, data_in, data_type, &E->e->iconst, GD_INT64, 1);
  else if (E->const_type & GD_IEEE754)
    _GD_ConvertType(D, data_in, data_type, &E->e->dconst, GD_FLOAT64, 1);
  else
    _GD_ConvertType(D, data_in, data_type, &E->e->uconst, GD_UINT64, 1);

  if (D->error) { /* bad input type */
    dreturn("%zi", 0);
    return 0;
  }

  D->include_list[E->fragment_index].modified = 1;

  dreturn("%i", 1);
  return 1;
}

static size_t _GD_DoStringOut(DIRFILE* D, gd_entry_t *E, const void *data_in)
{
  dtrace("%p, %p, %p", D, E, data_in);

  free(E->e->string);
  E->e->string = strdup(data_in);
  if (E->e->string == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }
  D->include_list[E->fragment_index].modified = 1;

  dreturn("%i", strlen(E->e->string) + 1);
  return strlen(E->e->string) + 1;
}

size_t _GD_DoFieldOut(DIRFILE *D, gd_entry_t* E, const char *field_code,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t data_type, const void *data_in)
{
  size_t n_wrote = 0;

  dtrace("%p, %p, \"%s\", %lli, %lli, %zi, %zi, 0x%x, %p", D, E, field_code,
      first_frame, first_samp, num_frames, num_samp, data_type, data_in);

  if (++D->recurse_level >= GD_MAX_RECURSE_LEVEL) {
    _GD_SetError(D, GD_E_RECURSE_LEVEL, 0, NULL, 0, field_code);
    D->recurse_level--;
    dreturn("%zi", 0);
    return 0;
  }

  if (!E->e->calculated)
    _GD_CalculateEntry(D, E);

  if (D->error) {
    dreturn("%i", 0);
    return 0;
  }

  switch (E->field_type) {
    case GD_RAW_ENTRY:
      n_wrote = _GD_DoRawOut(D, E, first_frame, first_samp, num_frames,
          num_samp, data_type, data_in);
      break;
    case GD_LINTERP_ENTRY:
      n_wrote = _GD_DoLinterpOut(D, E, first_frame, first_samp, num_frames,
          num_samp, data_type, data_in);
      break;
    case GD_LINCOM_ENTRY:
      n_wrote = _GD_DoLincomOut(D, E, first_frame, first_samp, num_frames,
          num_samp, data_type, data_in);
      break;
    case GD_BIT_ENTRY:
      n_wrote = _GD_DoBitOut(D, E, first_frame, first_samp, num_frames,
          num_samp, data_type, data_in);
      break;
    case GD_MULTIPLY_ENTRY:
    case GD_INDEX_ENTRY:
      _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_PUT, NULL, 0, field_code);
      break;
    case GD_PHASE_ENTRY:
      n_wrote = _GD_DoPhaseOut(D, E, first_frame, first_samp, num_frames,
          num_samp, data_type, data_in);
      break;
    case GD_CONST_ENTRY:
      n_wrote = _GD_DoConstOut(D, E, data_type, data_in);
      break;
    case GD_STRING_ENTRY:
      n_wrote = _GD_DoStringOut(D, E, data_in);
      break;
    case GD_NO_ENTRY:
      _GD_InternalError(D);
      break;
  }

  D->recurse_level--;
  dreturn("%zi", n_wrote);
  return n_wrote;
}

/* this function is little more than a public boilerplate for _GD_DoFieldOut */
size_t putdata64(DIRFILE* D, const char *field_code, off64_t first_frame,
    off64_t first_samp, size_t num_frames, size_t num_samp, gd_type_t data_type,
    const void *data_in)
{
  size_t n_wrote = 0;
  gd_entry_t *entry;

  dtrace("%p, \"%s\", %lli, %lli, %zi, %zi, 0x%x, %p", D, field_code,
      first_frame, first_samp, num_frames, num_samp, data_type, data_in);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%zi", 0);
    return 0;
  }

  if ((D->flags & GD_ACCMODE) != GD_RDWR) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%zi", 0);
    return 0;
  }

  _GD_ClearError(D);

  first_frame -= D->frame_offset;

  entry = _GD_FindField(D, field_code, NULL);

  if (entry == NULL)
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
  else if (entry->field_type & GD_SCALAR_ENTRY)
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
  else 
    n_wrote = _GD_DoFieldOut(D, entry, field_code, first_frame, first_samp,
        num_frames, num_samp, data_type, data_in);

  dreturn("%zi", n_wrote);
  return n_wrote;
}

/* 32(ish)-bit wrapper for the 64-bit version, when needed */
size_t putdata(DIRFILE* D, const char *field_code, off_t first_frame,
    off_t first_samp, size_t num_frames, size_t num_samp, gd_type_t data_type,
    const void *data_in)
{
  return putdata64(D, field_code, first_frame, first_samp, num_frames, num_samp,
      data_type, data_in);
}
/* vim: ts=2 sw=2 et
*/
