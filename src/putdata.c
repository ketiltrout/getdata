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
 * You should have received a copy of the GNU General Public
 * License along with GetData; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 * 02111-1307 USA.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef STDC_HEADERS
#include <inttypes.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#endif

#include "internal.h"

static size_t _GD_DoFieldOut(DIRFILE* D, const char *field_code,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t data_type, const void *data_in);

static size_t _GD_DoRawOut(DIRFILE *D, gd_entry_t *R,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t data_type, const void *data_in)
{
  off64_t s0;
  size_t ns, n_wrote;
  char datafilename[FILENAME_MAX];
  void *databuffer;

  s0 = first_samp + first_frame * R->samples_per_frame;
  ns = num_samp + num_frames * R->samples_per_frame;

  if (s0 < 0) {
    _GD_SetError(D, GD_E_RANGE, 0, NULL, 0, NULL);
    return 0;
  }

#ifdef GETDATA_DEBUG
  fprintf(stdout,"DoRawOut:  file pointer for field %s = %d\n", R->field,
      R->fp);
#endif

  databuffer = _GD_Alloc(D, R->data_type, ns);

  _GD_ConvertType(D, data_in, data_type, databuffer, R->data_type, ns);

  if (D->error) { /* bad input type */
    free(databuffer);
    return 0;
  }

  if (D->flags &
#ifdef WORDS_BIGENDIAN
      GD_LITTLE_ENDIAN
#else
      GD_BIG_ENDIAN
#endif
     )
    _GD_FixEndianness(databuffer, R->size, ns);

  /* write data to file.  Note that if the first sample is beyond     */
  /* the current end of file, a gap will result (see lseek(2)) */

  if (R->fp < 0) {
    /* open file for reading / writing if not already opened */

    sprintf(datafilename, "%s/%s", D->name, R->file);

    R->fp = open(datafilename, O_RDWR | O_CREAT, 0666);
    if (R->fp < 0) {
      _GD_SetError(D, GD_E_RAW_IO, 0, datafilename, errno, NULL);
      return 0;
    }

#ifdef GETDATA_DEBUG
    fprintf(stdout, "DoRawOut:  opening file %s for writing\n",
        datafilename);
#endif
  }

  lseek(R->fp, s0 * (int)(R->size), SEEK_SET);
  n_wrote = ((int)write(R->fp, databuffer, (size_t)(R->size) * (size_t)ns)) /
    (R->size);

#ifdef GETDATA_DEBUG
  fprintf(stdout,"DoRawOut:  %d samples\n", n_wrote);
#endif

  free(databuffer);

  return n_wrote;
}

static size_t _GD_DoLinterpOut(DIRFILE* D, gd_entry_t *I,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t data_type, const void *data_in)
{
  int spf;
  size_t ns;
  size_t n_wrote;

  if (I->count < 0) {
    _GD_ReadLinterpFile(D, I);
    if (D->error != GD_E_OK)
      return 0;
  }

  /* Interpolate X(y) instead of Y(x) */

  D->recurse_level++;
  spf = _GD_GetSPF(I->in_fields[0], D);
  D->recurse_level--;
  ns = num_samp + num_frames * (int)spf;

  _GD_LinterpData(D, data_in, data_type, ns, I->y, I->x, I->count);

  if (D->error != GD_E_OK)
    return 0;

  D->recurse_level++;
  n_wrote = _GD_DoFieldOut(D, I->in_fields[0], first_frame, first_samp,
      num_frames, num_samp, data_type, data_in);
  D->recurse_level--;

  return n_wrote;
}

static size_t _GD_DoLincomOut(DIRFILE* D, gd_entry_t *L,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t data_type, const void *data_in)
{
  int spf;
  size_t ns, n_wrote;
  void* tmpbuf;

  /* we cannot write to LINCOM fields that are a linear combination */
  /* of more than one raw field (no way to know how to split data). */

  if (L->count > 1) {
    _GD_SetError(D, GD_E_BAD_PUT_FIELD, 0, NULL, 0, L->field);
    return 0;
  }

  D->recurse_level++;

  /* do the inverse scaling */

  D->recurse_level++;
  spf = _GD_GetSPF(L->in_fields[0], D);
  D->recurse_level--;
  ns = num_samp + num_frames * (int)spf;

  if (D->error != GD_E_OK)
    return 0;

  /* writeable copy */
  tmpbuf = _GD_Alloc(D, data_type, ns);

  if (tmpbuf == NULL)
    return 0;

  memcpy(tmpbuf, data_in, ns * GD_SIZE(data_type));

  _GD_ScaleData(D, tmpbuf, data_type, ns, 1 / L->m[0], -L->b[0] / L->m[0]);

  if (D->error != GD_E_OK)
    return 0;

  n_wrote = _GD_DoFieldOut(D, L->in_fields[0], first_frame, first_samp,
      num_frames, num_samp, data_type, tmpbuf);
  free(tmpbuf);

  D->recurse_level--;

  return n_wrote;
}

static size_t _GD_DoBitOut(DIRFILE* D, gd_entry_t *B,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t data_type, const void *data_in)
{
  uint64_t *tmpbuf;
  uint64_t *readbuf;
  size_t i, n_wrote;
  int spf;
  size_t ns;

  const uint64_t mask = (B->numbits == 64) ? 0xffffffffffffffffULL :
    ((uint64_t)1 << B->numbits) - 1;

#ifdef GETDATA_DEBUG
  fprintf(stdout,"DoBitOut:  bitnum = %d numbits = %d mask = %llx\n",
      B->bitnum, B->numbits, mask);
#endif

  D->recurse_level++;
  spf = _GD_GetSPF(B->in_fields[0], D);
  D->recurse_level--;

  if (D->error != GD_E_OK)
    return 0;

  ns = num_samp + num_frames * (int)spf;

  tmpbuf = _GD_Alloc(D, GD_UINT64, ns);
  readbuf = _GD_Alloc(D, GD_UINT64, ns);

  if (tmpbuf == NULL || readbuf == NULL)
    return 0;

  memset(tmpbuf, 0, sizeof(uint64_t) * ns);
  memset(readbuf, 0, sizeof(uint64_t) * ns);

  _GD_ConvertType(D, data_in, data_type, (void*)tmpbuf, GD_UINT64, ns);

  /* first, READ the field in so that we can change the bits    */
  /* do not check error code, since the field may not exist yet */

#ifdef GETDATA_DEBUG
  fprintf(stdout,"DoBitOut:  reading in bitfield %s\n",B->in_fields[0]);
#endif

  D->recurse_level++;

  _GD_DoField(D, B->in_fields[0], first_frame, first_samp, 0, ns, GD_UINT64,
      readbuf);

  D->recurse_level--;

  /* error encountered, abort */
  if (D->error != GD_E_OK)
    return 0;

  /* now go through and set the correct bits in each field value */
  for (i = 0; i < ns; i++)
    readbuf[i] = (readbuf[i] & ~(mask << B->bitnum)) |
      (tmpbuf[i] & mask) << B->bitnum;

  /* write the modified data out */
  n_wrote = _GD_DoFieldOut(D, B->in_fields[0], first_frame, first_samp,
      num_frames, num_samp, GD_UINT64, (void*)readbuf);

  free(readbuf);
  free(tmpbuf);
  return n_wrote;
}

static size_t _GD_DoPhaseOut(DIRFILE* D, gd_entry_t *P,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t data_type, const void *data_in)
{
  size_t n_wrote;

  D->recurse_level++;
  n_wrote = _GD_DoFieldOut(D, P->in_fields[0], first_frame, first_samp + P->shift,
      num_frames, num_samp, data_type, data_in);
  D->recurse_level--;

  return n_wrote;
}

static size_t _GD_DoFieldOut(DIRFILE *D, const char *field_code,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t data_type, const void *data_in)
{
  gd_entry_t* entry;

  if (D->recurse_level > 10) {
    _GD_SetError(D, GD_E_RECURSE_LEVEL, 0, NULL, 0, field_code);
    return 0;
  }

  /* Find the field */
  entry = _GD_FindField(D, field_code);

  if (entry == NULL) { /* No match */
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
    return 0;
  }

  switch (entry->field_type) {
    case GD_RAW_ENTRY:
      return _GD_DoRawOut(D, entry, first_frame, first_samp, num_frames,
          num_samp, data_type, data_in);
    case GD_LINTERP_ENTRY:
      return _GD_DoLinterpOut(D, entry, first_frame, first_samp, num_frames,
          num_samp, data_type, data_in);
    case GD_LINCOM_ENTRY:
      return _GD_DoLincomOut(D, entry, first_frame, first_samp, num_frames,
          num_samp, data_type, data_in);
    case GD_BIT_ENTRY:
      return _GD_DoBitOut(D, entry, first_frame, first_samp, num_frames,
          num_samp, data_type, data_in);
    case GD_MULTIPLY_ENTRY:
      _GD_SetError(D, GD_E_BAD_PUT_FIELD, 0, NULL, 0, field_code);
      return 0;
    case GD_PHASE_ENTRY:
      return _GD_DoPhaseOut(D, entry, first_frame, first_samp, num_frames,
          num_samp, data_type, data_in);
  }

  _GD_InternalError(D);
  return 0;
}

/* this function is little more than a public boilerplate for _GD_DoFieldOut */
size_t putdata64(DIRFILE* D, const char *field_code, off64_t first_frame,
    off64_t first_samp, size_t num_frames, size_t num_samp, gd_type_t data_type,
    const void *data_in)
{
  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    return 0;
  }

  if ((D->flags & GD_ACCMODE) != GD_RDWR) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    return 0;
  }

  _GD_ClearError(D);

  first_frame -= D->frame_offset;

  return _GD_DoFieldOut(D, field_code, first_frame, first_samp, num_frames,
      num_samp, data_type, data_in);
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
