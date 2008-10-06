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
#include <inttypes.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#endif

/* The following has been extracted from internal.cpp from kjs */

/*
 * For systems without NAN, this is a NAN in IEEE double format.
 */

#if !defined(NAN)
static __attribute__ ((__const__)) double __NAN()
{
  /* work around some strict alignment requirements
     for double variables on some architectures (e.g. PA-RISC) */
  typedef union { unsigned char b[8]; double d; } nan_t;
#ifdef WORDS_BIGENDIAN
  static const nan_t NaN_Bytes = { { 0x7f, 0xf8, 0, 0, 0, 0, 0, 0 } };
#elif defined(arm)
  static const nan_t NaN_Bytes = { { 0, 0, 0xf8, 0x7f, 0, 0, 0, 0 } };
#else
  static const nan_t NaN_Bytes = { { 0, 0, 0, 0, 0, 0, 0xf8, 0x7f } };
#endif

  const double NaN = NaN_Bytes.d;
  return NaN;
}
#define NAN __NAN()
#endif /* !defined(NAN) */

/* encoding schemas */
const struct encoding_t encode[] = {
  { GD_UNENCODED,    "",
    &_GD_RawOpen,
    &_GD_RawSeek,
    &_GD_RawRead,
    &_GD_RawSize, &_GD_RawWrite, &_GD_RawSync, &_GD_RawClose },
  { GD_TEXT_ENCODED, ".txt", &_GD_AsciiOpen, &_GD_AsciiSeek, &_GD_AsciiRead,
    &_GD_AsciiSize, &_GD_AsciiWrite, &_GD_AsciiSync, &_GD_AsciiClose },
#ifdef USE_SLIMLIB
  { GD_SLIM_ENCODED, ".slm", &_GD_SlimOpen, &_GD_SlimSeek, &_GD_SlimRead,
    &_GD_SlimSize, NULL, NULL, &_GD_SlimClose },
#else
  { GD_SLIM_ENCODED, ".slm", NULL, NULL, NULL, NULL, NULL, NULL, NULL },
#endif
  { GD_ENC_UNSUPPORTED, "",  NULL, NULL, NULL, NULL, NULL, NULL, NULL },
};

/* _GD_FillFileFrame: fill dataout with frame indices
*/
static void _GD_FillFileFrame(void *dataout, gd_type_t rtype, off64_t s0,
    size_t n)
{
  size_t i;

  dtrace("%p, 0x%x, %lli, %zi", dataout, rtype, s0, n);

  switch (rtype) {
    case GD_INT8:
      for (i = 0; i < n; i++)
        ((int8_t*)dataout)[i] = (int8_t)(i + s0);
      break;
    case GD_UINT8:
      for (i = 0; i < n; i++)
        ((uint8_t*)dataout)[i] = (uint8_t)(i + s0);
      break;
    case GD_INT16:
      for (i = 0; i < n; i++)
        ((int16_t*)dataout)[i] = (int16_t)(i + s0);
      break;
    case GD_UINT16:
      for (i = 0; i < n; i++)
        ((uint16_t*)dataout)[i] = (uint16_t)(i + s0);
      break;
    case GD_INT32:
      for (i = 0; i < n; i++)
        ((uint32_t*)dataout)[i] = (int32_t)(i + s0);
      break;
    case GD_UINT32:
      for (i = 0; i < n; i++)
        ((uint32_t*)dataout)[i] = (uint32_t)(i + s0);
      break;
    case GD_INT64:
      for (i = 0; i < n; i++)
        ((int64_t*)dataout)[i] = (int64_t)(i + s0);
      break;
    case GD_UINT64:
      for (i = 0; i < n; i++)
        ((uint64_t*)dataout)[i] = (uint64_t)(i + s0);
      break;
    case GD_FLOAT:
      for (i = 0; i < n; i++)
        ((float*)dataout)[i] = (float)(i + s0);
      break;
    case GD_DOUBLE:
      for (i = 0; i < n; i++)
        ((double*)dataout)[i] = (double)(i + s0);
      break;
    default:
      break;
  }

  dreturnvoid();
}

/* _GD_FillZero: fill data buffer with zero/NaN of the appropriate type.  Used
 *       if s0 < 0.  Fills up to position 0 or ns + s0, whichever is less
 */
static int _GD_FillZero(void *databuffer, gd_type_t type, off64_t s0, size_t ns)
{
  size_t i, nz = ns;
  const double NaN = NAN;

  dtrace("%p, 0x%x, %lli, %zi", databuffer, type, s0, ns);

  if (s0 >= 0) {
    dreturn("%i", 0);
    return 0;
  }

  if (s0 + ns > 0)
    nz = -s0;

  if (type & GD_IEEE754) {
    if (type == GD_FLOAT32)
      for (i = 0; i < nz; ++i)
        *((float*)databuffer + i) = (float)NaN;
    else
      for (i = 0; i < nz; ++i)
        *((double*)databuffer + i) = (double)NaN;
  } else 
    memset(databuffer, 0, nz * GD_SIZE(type));

  dreturn("%i", nz);

  return (nz);
}

/* Figure out the encoding scheme */
unsigned int _GD_ResolveEncoding(const char* name, unsigned int scheme,
    struct _gd_private_entry *e)
{
  char candidate[FILENAME_MAX];
  char* ptr;
  int i, len = strlen(name);
  struct stat64 statbuf;

  dtrace("\"%s\", 0x%08x, %p", name, scheme, e);

  strcpy(candidate, name);
  ptr = candidate + len;
  len = FILENAME_MAX - len;

  for (i = 0; encode[i].scheme != GD_ENC_UNSUPPORTED; i++) {
    if (scheme == 0 || scheme == encode[i].scheme) {
      strcpy(ptr, encode[i].ext);

      if (stat64(candidate, &statbuf) == 0) 
        if (S_ISREG(statbuf.st_mode)) {
          if (e != NULL)
            e->encoding = i;
          dreturn("%08x", encode[i].scheme);
          return encode[i].scheme;
        }
    }
  }

  if (scheme != 0 && e != NULL) {
    for (i = 0; encode[i].scheme != GD_AUTO_ENCODED; i++)
      if (scheme == encode[i].scheme) {
        e->encoding = i;
        dreturn("0x%08x", encode[i].scheme);
        return encode[i].scheme;;
      }
  }

  dreturn("%08x", GD_AUTO_ENCODED);
  return GD_AUTO_ENCODED;
}

/* _GD_DoRaw:  Read from a raw.  Returns number of samples read.
*/
static size_t _GD_DoRaw(DIRFILE *D, gd_entry_t *R,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t return_type, void *data_out)
{
  off64_t s0;
  size_t ns, n_read = 0;
  ssize_t samples_read;
  char datafilename[FILENAME_MAX];
  char *databuffer;

  dtrace("%p, %p, %lli, %lli, %zi, %zi, 0x%x, %p)", D, R, first_frame,
      first_samp, num_frames, num_samp, return_type, data_out);

  s0 = first_samp + first_frame * R->spf;
  ns = num_samp + num_frames * R->spf;

  databuffer = malloc(ns * R->size);
  if (databuffer == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%zi", 0);
    return 0;
  }

  if (s0 < 0) {
    n_read = _GD_FillZero(databuffer, R->data_type, s0, ns);
    ns -= n_read;
    s0 = 0;
  }

  if (ns > 0) {
    /** open the file (and cache the fp) if it hasn't been opened yet. */
    if (R->e->fp < 0) {
      snprintf(datafilename, FILENAME_MAX, "%s/%s", D->name, R->e->file);

      /* Figure out the dirfile encoding type, if required */
      if ((D->flags & GD_ENCODING) == GD_AUTO_ENCODED)
        D->flags = (D->flags & ~GD_ENCODING) |
          _GD_ResolveEncoding(datafilename, 0, R->e);

      /* If the encoding is still unknown, none of the candidate files exist;
       * complain and return */
      if ((D->flags & GD_ENCODING) == GD_AUTO_ENCODED) {
        _GD_SetError(D, GD_E_RAW_IO, 0, datafilename, ENOENT, NULL);
        dreturn("%zi", 0);
        return 0;
      }

      /* Figure out the encoding subtype, if required */
      if (R->e->encoding == GD_ENC_UNKNOWN)
        _GD_ResolveEncoding(datafilename, D->flags & GD_ENCODING, R->e);

      if (encode[R->e->encoding].open == NULL) {
        _GD_SetError(D, GD_E_UNSUPPORTED, 0, NULL, 0, NULL);
        dreturn("%zi", 0);
        return 0;
      } else if ((*encode[R->e->encoding].open)(R->e, datafilename,
            D->flags & GD_ACCMODE, 0))
      {
        _GD_SetError(D, GD_E_RAW_IO, 0, datafilename, errno, NULL);
        dreturn("%zi", 0);
        return 0;
      }
    }

    if (encode[R->e->encoding].seek == NULL) {
      _GD_SetError(D, GD_E_UNSUPPORTED, 0, NULL, 0, NULL);
      dreturn("%zi", 0);
      return 0;
    }

    (*encode[R->e->encoding].seek)(R->e, s0, R->data_type, 0);

    if (encode[R->e->encoding].read == NULL) {
      _GD_SetError(D, GD_E_UNSUPPORTED, 0, NULL, 0, NULL);
      dreturn("%zi", 0);
      return 0;
    }

    samples_read = (*encode[R->e->encoding].read)(R->e,
        databuffer + n_read * R->size, R->data_type, ns);

    if (samples_read == -1) {
      _GD_SetError(D, GD_E_RAW_IO, 0, datafilename, errno, NULL);
      free(databuffer);
      dreturn("%zi", 0);
      return 0;
    }

    samples_read /= R->size;

    if (D->flags &
#ifdef WORDS_BIGENDIAN
        GD_LITTLE_ENDIAN
#else
        GD_BIG_ENDIAN
#endif
       )
      _GD_FixEndianness(databuffer + n_read * R->size, R->size, samples_read);

    n_read += samples_read;
  }
  _GD_ConvertType(D, databuffer, R->data_type, data_out, return_type, n_read);

  free(databuffer);

  dreturn("%zi", (D->error == GD_E_OK) ? n_read : 0);
  return (D->error == GD_E_OK) ? n_read : 0;
}

/* _GD_AddData: add vector B to vector A.  B is unchanged
*/
static void _GD_AddData(DIRFILE* D, void *A, int spfA, void *B, int spfB,
    gd_type_t type, int n)
{
  int i;

  switch (type) {
    case GD_NULL: /* null read */
      break;
    case GD_INT8:
      for (i = 0; i < n; i++)
        ((int8_t*)A)[i] = (int8_t)(((int8_t*)A)[i] + (((int8_t*)B)[i *
            spfB / spfA]));
      break;
    case GD_UINT8:
      for (i = 0; i < n; i++)
        ((uint8_t*)A)[i] = (uint8_t)(((uint8_t*)A)[i] + (((uint8_t*)B)[i *
            spfB / spfA]));
      break;
    case GD_INT16:
      for (i = 0; i < n; i++)
        ((int16_t*)A)[i] = (int16_t)(((int16_t*)A)[i] + (((int16_t*)B)[i *
            spfB / spfA]));
      break;
    case GD_UINT16:
      for (i = 0; i < n; i++)
        ((uint16_t*)A)[i] = (uint16_t)(((uint16_t*)A)[i] + (((uint16_t*)B)[i *
            spfB / spfA]));
      break;
    case GD_INT32:
      for (i = 0; i < n; i++)
        ((int32_t*)A)[i] = (int32_t)(((int32_t*)A)[i] + (((int32_t*)B)[i *
            spfB / spfA]));
      break;
    case GD_UINT32:
      for (i = 0; i < n; i++)
        ((uint32_t*)A)[i] = (uint32_t)(((uint32_t*)A)[i] + (((uint32_t*)B)[i *
            spfB / spfA]));
      break;
    case GD_INT64:
      for (i = 0; i < n; i++)
        ((int64_t*)A)[i] = (int64_t)(((int64_t*)A)[i] + (((int64_t*)B)[i *
            spfB / spfA]));
      break;
    case GD_UINT64:
      for (i = 0; i < n; i++)
        ((uint64_t*)A)[i] = (uint64_t)(((uint64_t*)A)[i] + (((uint64_t*)B)[i *
            spfB / spfA]));
      break;
    case GD_FLOAT:
      for (i = 0; i < n; i++)
        ((float*)A)[i] += ((float*)B)[i * spfB / spfA];
      break;
    case GD_DOUBLE:
      for (i = 0; i < n; i++)
        ((double*)A)[i] += ((double*)B)[i * spfB / spfA];
      break;
    default:
      _GD_SetError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
      return;
  }
}

/* MultiplyData: Multiply A by B.  B is unchanged.
*/
static void _GD_MultiplyData(DIRFILE* D, void *A, unsigned int spfA, void *B,
    unsigned int spfB, gd_type_t type, int n)
{
  int i;

  switch (type) {
    case GD_NULL: /* null read */
      break;
    case GD_INT8:
      for (i = 0; i < n; i++)
        ((int8_t*)A)[i] = (int8_t)(((int8_t*)A)[i] * (((int8_t*)B)[i * spfB /
            spfA]));
      break;
    case GD_UINT8:
      for (i = 0; i < n; i++)
        ((uint8_t*)A)[i] = (uint8_t)(((uint8_t*)A)[i] * (((uint8_t*)B)[i * spfB
            / spfA]));
      break;
    case GD_INT16:
      for (i = 0; i < n; i++)
        ((int16_t*)A)[i] = (int16_t)(((int16_t*)A)[i] * (((int16_t*)B)[i * spfB
            / spfA]));
      break;
    case GD_UINT16:
      for (i = 0; i < n; i++)
        ((uint16_t*)A)[i] = (uint16_t)(((uint16_t*)A)[i] * (((uint16_t*)B)[i *
            spfB / spfA]));
      break;
    case GD_INT32:
      for (i = 0; i < n; i++)
        ((int32_t*)A)[i] = (int32_t)(((int32_t*)A)[i] * (((int32_t*)B)[i *
            spfB / spfA]));
      break;
    case GD_UINT32:
      for (i = 0; i < n; i++)
        ((uint32_t*)A)[i] = (uint32_t)(((uint32_t*)A)[i] * (((uint32_t*)B)[i *
            spfB / spfA]));
      break;
    case GD_INT64:
      for (i = 0; i < n; i++)
        ((int64_t*)A)[i] = (int64_t)(((int64_t*)A)[i] * (((int64_t*)B)[i *
            spfB / spfA]));
      break;
    case GD_UINT64:
      for (i = 0; i < n; i++)
        ((uint64_t*)A)[i] = (uint64_t)(((uint64_t*)A)[i] * (((uint64_t*)B)[i *
            spfB / spfA]));
      break;
    case GD_FLOAT:
      for (i = 0; i < n; i++)
        ((float*)A)[i] *= ((float*)B)[i * spfB / spfA];
      break;
    case GD_DOUBLE:
      for (i = 0; i < n; i++)
        ((double*)A)[i] *= ((double*)B)[i * spfB / spfA];
      break;
    default:
      _GD_SetError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
      return;
  }
}

/* _GD_DoLincom:  Read from a lincom.  Returns number of samples read.
*/
static size_t _GD_DoLincom(DIRFILE *D, gd_entry_t *L,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t return_type, void *data_out)
{
  void *tmpbuf;
  int i;
  int spf1, spf2;
  size_t n_read, n_read2, num_samp2;
  off64_t first_samp2;

  if (L->e->entry[0] == NULL) {
    L->e->entry[0] = _GD_GetEntry(D, L->in_fields[0]);
    
    if (D->error != GD_E_OK)
      return 0;
  }

  spf1 = _GD_GetSPF(D, L->e->entry[0], L->in_fields[0]);
  if (D->error != GD_E_OK)
    return 0;

  /* read and scale the first field and record the number of samples
   * returned */
  n_read = _GD_DoField(D, L->e->entry[0], L->in_fields[0], first_frame,
      first_samp, num_frames, num_samp, return_type, data_out);

  if (D->error != GD_E_OK)
    return 0;

  /* Nothing to lincomise */
  if (n_read == 0)
    return 0;

  _GD_ScaleData(D, data_out, return_type, n_read, L->m[0], L->b[0]);

  if (L->n_fields > 1) {
    for (i = 1; i < L->n_fields; i++) {
      /* Resolve the next field, if needed */
      if (L->e->entry[i] == NULL) {
        L->e->entry[i] = _GD_GetEntry(D, L->in_fields[i]);

        if (D->error != GD_E_OK)
          return 0;
      }

      /* find the samples per frame of the next field */
      spf2 = _GD_GetSPF(D, L->e->entry[i], L->in_fields[i]);
      if (D->error != GD_E_OK)
        return 0;

      /* calculate the first sample and number of samples to read of the
       * next field */
      num_samp2 = (int)ceil((double)n_read * spf2 / spf1);
      first_samp2 = (first_frame * spf2 + first_samp * spf2 / spf1);

      /* Allocate a temporary buffer for the next field */
      tmpbuf = _GD_Alloc(D, return_type, num_samp2);

      if (D->error != GD_E_OK)
        return 0;

      /* read the next field */
      n_read2 = _GD_DoField(D, L->e->entry[i], L->in_fields[i], 0, first_samp2,
          0, num_samp2, return_type, tmpbuf);

      if (D->error != GD_E_OK) {
        free(tmpbuf);
        return 0;
      }

      _GD_ScaleData(D, tmpbuf, return_type, n_read2, L->m[i], L->b[i]);

      if (D->error != GD_E_OK) {
        free(tmpbuf);
        return 0;
      }

      if (n_read2 > 0 && n_read2 * spf1 != n_read * spf2)
        n_read = n_read2 * spf1 / spf2;

      _GD_AddData(D, data_out, spf1, tmpbuf, spf2, return_type, n_read);

      free(tmpbuf);
    }
  }

  return n_read;
}

/* _GD_DoMultiply:  Read from a multiply.  Returns number of samples read.
*/
static size_t _GD_DoMultiply(DIRFILE *D, gd_entry_t* M,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t return_type, void *data_out)
{
  void *tmpbuf;
  int spf1, spf2;
  size_t n_read, n_read2, num_samp2;
  off64_t first_samp2;

  /* find the samples per frame of the first field */
  if (M->e->entry[0] == NULL) {
    M->e->entry[0] = _GD_GetEntry(D, M->in_fields[0]);

    if (D->error != GD_E_OK)
      return 0;
  }

  spf1 = _GD_GetSPF(D, M->e->entry[0], M->in_fields[0]);
  if (D->error != GD_E_OK)
    return 0;

  /* read the first field and record the number of samples
   * returned */
  n_read = _GD_DoField(D, M->e->entry[0], M->in_fields[0], first_frame,
      first_samp, num_frames, num_samp, return_type, data_out);

  if (D->error != GD_E_OK)
    return 0;

  /* Nothing to multiply */
  if (n_read == 0)
    return 0;

  /* find the samples per frame of the second field */
  if (M->e->entry[1] == NULL) {
    M->e->entry[1] = _GD_GetEntry(D, M->in_fields[1]);

    if (D->error != GD_E_OK)
      return 0;
  }

  spf2 = _GD_GetSPF(D, M->e->entry[1], M->in_fields[1]);
  if (D->error != GD_E_OK)
    return 0;

  /* calculate the first sample and number of samples to read of the
   * second field */
  num_samp2 = (int)ceil((double)n_read * spf2 / spf1);
  first_samp2 = (first_frame * spf2 + first_samp * spf2 / spf1);

  /* Allocate a temporary buffer for the second field */
  tmpbuf = _GD_Alloc(D, return_type, num_samp2);

  if (D->error != GD_E_OK)
    return 0;

  /* read the second field */
  n_read2 = _GD_DoField(D, M->e->entry[1], M->in_fields[1], 0, first_samp2, 0,
      num_samp2, return_type, tmpbuf);
  if (D->error != GD_E_OK) {
    free(tmpbuf);
    return 0;
  }

  if (n_read2 > 0 && n_read2 * spf1 < n_read * spf2)
    n_read = n_read2 * spf1 / spf2;

  _GD_MultiplyData(D, data_out, spf1, tmpbuf, spf2, return_type, n_read);

  free(tmpbuf);

  return n_read;
}

/* _GD_DoBit:  Read from a bitfield.  Returns number of samples read.
*/
static size_t _GD_DoBit(DIRFILE *D, gd_entry_t *B,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t return_type, void *data_out)
{
  uint64_t *tmpbuf;
  size_t i;
  int spf;
  size_t ns;
  size_t n_read;

  const uint64_t mask = (B->numbits == 64) ? 0xffffffffffffffffULL :
    ((uint64_t)1 << B->numbits) - 1;

  if (B->e->entry[0] == NULL) {
    B->e->entry[0] = _GD_GetEntry(D, B->in_fields[0]);

    if (D->error != GD_E_OK)
      return 0;
  }

  spf = _GD_GetSPF(D, B->e->entry[0], B->in_fields[0]);

  if (D->error != GD_E_OK)
    return 0;

  ns = num_samp + num_frames * spf;
  tmpbuf = (uint64_t *)malloc(ns * sizeof(uint64_t));
  if (tmpbuf == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    return 0;
  }

  n_read = _GD_DoField(D, B->e->entry[0], B->in_fields[0], first_frame,
      first_samp, num_frames, num_samp, GD_UINT64, tmpbuf);

  if (D->error != GD_E_OK) {
    free(tmpbuf);
    return 0;
  }

  for (i = 0; i < n_read; i++)
    tmpbuf[i] = (tmpbuf[i] >> B->bitnum) & mask;

  _GD_ConvertType(D, tmpbuf, GD_UINT64, data_out, return_type, n_read);
  free(tmpbuf);

  return n_read;
}

/* _GD_DoPhase:  Read from a phase.  Returns number of samples read.
*/
static size_t _GD_DoPhase(DIRFILE *D, gd_entry_t *P,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t return_type, void *data_out)
{
  size_t n_read;

  if (P->e->entry[0] == NULL) {
    P->e->entry[0] = _GD_GetEntry(D, P->in_fields[0]);

    if (D->error != GD_E_OK)
      return 0;
  }

  n_read = _GD_DoField(D, P->e->entry[0], P->in_fields[0], first_frame,
      first_samp + P->shift, num_frames, num_samp, return_type, data_out);

  return n_read;
}

/* _GD_DoLinterp:  Read from a linterp.  Returns number of samples read.
*/
static size_t _GD_DoLinterp(DIRFILE *D, gd_entry_t* I,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t return_type, void *data_out)
{
  size_t n_read = 0;

  dtrace("%p, %p, %lli, %lli, %zi, %zi, 0x%x, %p [%p]", D, I, first_frame,
      first_samp, num_frames, num_samp, return_type, data_out, I->e->entry[0]);

  if (I->e->table_len < 0) {
    _GD_ReadLinterpFile(D, I);
    if (D->error != GD_E_OK) {
      dreturn("%zi", 0);
      return 0;
    }
  }

  dtrace("[%p]", I->e->entry[0]);

  if (I->e->entry[0] == NULL) {
    I->e->entry[0] = _GD_GetEntry(D, I->in_fields[0]);

    if (D->error != GD_E_OK)
      return 0;
  }

  dtrace("[%p]", I->e->entry[0]);

  n_read = _GD_DoField(D, I->e->entry[0], I->in_fields[0], first_frame,
      first_samp, num_frames, num_samp, return_type, data_out);

  if (D->error != GD_E_OK) {
    dreturn("%zi", 0);
    return 0;
  }

  _GD_LinterpData(D, data_out, return_type, n_read, I->e->x, I->e->y,
      I->e->table_len);

  dtrace("[%p]", I->e->entry[0]);

  dreturn("%zi", n_read);
  return n_read;
}

/* _GD_DoConst:  Read from a const.  Returns number of samples read (ie. 1).
*/
static size_t _GD_DoConst(DIRFILE *D, gd_entry_t *C, gd_type_t return_type,
    void *data_out)
{
  dtrace("%p, %p, 0x%x, %p", D, C, return_type, data_out);

  if (C->const_type & GD_SIGNED)
    _GD_ConvertType(D, &C->e->iconst, GD_INT64, data_out, return_type, 1);
  else if (C->const_type & GD_IEEE754)
    _GD_ConvertType(D, &C->e->dconst, GD_FLOAT64, data_out, return_type, 1);
  else
    _GD_ConvertType(D, &C->e->uconst, GD_UINT64, data_out, return_type, 1);

  if (D->error) { /* bad input type */
    dreturn("%i", 0);
    return 0;
  }

  dreturn("%i", 1);
  return 1;
}

/* _GD_DoString:  Read from a string.  Returns number of samples read (ie. 1).
*/
static size_t _GD_DoString(gd_entry_t *S, size_t num_samp, void *data_out)
{
  dtrace("%p, %zi, %p", S, num_samp, data_out);

  strncpy(data_out, S->e->string, num_samp); 

  dreturn("%i", 1);
  return 1;
}

/* _GD_DoField: Locate the field in the database and read it.
*/
size_t _GD_DoField(DIRFILE *D, gd_entry_t *entry, const char* field_code,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t return_type, void *data_out)
{
  size_t n_read = 0;

  dtrace("%p, %p, \"%s\", %lli, %lli, %zi, %zi, 0x%x, %p", D, entry, field_code,
      first_frame, first_samp, num_frames, num_samp, return_type, data_out);

  if (++D->recurse_level >= GD_MAX_RECURSE_LEVEL) {
    _GD_SetError(D, GD_E_RECURSE_LEVEL, 0, NULL, 0, field_code);
    D->recurse_level--;
    dreturn("%zi", 0);
    return 0;
  }

  /* if Asking for "FILEFRAM" or "INDEX", just return it */
  if (entry == NULL) {
    n_read = num_frames + num_samp;
    if (data_out != NULL) {
      _GD_FillFileFrame(data_out, return_type, first_frame + first_samp +
          D->frame_offset, n_read);
    }
    D->recurse_level--;
    dreturn("%zi", 0);
    return n_read;
  }

  switch (entry->field_type) {
    case GD_RAW_ENTRY:
      n_read = _GD_DoRaw(D, entry, first_frame, first_samp, num_frames,
          num_samp, return_type, data_out);
      break;
    case GD_LINTERP_ENTRY:
      n_read = _GD_DoLinterp(D, entry, first_frame, first_samp, num_frames,
          num_samp, return_type, data_out);
      break;
    case GD_LINCOM_ENTRY:
      n_read = _GD_DoLincom(D, entry, first_frame, first_samp, num_frames,
          num_samp, return_type, data_out);
      break;
    case GD_BIT_ENTRY:
      n_read = _GD_DoBit(D, entry, first_frame, first_samp, num_frames,
          num_samp, return_type, data_out);
      break;
    case GD_MULTIPLY_ENTRY:
      n_read = _GD_DoMultiply(D, entry, first_frame, first_samp, num_frames,
          num_samp, return_type, data_out);
      break;
    case GD_PHASE_ENTRY:
      n_read = _GD_DoPhase(D, entry, first_frame, first_samp, num_frames,
          num_samp, return_type, data_out);
      break;
    case GD_CONST_ENTRY:
      n_read = _GD_DoConst(D, entry, return_type, data_out);
      break;
    case GD_STRING_ENTRY:
      n_read = _GD_DoString(entry, num_samp, data_out);
      break;
    case GD_NO_ENTRY:
      /* Can't get here */
      _GD_InternalError(D);
      n_read = 0;
  }

  D->recurse_level--;
  dreturn("%zi", n_read);
  return n_read;
}

/* this function is little more than a public boilerplate for _GD_DoField */
size_t getdata64(DIRFILE* D, const char *field_code, off64_t first_frame,
    off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t return_type, void *data_out)
{
  size_t n_read;
  gd_entry_t* entry;

  dtrace("%p, \"%s\", %lli, %lli, %zi, %zi, 0x%x, %p", D, field_code,
      first_frame, first_samp, num_frames, num_samp, return_type, data_out);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%zi", 0);
    return 0;
  }

  _GD_ClearError(D);

  first_frame -= D->frame_offset;

  entry = _GD_GetEntry(D, field_code);
    
  if (D->error != GD_E_OK)
    n_read = 0;
  else if (entry && (entry->field_type == GD_CONST_ENTRY ||
        entry->field_type == GD_STRING_ENTRY)) {
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_GET, NULL, 0, field_code);
    n_read = 0;
  } else 
    n_read = _GD_DoField(D, entry, field_code, first_frame, first_samp,
        num_frames, num_samp, return_type, data_out);

  dreturn("%zi", n_read);
  return n_read;
}

/* 32(ish)-bit wrapper for the 64-bit version, when needed */
size_t getdata(DIRFILE* D, const char *field_code, off_t first_frame,
    off_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t return_type, void *data_out)
{
  return getdata64(D, field_code, first_frame, first_samp, num_frames, num_samp,
      return_type, data_out);
}
/* vim: ts=2 sw=2 et tw=80
*/
