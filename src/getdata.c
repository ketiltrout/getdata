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
 * The GNU C Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with the GNU C Library; if not, write to the Free
 * Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 * 02111-1307 USA.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef STDC_HEADERS
#include <inttypes.h>
#include <errno.h>
#include <fcntl.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#endif

#include "getdata_internal.h"

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

/* _GD_FillFileFrame: fill dataout with frame indicies
*/
static void _GD_FillFileFrame(void *dataout, char rtype, off64_t s0, size_t n)
{
  size_t i;

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
        ((uint32_t*)dataout)[i] = (i + s0);
      break;
    case GD_FLOAT:
      for (i = 0; i < n; i++)
        ((float*)dataout)[i] = (float)(i + s0);
      break;
    case GD_DOUBLE:
      for (i = 0; i < n; i++)
        ((double*)dataout)[i] = (double)(i + s0);
      break;
  }
}

/* _GD_ConvertType: copy data to output buffer while converting type.
*/
void _GD_ConvertType(DIRFILE* D, const void *data_in, gd_type_t in_type,
    void *data_out, gd_type_t out_type, size_t n)
{
  int i;

  _GD_ClearGetDataError(D);

  if (out_type == GD_NULL) /* null return type: don't return data */
    return;

  switch (in_type) {
    case GD_INT8:
      switch (out_type) {
        case GD_INT8:
          memcpy(data_out, data_in, n * sizeof(int8_t));
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((int8_t*)data_out)[i] = ((int8_t*)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t*)data_out)[i] = ((int8_t*)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t*)data_out)[i] = ((int8_t*)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t*)data_out)[i] = ((int8_t*)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t*)data_out)[i] = ((int8_t*)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t*)data_out)[i] = ((int8_t*)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t*)data_out)[i] = ((int8_t*)data_in)[i];
          return;
        case GD_FLOAT:
          for (i = 0; i < n; i++)
            ((float*)data_out)[i] = ((int8_t*)data_in)[i];
          return;
        case GD_DOUBLE:
          for (i = 0; i < n; i++)
            ((double*)data_out)[i] = ((int8_t*)data_in)[i];
          return;
        default:
          break;
      }
      break;
    case GD_UINT8:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t*)data_out)[i] = ((uint8_t*)data_in)[i];
          return;
        case GD_UINT8:
          memcpy(data_out, data_in, n * sizeof(uint8_t));
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t*)data_out)[i] = ((uint8_t*)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t*)data_out)[i] = ((uint8_t*)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t*)data_out)[i] = ((uint8_t*)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t*)data_out)[i] = ((uint8_t*)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t*)data_out)[i] = ((uint8_t*)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t*)data_out)[i] = ((uint8_t*)data_in)[i];
          return;
        case GD_FLOAT:
          for (i = 0; i < n; i++)
            ((float*)data_out)[i] = ((uint8_t*)data_in)[i];
          return;
        case GD_DOUBLE:
          for (i = 0; i < n; i++)
            ((double*)data_out)[i] = ((uint8_t*)data_in)[i];
          return;
        default:
          break;
      }
      break;
    case GD_INT16:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t*)data_out)[i] = ((int16_t*)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t*)data_out)[i] = ((int16_t*)data_in)[i];
          return;
        case GD_INT16:
          memcpy(data_out, data_in, n * sizeof(int16_t));
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t*)data_out)[i] = ((int16_t*)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t*)data_out)[i] = ((int16_t*)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t*)data_out)[i] = ((uint16_t*)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t*)data_out)[i] = ((int16_t*)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t*)data_out)[i] = ((uint16_t*)data_in)[i];
          return;
        case GD_FLOAT:
          for (i = 0; i < n; i++)
            ((float*)data_out)[i] = ((uint16_t*)data_in)[i];
          return;
        case GD_DOUBLE:
          for (i = 0; i < n; i++)
            ((double*)data_out)[i] = ((uint16_t*)data_in)[i];
          return;
        default:
          break;
      }
      break;
    case GD_UINT16:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t*)data_out)[i] = ((uint16_t*)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t*)data_out)[i] = ((uint16_t*)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t*)data_out)[i] = ((uint16_t*)data_in)[i];
          return;
        case GD_UINT16:
          memcpy(data_out, data_in, n * sizeof(uint16_t));
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t*)data_out)[i] = ((uint16_t*)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t*)data_out)[i] = ((uint16_t*)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t*)data_out)[i] = ((uint16_t*)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t*)data_out)[i] = ((uint16_t*)data_in)[i];
          return;
        case GD_FLOAT:
          for (i = 0; i < n; i++)
            ((float*)data_out)[i] = ((uint16_t*)data_in)[i];
          return;
        case GD_DOUBLE:
          for (i = 0; i < n; i++)
            ((double*)data_out)[i] = ((uint16_t*)data_in)[i];
          return;
        default:
          break;
      }
      break;
    case GD_INT32:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t*) data_out)[i]=((int32_t*)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t*) data_out)[i]=((int32_t*)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t*)data_out)[i]=((int32_t*)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t*)data_out)[i]=((int32_t*)data_in)[i];
          return;
        case GD_INT32:
          memcpy(data_out, data_in, n * sizeof(int32_t));
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t*)data_out)[i]=((int32_t*)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t*)data_out)[i]=((int32_t*)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t*)data_out)[i]=((int32_t*)data_in)[i];
          return;
        case GD_FLOAT:
          for (i = 0; i < n; i++)
            ((float*)data_out)[i]=((int32_t*)data_in)[i];
          return;
        case GD_DOUBLE:
          for (i = 0; i < n; i++)
            ((double*)data_out)[i]=((int32_t*)data_in)[i];
          return;
        default:
          break;
      }
      break;
    case GD_UINT32:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t*) data_out)[i]= ((uint32_t*)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t*) data_out)[i]= ((uint32_t*)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t*)data_out)[i]= ((uint32_t*)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t*)data_out)[i]= ((uint32_t*)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t*)data_out)[i]= ((uint32_t*)data_in)[i];
          return;
        case GD_UINT32:
          memcpy(data_out, data_in, n * sizeof(uint32_t));
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t*)data_out)[i]= ((uint32_t*)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t*)data_out)[i]= ((uint32_t*)data_in)[i];
          return;
        case GD_FLOAT:
          for (i = 0; i < n; i++)
            ((float*)data_out)[i]= ((uint32_t*)data_in)[i];
          return;
        case GD_DOUBLE:
          for (i = 0; i < n; i++)
            ((double*)data_out)[i]= ((uint32_t*)data_in)[i];
          return;
        default:
          break;
      }
      break;
    case GD_INT64:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t*) data_out)[i]=((int64_t*)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t*) data_out)[i]=((int64_t*)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t*)data_out)[i]=((int64_t*)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t*)data_out)[i]=((int64_t*)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t*)data_out)[i]=((int64_t*)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t*)data_out)[i]=((int64_t*)data_in)[i];
          return;
        case GD_INT64:
          memcpy(data_out, data_in, n * sizeof(int64_t));
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t*)data_out)[i]=((int64_t*)data_in)[i];
          return;
        case GD_FLOAT:
          for (i = 0; i < n; i++)
            ((float*)data_out)[i]=((int64_t*)data_in)[i];
          return;
        case GD_DOUBLE:
          for (i = 0; i < n; i++)
            ((double*)data_out)[i]=((int64_t*)data_in)[i];
          return;
        default:
          break;
      }
      break;
    case GD_UINT64:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t*) data_out)[i]= ((uint64_t*)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t*) data_out)[i]= ((uint64_t*)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t*)data_out)[i]= ((uint64_t*)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t*)data_out)[i]= ((uint64_t*)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t*)data_out)[i]= ((uint64_t*)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t*)data_out)[i]= ((uint64_t*)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t*)data_out)[i]= ((uint64_t*)data_in)[i];
          return;
        case GD_UINT64:
          memcpy(data_out, data_in, n * sizeof(uint64_t));
          return;
        case GD_FLOAT:
          for (i = 0; i < n; i++)
            ((float*)data_out)[i]= ((uint64_t*)data_in)[i];
          return;
        case GD_DOUBLE:
          for (i = 0; i < n; i++)
            ((double*)data_out)[i]= ((uint64_t*)data_in)[i];
          return;
        default:
          break;
      }
      break;
    case GD_FLOAT:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t*) data_out)[i]=((float*)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t*) data_out)[i]=((float*)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t*)data_out)[i]=((float*)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t*)data_out)[i]=((float*)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t*)data_out)[i]=((float*)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t*)data_out)[i]=((float*)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t*)data_out)[i]=((float*)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t*)data_out)[i]=((float*)data_in)[i];
          return;
        case GD_FLOAT:
          memcpy(data_out, data_in, n * sizeof(float));
          return;
        case GD_DOUBLE:
          for (i = 0; i < n; i++)
            ((double*)data_out)[i]=((float*)data_in)[i];
          return;
        default:
          break;
      }
      break;
    case GD_DOUBLE:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t*) data_out)[i]=((double*)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t*) data_out)[i]=((double*)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t*)data_out)[i]=((double*)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t*)data_out)[i]=((double*)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t*)data_out)[i]=((double*)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t*)data_out)[i]=((double*)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t*)data_out)[i]=((double*)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t*)data_out)[i]=((double*)data_in)[i];
          return;
        case GD_FLOAT:
          for (i = 0; i < n; i++)
            ((float*)data_out)[i]=((double*)data_in)[i];
          return;
        case GD_DOUBLE:
          memcpy(data_out, data_in, n * sizeof(double));
          return;
        default:
          break;
      }
      break;
    default:
      break;
  }

  _GD_SetGetDataError(D, GD_E_BAD_TYPE, in_type, NULL, 0, NULL);
}


/* _GD_FillZero: fill data buffer with zero/NaN of the appropriate type.  Used
 *       if s0 < 0.  Fills up to position 0 or ns + s0, whichever is less
 */
static int _GD_FillZero(void *databuffer, gd_type_t type, off64_t s0, size_t ns)
{
  size_t i, nz = ns;
  const double NaN = NAN;

  if (s0 >= 0)
    return 0;

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

  return (nz);
}

/* Binary search to find the field */
struct gd_entry_t* _GD_FindField(DIRFILE* D, const char* field_code)
{
  int i, c;
  int l = 0;
  int u = D->n_entries;

  while (l < u) {
    i = (l + u) / 2;
    c = strcmp(field_code, D->entries[i]->field);
    if (c < 0)
      u = i;
    else if (c > 0)
      l = i + 1;
    else
      return D->entries[i];
  }

  return NULL;
}

void _GD_FixEndianness(DIRFILE* D, char* databuffer, size_t size, size_t ns)
{
  size_t i;
  int j;
  char b;

#ifdef GETDATA_DEBUG
  printf("_GD_FixEndianness(%p, %p, %zi, %zi)\n", D, databuffer, size, ns);
#endif

  if (size == 1)
    return;

  for (i = 0; i < ns; ++i)
    for (j = 0; j < size / 2; ++j) {
      b = databuffer[size * (i + 1) - j - 1];
      databuffer[size * (i + 1) - j - 1] = databuffer[size * i + j];
      databuffer[size * i + j] = b;
    }
}

/* _GD_DoRaw:  Read from a raw.  Returns number of samples read.
*/
static size_t _GD_DoRaw(DIRFILE *D, struct RawEntryType *R,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t return_type, void *data_out)
{
  off64_t s0;
  size_t ns, n_read = 0;
  ssize_t samples_read;
  char datafilename[FILENAME_MAX];
  void *databuffer;

  s0 = first_samp + first_frame*R->samples_per_frame;
  ns = num_samp + num_frames*R->samples_per_frame;

#ifdef GETDATA_DEBUG
  printf("_GD_DoRaw(%p, %p, %lli, %lli, %zi, %zi, %i, %p) %i\n", D, R,
      first_frame, first_samp, num_frames, num_samp, return_type, data_out,
      R->fp);
#endif

  /** open the file (and cache the fp) if it hasn't been opened yet. */
  if (R->fp < 0) {
    snprintf(datafilename, FILENAME_MAX, "%s/%s", D->name, R->file);
    errno = 0;
    R->fp = open(datafilename, ((D->flags & GD_ACCMODE) == GD_RDWR) ? O_RDWR :
        O_RDONLY);
    if (R->fp < 0) {
      _GD_SetGetDataError(D, GD_E_RAW_IO, 0, NULL, 0, datafilename);
      return 0;
    }
  }

  databuffer = malloc(ns * R->size);
  if (databuffer == NULL) {
    _GD_SetGetDataError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    return 0;
  }

  if (s0 < 0) {
    n_read = _GD_FillZero(databuffer, R->data_type, s0, ns);
    ns -= n_read;
    s0 = 0;
  }

  if (ns > 0) {
    lseek64(R->fp, s0 * R->size, SEEK_SET);

    samples_read = read(R->fp, databuffer + n_read * R->size, ns * R->size);

    if (samples_read == -1) {
      _GD_SetGetDataError(D, GD_E_RAW_IO, 0, NULL, 0, datafilename);
      free(databuffer);
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
      _GD_FixEndianness(D, databuffer + n_read * R->size, R->size,
          samples_read);

    n_read += samples_read;
  }
  _GD_ConvertType(D, databuffer, R->data_type, data_out, return_type, n_read);

  free(databuffer);

  return (D->error == GD_E_OK) ? n_read : 0;
}


/* _GD_Alloc: allocate a buffer of the right type & size
*/
void* _GD_Alloc(DIRFILE* D, gd_type_t type, int n)
{
  assert(n > 0);

  _GD_ClearGetDataError(D);

  if (type == GD_NULL)
    return NULL;
  else if (GD_SIZE(type) == 0) {
    _GD_SetGetDataError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
    return NULL;
  }

  return malloc(n * GD_SIZE(type));
}

/* _GD_ScaleData: Compute data = data * m + b, for scalar m and b.
*/
void _GD_ScaleData(DIRFILE* D, void *data, gd_type_t type, int npts, double m,
    double b)
{
  int i;

  switch (type) {
    case GD_NULL:
      break;
    case GD_INT8:
      for (i = 0; i < npts; i++)
        ((int8_t*)data)[i] = ((double)((int8_t*)data)[i] * m + b);
      break;
    case GD_UINT8:
      for (i = 0; i < npts; i++)
        ((uint8_t*)data)[i] = ((double)((uint8_t*)data)[i] * m + b);
      break;
    case GD_INT16:
      for (i = 0; i < npts; i++)
        ((int16_t*)data)[i] = ((double)((int16_t*)data)[i] * m + b);
      break;
    case GD_UINT16:
      for (i = 0; i < npts; i++)
        ((uint16_t*)data)[i] = ((double)((uint16_t*)data)[i] * m + b);
      break;
    case GD_INT32:
      for (i = 0; i < npts; i++)
        ((int32_t*)data)[i] = ((double)((int32_t*)data)[i] * m + b);
      break;
    case GD_UINT32:
      for (i = 0; i < npts; i++)
        ((uint32_t*)data)[i] = ((double)((uint32_t*)data)[i] * m + b);
      break;
    case GD_INT64:
      for (i = 0; i < npts; i++)
        ((int64_t*)data)[i] = ((double)((int64_t*)data)[i] * m + b);
      break;
    case GD_UINT64:
      for (i = 0; i < npts; i++)
        ((uint64_t*)data)[i] = ((double)((uint64_t*)data)[i] * m + b);
      break;
    case GD_FLOAT:
      for (i = 0; i < npts; i++)
        ((float*)data)[i] = ((double)((float*)data)[i] * m + b);
      break;
    case GD_DOUBLE:
      for (i = 0; i < npts; i++)
        ((double*)data)[i] = ((double)((double*)data)[i] * m + b);
      break;
    default:
      _GD_SetGetDataError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
      return;
  }

  _GD_ClearGetDataError(D);
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
        ((int8_t*)A)[i] += ((int8_t*)B)[i * spfB / spfA];
      break;
    case GD_UINT8:
      for (i = 0; i < n; i++)
        ((uint8_t*)A)[i] += ((uint8_t*)B)[i * spfB / spfA];
      break;
    case GD_INT16:
      for (i = 0; i < n; i++)
        ((int16_t*)A)[i] += ((int16_t*)B)[i * spfB / spfA];
      break;
    case GD_UINT16:
      for (i = 0; i < n; i++)
        ((uint16_t*)A)[i] += ((uint16_t*)B)[i * spfB / spfA];
      break;
    case GD_INT32:
      for (i = 0; i < n; i++)
        ((int32_t*)A)[i] += ((int32_t*)B)[i * spfB / spfA];
      break;
    case GD_UINT32:
      for (i = 0; i < n; i++)
        ((uint32_t*)A)[i] += ((uint32_t*)B)[i * spfB / spfA];
      break;
    case GD_INT64:
      for (i = 0; i < n; i++)
        ((int64_t*)A)[i] += ((int64_t*)B)[i * spfB / spfA];
      break;
    case GD_UINT64:
      for (i = 0; i < n; i++)
        ((uint64_t*)A)[i] += ((uint64_t*)B)[i * spfB / spfA];
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
      _GD_SetGetDataError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
      return;
  }

  _GD_ClearGetDataError(D);
}

/* MultiplyData: Multiply A by B.  B is unchanged.
*/
static void _GD_MultiplyData(DIRFILE* D, void *A, int spfA, void *B, int spfB,
    gd_type_t type, int n)
{
  int i;

  switch (type) {
    case GD_NULL: /* null read */
      break;
    case GD_INT8:
      for (i = 0; i < n; i++)
        ((int8_t*)A)[i] *= ((int8_t*)B)[i * spfB / spfA];
      break;
    case GD_UINT8:
      for (i = 0; i < n; i++)
        ((uint8_t*)A)[i] *= ((uint8_t*)B)[i * spfB / spfA];
      break;
    case GD_INT16:
      for (i = 0; i < n; i++)
        ((int16_t*)A)[i] *= ((int16_t*)B)[i * spfB / spfA];
      break;
    case GD_UINT16:
      for (i = 0; i < n; i++)
        ((uint16_t*)A)[i] *= ((uint16_t*)B)[i * spfB / spfA];
      break;
    case GD_INT32:
      for (i = 0; i < n; i++)
        ((int32_t*)A)[i] *= ((int32_t*)B)[i * spfB / spfA];
      break;
    case GD_UINT32:
      for (i = 0; i < n; i++)
        ((uint32_t*)A)[i] *= ((uint32_t*)B)[i * spfB / spfA];
      break;
    case GD_INT64:
      for (i = 0; i < n; i++)
        ((int64_t*)A)[i] *= ((int64_t*)B)[i * spfB / spfA];
      break;
    case GD_UINT64:
      for (i = 0; i < n; i++)
        ((uint64_t*)A)[i] *= ((uint64_t*)B)[i * spfB / spfA];
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
      _GD_SetGetDataError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
      return;
  }

  _GD_ClearGetDataError(D);
}


/* _GD_DoLincom:  Read from a lincom.  Returns number of samples read.
*/
static size_t _GD_DoLincom(DIRFILE *D, struct LincomEntryType *L,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t return_type, void *data_out)
{
  void *tmpbuf;
  int i;
  int spf1, spf2;
  size_t n_read, n_read2, num_samp2;
  off64_t first_samp2;

  D->recurse_level++;
  spf1 = _GD_GetSPF(L->in_fields[0], D);
  if (D->error != GD_E_OK)
    return 0;

  /* read and scale the first field and record the number of samples
   * returned */
  n_read = _GD_DoField(D, L->in_fields[0], first_frame, first_samp,
      num_frames, num_samp, return_type, data_out);

  D->recurse_level--;

  if (D->error != GD_E_OK)
    return 0;

  /* Nothing to lincomise */
  if (n_read == 0)
    return 0;

  _GD_ScaleData(D, data_out, return_type, n_read, L->m[0], L->b[0]);

  if (L->n_infields > 1) {
    for (i = 1; i < L->n_infields; i++) {
      D->recurse_level++;

      /* find the samples per frame of the next field */
      spf2 = _GD_GetSPF(L->in_fields[i], D);
      if (D->error != GD_E_OK)
        return 1;

      /* calculate the first sample and number of samples to read of the
       * next field */
      num_samp2 = (int)ceil((double)n_read * spf2 / spf1);
      first_samp2 = (first_frame * spf2 + first_samp * spf2 / spf1);

      /* Allocate a temporary buffer for the next field */
      tmpbuf = _GD_Alloc(D, return_type, num_samp2);

      if (D->error != GD_E_OK)
        return 0;

      /* read the next field */
      n_read2 = _GD_DoField(D, L->in_fields[i], 0, first_samp2, 0, num_samp2,
          return_type, tmpbuf);
      D->recurse_level--;

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
static size_t _GD_DoMultiply(DIRFILE *D, struct MultiplyEntryType* M,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t return_type, void *data_out)
{
  void *tmpbuf;
  int spf1, spf2;
  size_t n_read, n_read2, num_samp2;
  off64_t first_samp2;

  D->recurse_level++;

  /* find the samples per frame of the first field */
  spf1 = _GD_GetSPF(M->in_fields[0], D);
  if (D->error != GD_E_OK)
    return 0;

  /* read the first field and record the number of samples
   * returned */
  n_read = _GD_DoField(D, M->in_fields[0], first_frame, first_samp,
      num_frames, num_samp, return_type, data_out);

  D->recurse_level--;

  if (D->error != GD_E_OK)
    return 0;

  /* Nothing to multiply */
  if (n_read == 0)
    return 0;

  D->recurse_level++;

  /* find the samples per frame of the second field */
  spf2 = _GD_GetSPF(M->in_fields[1], D);
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
  n_read2 = _GD_DoField(D, M->in_fields[1], 0, first_samp2, 0, num_samp2,
      return_type, tmpbuf);
  D->recurse_level--;
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
static size_t _GD_DoBit(DIRFILE *D, struct BitEntryType *B,
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

  /* if we got here, we found the field! */
  D->recurse_level++;
  spf = _GD_GetSPF(B->raw_field, D);
  D->recurse_level--;

  if (D->error != GD_E_OK)
    return 0;

  ns = num_samp + num_frames * spf;
  tmpbuf = (uint64_t *)malloc(ns * sizeof(uint64_t));
  if (tmpbuf == NULL) {
    _GD_SetGetDataError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    return 0;
  }

  D->recurse_level++;
  n_read = _GD_DoField(D, B->raw_field, first_frame, first_samp,
      num_frames, num_samp, GD_UINT64, tmpbuf);
  D->recurse_level--;

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
static size_t _GD_DoPhase(DIRFILE *D, struct PhaseEntryType *P,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t return_type, void *data_out)
{
  size_t n_read;

  D->recurse_level++;
  n_read = _GD_DoField(D, P->raw_field, first_frame, first_samp + P->shift,
      num_frames, num_samp, return_type, data_out);
  D->recurse_level--;

  return n_read;
}

/* _GD_MakeDummyLinterp: Make an empty linterp
*/
static void _GD_MakeDummyLinterp(DIRFILE* D, struct LinterpEntryType *E)
{
  E->n_interp = 2;
  E->x = (double *)malloc(2*sizeof(double));
  E->y = (double *)malloc(2*sizeof(double));

  if (E->x == NULL || E->y == NULL) {
    _GD_SetGetDataError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    return;
  }

  E->x[0] = 0;
  E->y[0] = 0;
  E->x[1] = 1;
  E->y[1] = 1;
}

/* _GD_ReadLinterpFile: Read in the linterp data for this field
*/
void _GD_ReadLinterpFile(DIRFILE* D, struct LinterpEntryType *E)
{
  FILE *fp;
  int i;
  char line[MAX_LINE_LENGTH];
  int linenum = 0;

  fp = fopen(E->linterp_file, "r");
  if (fp == NULL) {
    _GD_MakeDummyLinterp(D, E);
    _GD_SetGetDataError(D, GD_E_OPEN_LINFILE, GD_E_LINFILE_OPEN, NULL, 0,
        E->linterp_file);
    return;
  }

  /* first read the file to see how big it is */
  i = 0;
  while (_GD_GetLine(fp, line, &linenum))
    i++;

  if (i < 2) {
    _GD_MakeDummyLinterp(D, E);
    _GD_SetGetDataError(D, GD_E_OPEN_LINFILE, GD_E_LINFILE_LENGTH, NULL, 0,
        E->linterp_file);
    return;
  }

  E->n_interp = i;
  E->x = (double *)malloc(i * sizeof(double));
  E->y = (double *)malloc(i * sizeof(double));
  if (E->x == NULL || E->y == NULL) {
    _GD_SetGetDataError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    return;
  }

  /* now read in the data */
  rewind(fp);
  linenum = 0;
  for (i = 0; i < E->n_interp; i++) {
    _GD_GetLine(fp, line, &linenum);
    sscanf(line, "%lg %lg",&(E->x[i]), &(E->y[i]));
  }

  _GD_ClearGetDataError(D);
}

/* _GD_GetIndex: get LUT index.
*/
static int _GD_GetIndex(double x, double lx[], int idx, int n)
{
  /* Just linearly search - we're probably right to start    */
  /* increment until we are bigger */
  while ((idx < n - 2) && (x > lx[idx]))
    idx++;

  /* decrement until we are smaller */
  while ((idx > 0) && (x < lx[idx]))
    idx--;

  return idx;
}

/* _GD_LinterpData: calibrate data using lookup table lx and ly
*/
void _GD_LinterpData(DIRFILE* D, const void *data, gd_type_t type, int npts,
    double *lx, double *ly, int n_ln)
{
  int i, idx = 0;
  double x;

  switch (type) {
    case GD_NULL:
      break;
    case GD_INT8:
      for (i = 0; i < npts; i++) {
        x = ((int8_t *)data)[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((int8_t *)data)[i] = (int8_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_UINT8:
      for (i = 0; i < npts; i++) {
        x = ((uint8_t *)data)[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((uint8_t *)data)[i] = (uint8_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_INT16:
      for (i = 0; i < npts; i++) {
        x = ((int16_t *)data)[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((int16_t *)data)[i] = (int16_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_UINT16:
      for (i = 0; i < npts; i++) {
        x = ((uint16_t *)data)[i];
        idx = _GD_GetIndex(x, lx, idx,n_ln);
        ((uint16_t *)data)[i] = (uint16_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_INT32:
      for (i = 0; i < npts; i++) {
        x = ((int32_t *)data)[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((int32_t *)data)[i] = (int32_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_UINT32:
      for (i = 0; i < npts; i++) {
        x = ((uint32_t *)data)[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((uint32_t *)data)[i] = (uint32_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_INT64:
      for (i = 0; i < npts; i++) {
        x = ((int64_t *)data)[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((int64_t *)data)[i] = (int64_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_UINT64:
      for (i = 0; i < npts; i++) {
        x = ((uint64_t *)data)[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((uint64_t *)data)[i] = (uint64_t)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_FLOAT:
      for (i = 0; i < npts; i++) {
        x = ((float *)data)[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((float *)data)[i] = (float)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    case GD_DOUBLE:
      for (i = 0; i < npts; i++) {
        x = ((double *)data)[i];
        idx = _GD_GetIndex(x, lx, idx, n_ln);
        ((double *)data)[i] = (double)(ly[idx] + (ly[idx + 1] - ly[idx]) /
          (lx[idx + 1] - lx[idx]) * (x - lx[idx]));
      }
      break;
    default:
      _GD_SetGetDataError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
      return;
  }

  _GD_ClearGetDataError(D);
}

/* _GD_DoLinterp:  Read from a linterp.  Returns number of samples read.
*/
static size_t _GD_DoLinterp(DIRFILE *D, struct LinterpEntryType* I,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t return_type, void *data_out)
{
  size_t n_read = 0;

  if (I->n_interp < 0) {
    _GD_ReadLinterpFile(D, I);
    if (D->error != GD_E_OK)
      return 0;
  }

  D->recurse_level++;
  n_read = _GD_DoField(D, I->raw_field, first_frame, first_samp,
      num_frames, num_samp, return_type, data_out);
  D->recurse_level--;

  if (D->error != GD_E_OK)
    return 0;

  _GD_LinterpData(D, data_out, return_type, n_read, I->x, I->y, I->n_interp);

  return n_read;
}

/* _GD_DoField: Locate the field in the database and read it.
*/
size_t _GD_DoField(DIRFILE *D, const char *field_code, off64_t first_frame,
    off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t return_type, void *data_out)
{
  size_t n_read = 0;
  struct gd_entry_t* entry;

#ifdef GETDATA_DEBUG
  printf("_GD_DoField(%p, %s, %lli, %lli, %zi, %zi, %i, %p)\n", D, field_code,
      first_frame, first_samp, num_frames, num_samp, return_type, data_out);
#endif

  if (D->recurse_level >= GD_MAX_RECURSE_LEVEL) {
    _GD_SetGetDataError(D, GD_E_RECURSE_LEVEL, 0, NULL, 0, field_code);
    return 0;
  }

  /* if Asking for "FILEFRAM" or "INDEX", just return it */
  if ((strcmp(field_code,"FILEFRAM") == 0) ||
      (strcmp(field_code,"INDEX") == 0)) {
    n_read = num_frames + num_samp;
    if (data_out != NULL) {
      _GD_FillFileFrame(data_out, return_type, first_frame + first_samp +
          D->frame_offset, n_read);
    }
    _GD_ClearGetDataError(D);
    return n_read;
  }

  /* Find the field */
  entry = _GD_FindField(D, field_code);

  if (entry == NULL) { /* No match */
    _GD_SetGetDataError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
    return 0;
  }

  switch (entry->field_type) {
    case GD_RAW_ENTRY:
      return _GD_DoRaw(D, ENTRY(Raw, entry), first_frame, first_samp,
          num_frames, num_samp, return_type, data_out);
    case GD_LINTERP_ENTRY:
      return _GD_DoLinterp(D, ENTRY(Linterp, entry), first_frame, first_samp,
          num_frames, num_samp, return_type, data_out);
    case GD_LINCOM_ENTRY:
      return _GD_DoLincom(D, ENTRY(Lincom, entry), first_frame, first_samp,
          num_frames, num_samp, return_type, data_out);
    case GD_BIT_ENTRY:
      return _GD_DoBit(D, ENTRY(Bit, entry), first_frame, first_samp,
          num_frames, num_samp, return_type, data_out);
    case GD_MULTIPLY_ENTRY:
      return _GD_DoMultiply(D, ENTRY(Multiply, entry), first_frame, first_samp,
          num_frames, num_samp, return_type, data_out);
    case GD_PHASE_ENTRY:
      return _GD_DoPhase(D, ENTRY(Phase, entry), first_frame, first_samp,
          num_frames, num_samp, return_type, data_out);
  }

  /* Can't get here */
  _GD_SetGetDataError(D, GD_E_INTERNAL_ERROR, 0, __FILE__, __LINE__, NULL);
  return 0;
}

/* this function is little more than a public boilerplate for _GD_DoField */
size_t getdata64(DIRFILE* D, const char *field_code, off64_t first_frame,
    off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t return_type, void *data_out)
{
  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetGetDataError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    return 0;
  }

#ifdef GETDATA_DEBUG
  printf("getdata64(%p, %s, %llu, %llu, %zi, %zi, %i, %p)\n", D, field_code,
      first_frame, first_samp, num_frames, num_samp, return_type, data_out);
#endif

  _GD_ClearGetDataError(D);

  first_frame -= D->frame_offset;

  return _GD_DoField(D, field_code, first_frame, first_samp, num_frames,
      num_samp, return_type, data_out);
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
