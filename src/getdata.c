/* (C) 2002-2005 C. Barth Netterfield
 * (C) 2005-2009 D. V. Wiebe
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

/* _GD_DoRaw:  Read from a raw.  Returns number of samples read.
*/
static size_t _GD_DoRaw(DIRFILE *D, gd_entry_t *E,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t return_type, void *data_out)
{
  off64_t s0;
  size_t ns, n_read = 0;
  ssize_t samples_read;
  char *databuffer;

  dtrace("%p, %p, %lli, %lli, %zi, %zi, 0x%x, %p)", D, E, first_frame,
      first_samp, num_frames, num_samp, return_type, data_out);

  first_frame -= D->fragment[E->fragment_index].frame_offset;

  s0 = first_samp + first_frame * E->spf;
  ns = num_samp + num_frames * E->spf;

  databuffer = malloc(ns * E->e->size);
  if (databuffer == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%zi", 0);
    return 0;
  }

  if (s0 < 0) {
    n_read = _GD_FillZero(databuffer, E->data_type, s0, ns);
    ns -= n_read;
    s0 = 0;
  }

  if (ns > 0) {
    /** open the file (and cache the fp) if it hasn't been opened yet. */
    if (E->e->file[0].fp < 0) {
      if (!_GD_Supports(D, E, GD_EF_OPEN | GD_EF_SEEK | GD_EF_READ)) {
        dreturn("%i", 0);
        return 0;
      } else if (_GD_SetEncodedName(D, E->e->file, E->e->filebase, 0)) {
        dreturn("%i", 0);
        return 0;
      } else if ((*_gd_ef[E->e->file[0].encoding].open)(E->e->file,
            D->flags & GD_ACCMODE, 0))
      {
        _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[0].name, errno, NULL);
        dreturn("%zi", 0);
        return 0;
      }
    }

    if ((*_gd_ef[E->e->file[0].encoding].seek)(E->e->file, s0, E->data_type, 0)
        == -1)
    {
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[0].name, errno, NULL);
      dreturn("%zi", 0);
      return 0;
    }

    samples_read = (*_gd_ef[E->e->file[0].encoding].read)(E->e->file,
        databuffer + n_read * E->e->size, E->data_type, ns);

    if (samples_read == -1) {
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->file[0].name, errno, NULL);
      free(databuffer);
      dreturn("%zi", 0);
      return 0;
    }

    if (_gd_ef[E->e->file[0].encoding].ecor &&
        (D->fragment[E->fragment_index].byte_sex ==
#ifdef WORDS_BIGENDIAN
         GD_LITTLE_ENDIAN
#else
         GD_BIG_ENDIAN
#endif
        ))
      _GD_FixEndianness(databuffer + n_read * E->e->size, E->e->size,
          samples_read);

    n_read += samples_read;
  }
  _GD_ConvertType(D, databuffer, E->data_type, data_out, return_type, n_read);

  free(databuffer);

  dreturn("%zi", (D->error == GD_E_OK) ? n_read : 0);
  return (D->error == GD_E_OK) ? n_read : 0;
}

/* Macros to reduce tangly code */
#define POLYNOM5(t) \
  ((t*)data)[i] = (t)( \
    ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] \
      * ((t*)data)[i] * ((t*)data)[i] * a[5] \
    + ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] * a[4] \
    + ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] * a[3] \
    + ((t*)data)[i] * ((t*)data)[i] * a[2] \
    + ((t*)data)[i] * a[1] + a[0] \
    )

#define POLYNOM4(t) \
  ((t*)data)[i] = (t)( \
    ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] * a[4] \
    + ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] * a[3] \
    + ((t*)data)[i] * ((t*)data)[i] * a[2] \
    + ((t*)data)[i] * a[1] + a[0] \
    )

#define POLYNOM3(t) \
  ((t*)data)[i] = (t)( \
    ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] * a[3] \
    + ((t*)data)[i] * ((t*)data)[i] * a[2] \
    + ((t*)data)[i] * a[1] + a[0] \
    )

#define POLYNOM2(t) \
  ((t*)data)[i] = (t)( \
    ((t*)data)[i] * ((t*)data)[i] * a[2] \
    + ((t*)data)[i] * a[1] + a[0] \
    )

/* _GD_PolynomData: Compute data = Sum(i=0..n; data**i * a[i]), for scalar a,
 * and integer 2 <= n < GD_MAX_POLYNOM
 */
static void _GD_PolynomData(DIRFILE* D, void *data, gd_type_t type, size_t npts,
    int n, double* a)
{
  size_t i;

  dtrace("%p, %p, 0x%x, %zi, %i, %p", D, data, type, npts, n, a);

  if (n == 1) {
    /* no need to duplicate this case */
    _GD_ScaleData(D, data, type, npts, a[1], a[0]);
  } else if (n == 2) {
    switch (type) {
      case GD_NULL:
        break;
      case GD_INT8:
        for (i = 0; i < npts; i++)
          POLYNOM2(int8_t);
        break;
      case GD_UINT8:
        for (i = 0; i < npts; i++)
          POLYNOM2(uint8_t);
        break;
      case GD_INT16:
        for (i = 0; i < npts; i++)
          POLYNOM2(int16_t);
        break;
      case GD_UINT16:
        for (i = 0; i < npts; i++)
          POLYNOM2(uint16_t);
        break;
      case GD_INT32:
        for (i = 0; i < npts; i++)
          POLYNOM2(int32_t);
        break;
      case GD_UINT32:
        for (i = 0; i < npts; i++)
          POLYNOM2(uint32_t);
        break;
      case GD_INT64:
        for (i = 0; i < npts; i++)
          POLYNOM2(int64_t);
        break;
      case GD_UINT64:
        for (i = 0; i < npts; i++)
          POLYNOM2(uint64_t);
        break;
      case GD_FLOAT:
        for (i = 0; i < npts; i++)
          POLYNOM2(float);
        break;
      case GD_DOUBLE:
        for (i = 0; i < npts; i++)
          POLYNOM2(double);
        break;
      default:
        _GD_SetError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
        break;
    }
  } else if (n == 3) {
    switch (type) {
      case GD_NULL:
        break;
      case GD_INT8:
        for (i = 0; i < npts; i++)
          POLYNOM3(int8_t);
        break;
      case GD_UINT8:
        for (i = 0; i < npts; i++)
          POLYNOM3(uint8_t);
        break;
      case GD_INT16:
        for (i = 0; i < npts; i++)
          POLYNOM3(int16_t);
        break;
      case GD_UINT16:
        for (i = 0; i < npts; i++)
          POLYNOM3(uint16_t);
        break;
      case GD_INT32:
        for (i = 0; i < npts; i++)
          POLYNOM3(int32_t);
        break;
      case GD_UINT32:
        for (i = 0; i < npts; i++)
          POLYNOM3(uint32_t);
        break;
      case GD_INT64:
        for (i = 0; i < npts; i++)
          POLYNOM3(int64_t);
        break;
      case GD_UINT64:
        for (i = 0; i < npts; i++)
          POLYNOM3(uint64_t);
        break;
      case GD_FLOAT:
        for (i = 0; i < npts; i++)
          POLYNOM3(float);
        break;
      case GD_DOUBLE:
        for (i = 0; i < npts; i++)
          POLYNOM3(double);
        break;
      default:
        _GD_SetError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
        break;
    }
  } else if (n == 4) {
    switch (type) {
      case GD_NULL:
        break;
      case GD_INT8:
        for (i = 0; i < npts; i++)
          POLYNOM4(int8_t);
        break;
      case GD_UINT8:
        for (i = 0; i < npts; i++)
          POLYNOM4(uint8_t);
        break;
      case GD_INT16:
        for (i = 0; i < npts; i++)
          POLYNOM4(int16_t);
        break;
      case GD_UINT16:
        for (i = 0; i < npts; i++)
          POLYNOM4(uint16_t);
        break;
      case GD_INT32:
        for (i = 0; i < npts; i++)
          POLYNOM4(int32_t);
        break;
      case GD_UINT32:
        for (i = 0; i < npts; i++)
          POLYNOM4(uint32_t);
        break;
      case GD_INT64:
        for (i = 0; i < npts; i++)
          POLYNOM4(int64_t);
        break;
      case GD_UINT64:
        for (i = 0; i < npts; i++)
          POLYNOM4(uint64_t);
        break;
      case GD_FLOAT:
        for (i = 0; i < npts; i++)
          POLYNOM4(float);
        break;
      case GD_DOUBLE:
        for (i = 0; i < npts; i++)
          POLYNOM4(double);
        break;
      default:
        _GD_SetError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
        break;
    }
  } else if (n == 5) {
    switch (type) {
      case GD_NULL:
        break;
      case GD_INT8:
        for (i = 0; i < npts; i++)
          POLYNOM5(int8_t);
        break;
      case GD_UINT8:
        for (i = 0; i < npts; i++)
          POLYNOM5(uint8_t);
        break;
      case GD_INT16:
        for (i = 0; i < npts; i++)
          POLYNOM5(int16_t);
        break;
      case GD_UINT16:
        for (i = 0; i < npts; i++)
          POLYNOM5(uint16_t);
        break;
      case GD_INT32:
        for (i = 0; i < npts; i++)
          POLYNOM5(int32_t);
        break;
      case GD_UINT32:
        for (i = 0; i < npts; i++)
          POLYNOM5(uint32_t);
        break;
      case GD_INT64:
        for (i = 0; i < npts; i++)
          POLYNOM5(int64_t);
        break;
      case GD_UINT64:
        for (i = 0; i < npts; i++)
          POLYNOM5(uint64_t);
        break;
      case GD_FLOAT:
        for (i = 0; i < npts; i++)
          POLYNOM5(float);
        break;
      case GD_DOUBLE:
        for (i = 0; i < npts; i++)
          POLYNOM5(double);
        break;
      default:
        _GD_SetError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
        break;
    }
  } else {
    /* In this case, someone increased GD_MAX_POLYNOM without adding
     * to this if statement */
    _GD_InternalError(D);
  }

  dreturnvoid();
}

/* MultiplyData: Multiply A by B.  B is unchanged.
*/
static void _GD_MultiplyData(DIRFILE* D, void *A, unsigned int spfA, void *B,
    unsigned int spfB, gd_type_t type, size_t n)
{
  size_t i;

  dtrace("%p, %p, %u, %p, %u, 0x%x, %zi", D, A, spfA, B, spfB, type, n);

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
      break;
  }

  dreturnvoid();
}

/* _GD_DoLincom:  Read from a lincom.  Returns number of samples read.
*/
static size_t _GD_DoLincom(DIRFILE *D, gd_entry_t *E,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t return_type, void *data_out)
{
  void *tmpbuf;
  int i;
  int spf1, spf2;
  size_t n_read, n_read2, num_samp2;
  off64_t first_samp2;

  dtrace("%p, %p, %lli, %lli, %zi, %zi, 0x%x, %p", D, E, first_frame,
      first_samp, num_frames, num_samp, return_type, data_out);

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

  spf1 = _GD_GetSPF(D, E->e->entry[0]);
  if (D->error != GD_E_OK) {
    dreturn("%zi", 0);
    return 0;
  }

  /* read and scale the first field and record the number of samples
   * returned */
  n_read = _GD_DoField(D, E->e->entry[0], E->in_fields[0], first_frame,
      first_samp, num_frames, num_samp, return_type, data_out);

  if (D->error != GD_E_OK) {
    dreturn("%zi", 0);
    return 0;
  }

  /* Nothing to lincomise */
  if (n_read == 0) {
    dreturn("%zi", 0);
    return 0;
  }

  _GD_ScaleData(D, data_out, return_type, n_read, E->m[0], E->b[0]);

  if (E->n_fields > 1) {
    for (i = 1; i < E->n_fields; i++) {
      /* Resolve the next field, if needed */
      if (E->e->entry[i] == NULL) {
        E->e->entry[i] = _GD_FindField(D, E->in_fields[i], NULL);

        if (E->e->entry[i] == NULL) {
          _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, E->in_fields[i]);
          dreturn("%zi", 0);
          return 0;
        }

        /* scalar entries not allowed */
        if (E->e->entry[i]->field_type & GD_SCALAR_ENTRY) {
          _GD_SetError(D, GD_E_DIMENSION, 0, E->field, 0,
              E->e->entry[i]->field);
          dreturn("%zi", 0);
          return 0;
        }
      }

      /* find the samples per frame of the next field */
      spf2 = _GD_GetSPF(D, E->e->entry[i]);
      if (D->error != GD_E_OK) {
        dreturn("%zi", 0);
        return 0;
      }

      /* calculate the first sample and number of samples to read of the
       * next field */
      num_samp2 = (int)ceil((double)n_read * spf2 / spf1);
      first_samp2 = (first_frame * spf2 + first_samp * spf2 / spf1);

      /* Allocate a temporary buffer for the next field */
      tmpbuf = _GD_Alloc(D, return_type, num_samp2);

      if (D->error != GD_E_OK) {
        dreturn("%zi", 0);
        return 0;
      }

      /* read the next field */
      n_read2 = _GD_DoField(D, E->e->entry[i], E->in_fields[i], 0, first_samp2,
          0, num_samp2, return_type, tmpbuf);

      if (D->error != GD_E_OK) {
        free(tmpbuf);
        dreturn("%zi", 0);
        return 0;
      }

      _GD_ScaleData(D, tmpbuf, return_type, n_read2, E->m[i], E->b[i]);

      if (D->error != GD_E_OK) {
        free(tmpbuf);
        dreturn("%zi", 0);
        return 0;
      }

      if (n_read2 > 0 && n_read2 * spf1 != n_read * spf2)
        n_read = n_read2 * spf1 / spf2;

      _GD_AddData(D, data_out, spf1, tmpbuf, spf2, return_type, n_read);

      free(tmpbuf);
    }
  }

  dreturn("%zi", n_read);
  return n_read;
}

/* _GD_DoMultiply:  Read from a multiply.  Returns number of samples read.
*/
static size_t _GD_DoMultiply(DIRFILE *D, gd_entry_t* E,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t return_type, void *data_out)
{
  void *tmpbuf;
  int spf1, spf2;
  size_t n_read, n_read2, num_samp2;
  off64_t first_samp2;

  dtrace("%p, %p, %lli, %lli, %zi, %zi, 0x%x, %p", D, E, first_frame,
      first_samp, num_frames, num_samp, return_type, data_out);

  /* find the samples per frame of the first field */
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

  spf1 = _GD_GetSPF(D, E->e->entry[0]);
  if (D->error != GD_E_OK) {
    dreturn("%zi", 0);
    return 0;
  }

  /* read the first field and record the number of samples
   * returned */
  n_read = _GD_DoField(D, E->e->entry[0], E->in_fields[0], first_frame,
      first_samp, num_frames, num_samp, return_type, data_out);

  if (D->error != GD_E_OK) {
    dreturn("%zi", 0);
    return 0;
  }

  /* Nothing to multiply */
  if (n_read == 0) {
    dreturn("%zi", 0);
    return 0;
  }

  /* find the samples per frame of the second field */
  if (E->e->entry[1] == NULL) {
    E->e->entry[1] = _GD_FindField(D, E->in_fields[1], NULL);

    if (E->e->entry[0] == NULL) {
      _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, E->in_fields[0]);
      return 0;
    }

    /* scalar entries not allowed */
    if (E->e->entry[1]->field_type & GD_SCALAR_ENTRY) {
      _GD_SetError(D, GD_E_DIMENSION, 0, E->field, 0, E->e->entry[1]->field);
      dreturn("%zi", 0);
      return 0;
    }
  }

  spf2 = _GD_GetSPF(D, E->e->entry[1]);
  if (D->error != GD_E_OK) {
    dreturn("%zi", 0);
    return 0;
  }

  /* calculate the first sample and number of samples to read of the
   * second field */
  num_samp2 = (int)ceil((double)n_read * spf2 / spf1);
  first_samp2 = (first_frame * spf2 + first_samp * spf2 / spf1);

  /* Allocate a temporary buffer for the second field */
  tmpbuf = _GD_Alloc(D, return_type, num_samp2);

  if (D->error != GD_E_OK) {
    dreturn("%zi", 0);
    return 0;
  }

  /* read the second field */
  n_read2 = _GD_DoField(D, E->e->entry[1], E->in_fields[1], 0, first_samp2, 0,
      num_samp2, return_type, tmpbuf);
  if (D->error != GD_E_OK) {
    free(tmpbuf);
    dreturn("%zi", 0);
    return 0;
  }

  if (n_read2 > 0 && n_read2 * spf1 < n_read * spf2)
    n_read = n_read2 * spf1 / spf2;

  _GD_MultiplyData(D, data_out, spf1, tmpbuf, spf2, return_type, n_read);

  free(tmpbuf);

  dreturn("%zi", n_read);
  return n_read;
}

/* _GD_DoBit:  Read from a bitfield.  Returns number of samples read.
*/
static size_t _GD_DoBit(DIRFILE *D, gd_entry_t *E, int is_signed,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t return_type, void *data_out)
{
  void *tmpbuf;
  size_t i;
  int spf;
  size_t ns;
  size_t n_read;

  dtrace("%p, %p, %i, %lli, %lli, %zi, %zi, 0x%x, %p", D, E, is_signed,
      first_frame, first_samp, num_frames, num_samp, return_type, data_out);

  const uint64_t mask = (E->numbits == 64) ? 0xffffffffffffffffULL :
    ((uint64_t)1 << E->numbits) - 1;

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

  ns = num_samp + num_frames * spf;
  if (is_signed)
    tmpbuf = (int64_t *)malloc(ns * sizeof(int64_t));
  else
    tmpbuf = (uint64_t *)malloc(ns * sizeof(uint64_t));
  if (tmpbuf == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%zi", 0);
    return 0;
  }

  n_read = _GD_DoField(D, E->e->entry[0], E->in_fields[0], first_frame,
      first_samp, num_frames, num_samp, GD_UINT64, tmpbuf);

  if (D->error != GD_E_OK) {
    free(tmpbuf);
    dreturn("%zi", 0);
    return 0;
  }

  /* extract bits */
  if (is_signed) {
    uint64_t sign = -1 << (E->numbits + - 1);
    for (i = 0; i < n_read; i++)
      ((int64_t*)tmpbuf)[i] = (((((uint64_t*)tmpbuf)[i] >> E->bitnum) & mask)
        + sign) ^ sign;
  } else
    for (i = 0; i < n_read; i++)
      ((uint64_t*)tmpbuf)[i] = (((uint64_t*)tmpbuf)[i] >> E->bitnum) & mask;

  _GD_ConvertType(D, tmpbuf, (is_signed) ? GD_INT64 : GD_UINT64, data_out,
      return_type, n_read);
  free(tmpbuf);

  dreturn("%zi", n_read);
  return n_read;
}

/* _GD_DoPhase:  Read from a phase.  Returns number of samples read.
*/
static size_t _GD_DoPhase(DIRFILE *D, gd_entry_t *E,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t return_type, void *data_out)
{
  size_t n_read;

  dtrace("%p, %p, %lli, %lli, %zi, %zi, 0x%x, %p", D, E, first_frame,
      first_samp, num_frames, num_samp, return_type, data_out);

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

  n_read = _GD_DoField(D, E->e->entry[0], E->in_fields[0], first_frame,
      first_samp + E->shift, num_frames, num_samp, return_type, data_out);

  dreturn("%zi", n_read);
  return n_read;
}

/* _GD_DoLinterp:  Read from a linterp.  Returns number of samples read.
*/
static size_t _GD_DoLinterp(DIRFILE *D, gd_entry_t* E,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t return_type, void *data_out)
{
  size_t n_read = 0;

  dtrace("%p, %p, %lli, %lli, %zi, %zi, 0x%x, %p [%p]", D, E, first_frame,
      first_samp, num_frames, num_samp, return_type, data_out, E->e->entry[0]);

  if (E->e->table_len < 0) {
    _GD_ReadLinterpFile(D, E);
    if (D->error != GD_E_OK) {
      dreturn("%zi", 0);
      return 0;
    }
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

  n_read = _GD_DoField(D, E->e->entry[0], E->in_fields[0], first_frame,
      first_samp, num_frames, num_samp, return_type, data_out);

  if (D->error != GD_E_OK) {
    dreturn("%zi", 0);
    return 0;
  }

  _GD_LinterpData(D, data_out, return_type, n_read, E->e->x, E->e->y,
      E->e->table_len);

  dreturn("%zi", n_read);
  return n_read;
}

/* _GD_DoPolynom:  Read from a polynom.  Returns number of samples read.
*/
static size_t _GD_DoPolynom(DIRFILE *D, gd_entry_t *E,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t return_type, void *data_out)
{
  int spf;
  size_t n_read;

  dtrace("%p, %p, %lli, %lli, %zi, %zi, 0x%x, %p", D, E, first_frame,
      first_samp, num_frames, num_samp, return_type, data_out);

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

  /* read the input field */
  n_read = _GD_DoField(D, E->e->entry[0], E->in_fields[0], first_frame,
      first_samp, num_frames, num_samp, return_type, data_out);

  if (D->error != GD_E_OK) {
    dreturn("%zi", 0);
    return 0;
  }

  /* Nothing to polynomise */
  if (n_read == 0) {
    dreturn("%zi", 0);
    return 0;
  }

  _GD_PolynomData(D, data_out, return_type, n_read, E->poly_ord, E->a);

  dreturn("%zi", n_read);
  return n_read;
}

/* _GD_DoConst:  Read from a const.  Returns number of samples read (ie. 1).
*/
size_t _GD_DoConst(DIRFILE *D, const gd_entry_t *E, gd_type_t return_type,
    void *data_out)
{
  dtrace("%p, %p, 0x%x, %p", D, E, return_type, data_out);

  if (E->const_type & GD_SIGNED)
    _GD_ConvertType(D, &E->e->iconst, GD_INT64, data_out, return_type, 1);
  else if (E->const_type & GD_IEEE754)
    _GD_ConvertType(D, &E->e->dconst, GD_FLOAT64, data_out, return_type, 1);
  else
    _GD_ConvertType(D, &E->e->uconst, GD_UINT64, data_out, return_type, 1);

  if (D->error) { /* bad input type */
    dreturn("%i", 0);
    return 0;
  }

  dreturn("%i", 1);
  return 1;
}

/* _GD_DoString:  Read from a string.  Returns number of samples read (ie. 1).
*/
static size_t _GD_DoString(gd_entry_t *E, size_t num_samp, void *data_out)
{
  dtrace("%p, %zi, %p", E, num_samp, data_out);

  if (num_samp > 0 && data_out != NULL)
    strncpy(data_out, E->e->string, num_samp); 

  dreturn("%i", strlen(E->e->string) + 1);
  return strlen(E->e->string) + 1;
}

/* _GD_DoField: Locate the field in the database and read it.
*/
size_t _GD_DoField(DIRFILE *D, gd_entry_t *E, const char* field_code,
    off64_t first_frame, off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t return_type, void *data_out)
{
  size_t n_read = 0;

  dtrace("%p, %p, \"%s\", %lli, %lli, %zi, %zi, 0x%x, %p", D, E, field_code,
      first_frame, first_samp, num_frames, num_samp, return_type, data_out);

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
      n_read = _GD_DoRaw(D, E, first_frame, first_samp, num_frames,
          num_samp, return_type, data_out);
      break;
    case GD_LINTERP_ENTRY:
      n_read = _GD_DoLinterp(D, E, first_frame, first_samp, num_frames,
          num_samp, return_type, data_out);
      break;
    case GD_LINCOM_ENTRY:
      n_read = _GD_DoLincom(D, E, first_frame, first_samp, num_frames,
          num_samp, return_type, data_out);
      break;
    case GD_BIT_ENTRY:
      n_read = _GD_DoBit(D, E, 0, first_frame, first_samp, num_frames,
          num_samp, return_type, data_out);
      break;
    case GD_MULTIPLY_ENTRY:
      n_read = _GD_DoMultiply(D, E, first_frame, first_samp, num_frames,
          num_samp, return_type, data_out);
      break;
    case GD_PHASE_ENTRY:
      n_read = _GD_DoPhase(D, E, first_frame, first_samp, num_frames,
          num_samp, return_type, data_out);
      break;
    case GD_INDEX_ENTRY:
      /* if Asking for "INDEX", just return it */
      n_read = num_frames + num_samp;
      if (data_out != NULL)
        _GD_FillFileFrame(data_out, return_type, first_frame + first_samp,
            n_read);
      break;
    case GD_POLYNOM_ENTRY:
      n_read = _GD_DoPolynom(D, E, first_frame, first_samp, num_frames,
          num_samp, return_type, data_out);
      break;
    case GD_SBIT_ENTRY:
      n_read = _GD_DoBit(D, E, 1, first_frame, first_samp, num_frames,
          num_samp, return_type, data_out);
      break;
    case GD_CONST_ENTRY:
      n_read = _GD_DoConst(D, E, return_type, data_out);
      break;
    case GD_STRING_ENTRY:
      n_read = _GD_DoString(E, num_samp, data_out);
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
  size_t n_read = 0;
  gd_entry_t* entry;

  dtrace("%p, \"%s\", %lli, %lli, %zi, %zi, 0x%x, %p", D, field_code,
      first_frame, first_samp, num_frames, num_samp, return_type, data_out);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%zi", 0);
    return 0;
  }

  _GD_ClearError(D);

  entry = _GD_FindField(D, field_code, NULL);

  if (entry == NULL)
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
  else if (entry->field_type & GD_SCALAR_ENTRY)
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
  else 
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
