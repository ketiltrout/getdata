/* Copyright (C) 2002-2005 C. Barth Netterfield
 * Copyright (C) 2005-2011 D. V. Wiebe
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

#include "nan.h"

#define EXTRACT_REPR(it,ot,f) \
  for (i = 0; i < n; ++i) ((ot *)rdata)[i] = (ot)f(((it *)cdata)[i])

#ifdef GD_NO_C99_API
#define fargs(x) (((x) < 0) ? M_PI : 0)
#define fargu(x) (((x) & (1 << sizeof((x)))) ? M_PI : 0)

#define EXTRACT_REPRC(it,ot,f) \
  for (i = 0; i < n; ++i) ((ot *)rdata)[i] = (ot)f((it *)cdata + 2 * i)

#define EXTRACT_REPRC2(it,ot) \
  switch (repr) { \
    case GD_REPR_REAL: EXTRACT_REPRC(it,ot,creal); break; \
    case GD_REPR_IMAG: EXTRACT_REPRC(it,ot,cimag); break; \
    case GD_REPR_MOD:  EXTRACT_REPRC(it,ot,cabs); break; \
    case GD_REPR_ARG:  EXTRACT_REPRC(it,ot,carg); break; \
  }

#define EXTRACT_REPRR2(it,ot,f) \
  switch (repr) { \
    case GD_REPR_REAL: EXTRACT_REPR(it,ot,); break; \
    case GD_REPR_IMAG: EXTRACT_REPR(it,ot,0 *); break; \
    case GD_REPR_MOD:  EXTRACT_REPR(it,ot,fabs); break; \
    case GD_REPR_ARG:  EXTRACT_REPR(it,ot,f); break; \
  }

#define EXTRACT_REPRS(ot) \
  switch (in_type) { \
    case GD_UINT8:      EXTRACT_REPRR2(       uint8_t, ot,fargu); break; \
    case GD_INT8:       EXTRACT_REPRR2(        int8_t, ot,fargs); break; \
    case GD_UINT16:     EXTRACT_REPRR2(      uint16_t, ot,fargu); break; \
    case GD_INT16:      EXTRACT_REPRR2(       int16_t, ot,fargs); break; \
    case GD_UINT32:     EXTRACT_REPRR2(      uint32_t, ot,fargu); break; \
    case GD_INT32:      EXTRACT_REPRR2(       int32_t, ot,fargs); break; \
    case GD_UINT64:     EXTRACT_REPRR2(      uint64_t, ot,fargu); break; \
    case GD_INT64:      EXTRACT_REPRR2(       int64_t, ot,fargs); break; \
    case GD_FLOAT32:    EXTRACT_REPRR2(         float, ot,fargs); break; \
    case GD_FLOAT64:    EXTRACT_REPRR2(        double, ot,fargs); break; \
    case GD_COMPLEX64:  EXTRACT_REPRC2(        float, ot); break; \
    case GD_COMPLEX128: EXTRACT_REPRC2(       double, ot); break; \
    case GD_NULL: \
      break; \
    default: \
      _GD_SetError(D, GD_E_BAD_TYPE, in_type, NULL, 0, NULL); \
  }
#else
#define EXTRACT_REPR2(it,ot) \
  switch (repr) { \
    case GD_REPR_REAL: EXTRACT_REPR(it,ot,creal); break; \
    case GD_REPR_IMAG: EXTRACT_REPR(it,ot,cimag); break; \
    case GD_REPR_MOD:  EXTRACT_REPR(it,ot,cabs); break; \
    case GD_REPR_ARG:  EXTRACT_REPR(it,ot,carg); break; \
  }

#define EXTRACT_REPRS(ot) \
  switch (in_type) { \
    case GD_UINT8:      EXTRACT_REPR2(       uint8_t, ot); break; \
    case GD_INT8:       EXTRACT_REPR2(        int8_t, ot); break; \
    case GD_UINT16:     EXTRACT_REPR2(      uint16_t, ot); break; \
    case GD_INT16:      EXTRACT_REPR2(       int16_t, ot); break; \
    case GD_UINT32:     EXTRACT_REPR2(      uint32_t, ot); break; \
    case GD_INT32:      EXTRACT_REPR2(       int32_t, ot); break; \
    case GD_UINT64:     EXTRACT_REPR2(      uint64_t, ot); break; \
    case GD_INT64:      EXTRACT_REPR2(       int64_t, ot); break; \
    case GD_FLOAT32:    EXTRACT_REPR2(         float, ot); break; \
    case GD_FLOAT64:    EXTRACT_REPR2(        double, ot); break; \
    case GD_COMPLEX64:  EXTRACT_REPR2( complex float, ot); break; \
    case GD_COMPLEX128: EXTRACT_REPR2(complex double, ot); break; \
    case GD_NULL: \
      break; \
    default: \
      _GD_SetError(D, GD_E_BAD_TYPE, in_type, NULL, 0, NULL); \
  }
#endif


static void _GD_ExtractRepr(DIRFILE* D, const void* cdata, gd_type_t in_type,
    void* rdata, gd_type_t type, size_t n, int repr)
{
  size_t i;

  dtrace("%p, %p, %x, %p, %x, %zu, %i", D, cdata, in_type, rdata, type, n,
      repr);

  switch (type) {
    case GD_UINT8:      EXTRACT_REPRS(       uint8_t); break;
    case GD_INT8:       EXTRACT_REPRS(        int8_t); break;
    case GD_UINT16:     EXTRACT_REPRS(      uint16_t); break;
    case GD_INT16:      EXTRACT_REPRS(       int16_t); break;
    case GD_UINT32:     EXTRACT_REPRS(      uint32_t); break;
    case GD_INT32:      EXTRACT_REPRS(       int32_t); break;
    case GD_UINT64:     EXTRACT_REPRS(      uint64_t); break;
    case GD_INT64:      EXTRACT_REPRS(       int64_t); break;
    case GD_FLOAT32:    EXTRACT_REPRS(         float); break;
    case GD_FLOAT64:    EXTRACT_REPRS(        double); break;
#ifdef GD_NO_C99_API
#undef EXTRACT_REPRC
#undef EXTRACT_REPR
#define EXTRACT_REPRC(it,ot,f) \
  do { \
    for (i = 0; i < n; ++i) { \
      ((ot *)rdata)[2 * i] = (ot)f((it *)cdata + 2 * i); \
      ((ot *)rdata)[2 * i + 1] = 0; \
    } \
  } while(0)

#define EXTRACT_REPR(it,ot,f) \
  do { \
    for (i = 0; i < n; ++i) { \
      ((ot *)rdata)[2 * i] = (ot)f(((it *)cdata)[i]); \
      ((ot *)rdata)[2 * i + 1] = 0; \
    } \
  } while(0)

    case GD_COMPLEX64:  EXTRACT_REPRS(         float); break;
    case GD_COMPLEX128: EXTRACT_REPRS(        double); break;
#else
    case GD_COMPLEX64:  EXTRACT_REPRS( float complex); break;
    case GD_COMPLEX128: EXTRACT_REPRS(double complex); break;
#endif
    case GD_NULL:                                      break;
    default:
      _GD_SetError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
      break;
  }

  dreturnvoid();
}

/* _GD_FillFileFrame: fill dataout with frame indices
*/
static void _GD_FillFileFrame(void *dataout, gd_type_t rtype, off64_t s0,
    size_t n)
{
  size_t i;

  dtrace("%p, 0x%x, %lli, %zu", dataout, rtype, s0, n);

  switch (rtype) {
    case GD_INT8:
      for (i = 0; i < n; i++)
        ((int8_t *)dataout)[i] = (int8_t)(i + s0);
      break;
    case GD_UINT8:
      for (i = 0; i < n; i++)
        ((uint8_t *)dataout)[i] = (uint8_t)(i + s0);
      break;
    case GD_INT16:
      for (i = 0; i < n; i++)
        ((int16_t *)dataout)[i] = (int16_t)(i + s0);
      break;
    case GD_UINT16:
      for (i = 0; i < n; i++)
        ((uint16_t *)dataout)[i] = (uint16_t)(i + s0);
      break;
    case GD_INT32:
      for (i = 0; i < n; i++)
        ((int32_t *)dataout)[i] = (int32_t)(i + s0);
      break;
    case GD_UINT32:
      for (i = 0; i < n; i++)
        ((uint32_t *)dataout)[i] = (uint32_t)(i + s0);
      break;
    case GD_INT64:
      for (i = 0; i < n; i++)
        ((int64_t *)dataout)[i] = (int64_t)(i + s0);
      break;
    case GD_UINT64:
      for (i = 0; i < n; i++)
        ((uint64_t *)dataout)[i] = (uint64_t)(i + s0);
      break;
    case GD_FLOAT32:
      for (i = 0; i < n; i++)
        ((float *)dataout)[i] = (float)(i + s0);
      break;
    case GD_FLOAT64:
      for (i = 0; i < n; i++)
        ((double *)dataout)[i] = (double)(i + s0);
      break;
    case GD_COMPLEX64:
      for (i = 0; i < n; i++)
        _gd_r2ca(dataout, i, i + s0, float);
      break;
    case GD_COMPLEX128:
      for (i = 0; i < n; i++)
        _gd_r2ca(dataout, i, i + s0, double);
      break;
    default:
      break;
  }

  dreturnvoid();
}

/* _GD_FillZero: fill data buffer with zero/NaN of the appropriate type.
 */
static int _GD_FillZero(void *databuffer, gd_type_t type, size_t nz)
{
  size_t i;
  const double NaN = NAN;

  dtrace("%p, 0x%x, %zu", databuffer, type, nz);

  if (type & GD_IEEE754) {
    if (type == GD_FLOAT32)
      for (i = 0; i < nz; ++i)
        *((float *)databuffer + i) = (float)NaN;
    else
      for (i = 0; i < nz; ++i)
        *((double *)databuffer + i) = (double)NaN;
  } else if (type & GD_COMPLEX) {
    if (type == GD_COMPLEX64)
      for (i = 0; i < 2 * nz; ++i)
        *((float *)databuffer + i) = (float)NaN;
    else
      for (i = 0; i < 2 * nz; ++i)
        *((double *)databuffer + i) = (double) NaN;
  } else 
    memset(databuffer, 0, nz * GD_SIZE(type));

  dreturn("%zu", nz);

  return (nz);
}

/* _GD_DoRaw:  Read from a raw.  Returns number of samples read.
*/
static size_t _GD_DoRaw(DIRFILE *D, gd_entry_t *E, off64_t s0, size_t ns,
    gd_type_t return_type, void *data_out)
{
  size_t n_read = 0;
  ssize_t samples_read;
  char *databuffer;
  size_t zero_pad = 0;

  dtrace("%p, %p, %lli, %zu, 0x%x, %p)", D, E, s0, ns, return_type, data_out);

  if (s0 < E->EN(raw,spf) * D->fragment[E->fragment_index].frame_offset)
    zero_pad = E->EN(raw,spf) * D->fragment[E->fragment_index].frame_offset -
      s0;
  else
    s0 -= E->EN(raw,spf) * D->fragment[E->fragment_index].frame_offset;

  databuffer = (char *)malloc(ns * E->e->u.raw.size);
  if (databuffer == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  if (zero_pad > 0) {
    n_read = _GD_FillZero(databuffer, E->EN(raw,data_type), (zero_pad > ns) ?
        ns :
        zero_pad);
    ns -= n_read;
    s0 = 0;
  }

  if (ns > 0) {
    /** open the file (and cache the fp) if it hasn't been opened yet. */
    if (_GD_InitRawIO(D, E, GD_EF_SEEK | GD_EF_READ, 0)) {
      free(databuffer);
      dreturn("%i", 0);
      return 0;
    }

    if ((*_gd_ef[E->e->u.raw.file[0].subenc].seek)(E->e->u.raw.file, s0,
          E->EN(raw,data_type), 0) == -1)
    {
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.raw.file[0].name, errno, NULL);
      free(databuffer);
      dreturn("%i", 0);
      return 0;
    }

    samples_read =
      (*_gd_ef[E->e->u.raw.file[0].subenc].read)(E->e->u.raw.file,
          databuffer + n_read * E->e->u.raw.size, E->EN(raw,data_type), ns);

    if (samples_read == -1) {
      _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.raw.file[0].name, errno, NULL);
      free(databuffer);
      dreturn("%i", 0);
      return 0;
    }

    if (_gd_ef[E->e->u.raw.file[0].subenc].ecor) {
      /* convert to/from middle-ended doubles */
      if ((E->EN(raw,data_type) == GD_FLOAT64 ||
            E->EN(raw,data_type) == GD_COMPLEX128) &&
          D->fragment[E->fragment_index].byte_sex & GD_ARM_FLAG)
      {
        _GD_ArmEndianise((uint64_t *)(databuffer + n_read * E->e->u.raw.size),
            E->EN(raw,data_type) & GD_COMPLEX, samples_read);
      }

      if (D->fragment[E->fragment_index].byte_sex &
#ifdef WORDS_BIGENDIAN
          GD_LITTLE_ENDIAN
#else
          GD_BIG_ENDIAN
#endif
         )
      {
        if (E->EN(raw,data_type) & GD_COMPLEX)
          _GD_FixEndianness(databuffer + n_read * E->e->u.raw.size,
              E->e->u.raw.size / 2, samples_read * 2);
        else
          _GD_FixEndianness(databuffer + n_read * E->e->u.raw.size,
              E->e->u.raw.size, samples_read);
      }
    }

    n_read += samples_read;
  }
  _GD_ConvertType(D, databuffer, E->EN(raw,data_type), data_out, return_type,
      n_read);

  free(databuffer);

  dreturn("%zu", (D->error == GD_E_OK) ? n_read : (size_t)0);
  return (D->error == GD_E_OK) ? n_read : (size_t)0;
}

/* Macros to reduce tangly code */
#define POLYNOM5(t,npts) \
  for (i = 0; i < npts; i++) ((t*)data)[i] = (t)( \
      ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] \
      * ((t*)data)[i] * ((t*)data)[i] * a[5] \
      + ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] * a[4] \
      + ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] * a[3] \
      + ((t*)data)[i] * ((t*)data)[i] * a[2] \
      + ((t*)data)[i] * a[1] + a[0] \
      )

#define POLYNOM4(t,npts) \
  for (i = 0; i < npts; i++) ((t*)data)[i] = (t)( \
      ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] * a[4] \
      + ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] * a[3] \
      + ((t*)data)[i] * ((t*)data)[i] * a[2] \
      + ((t*)data)[i] * a[1] + a[0] \
      )

#define POLYNOM3(t,npts) \
  for (i = 0; i < npts; i++) ((t*)data)[i] = (t)( \
      ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] * a[3] \
      + ((t*)data)[i] * ((t*)data)[i] * a[2] \
      + ((t*)data)[i] * a[1] + a[0] \
      )

#define POLYNOM2(t,npts) \
  for (i = 0; i < npts; i++) ((t*)data)[i] = (t)( \
      ((t*)data)[i] * ((t*)data)[i] * a[2] \
      + ((t*)data)[i] * a[1] + a[0] \
      )

#ifdef GD_NO_C99_API
#define POLYNOMC(t) \
  switch (n) { \
    case 2: POLYNOM2(t,2 * npts); break; \
    case 3: POLYNOM3(t,2 * npts); break; \
    case 4: POLYNOM4(t,2 * npts); break; \
    case 5: POLYNOM5(t,2 * npts); break; \
    default: _GD_InternalError(D); \
  }
#else
#define POLYNOMC(t) POLYNOM(complex t)
#endif

#define POLYNOM(t) \
  switch (n) { \
    case 2: POLYNOM2(t,npts); break; \
    case 3: POLYNOM3(t,npts); break; \
    case 4: POLYNOM4(t,npts); break; \
    case 5: POLYNOM5(t,npts); break; \
    default: _GD_InternalError(D); \
  }

/* _GD_PolynomData: Compute data = Sum(i=0..n; data**i * a[i]), for scalar a,
 * and integer 2 <= n < GD_MAX_POLYORD
 */
static void _GD_PolynomData(DIRFILE* D, void *data, gd_type_t type, size_t npts,
    int n, double* a)
{
  size_t i;

  dtrace("%p, %p, 0x%x, %zu, %i, %p", D, data, type, npts, n, a);

  if (n == 1) {
    /* no need to duplicate this case */
    _GD_LincomData(D, 1, data, type, NULL, NULL, a + 1, a, NULL, npts);
  } else {
    switch (type) {
      case GD_NULL:                          break;
      case GD_INT8:       POLYNOM(  int8_t); break;
      case GD_UINT8:      POLYNOM( uint8_t); break;
      case GD_INT16:      POLYNOM( int16_t); break;
      case GD_UINT16:     POLYNOM(uint16_t); break;
      case GD_INT32:      POLYNOM( int32_t); break;
      case GD_UINT32:     POLYNOM(uint32_t); break;
      case GD_INT64:      POLYNOM( int64_t); break;
      case GD_UINT64:     POLYNOM(uint64_t); break;
      case GD_FLOAT32:    POLYNOM(   float); break;
      case GD_FLOAT64:    POLYNOM(  double); break;
      case GD_COMPLEX64:  POLYNOMC(  float); break;
      case GD_COMPLEX128: POLYNOMC( double); break;
      default:            _GD_SetError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
                          break;
    }
  }

  dreturnvoid();
}

#ifdef GD_NO_C99_API
#undef POLYNOM5
#undef POLYNOM4
#undef POLYNOM3
#undef POLYNOM2
#undef POLYNOMC

#define POLYNOM5(t,npts) \
  for (i = 0; i < npts; i++) ((t*)data)[i] = (t)( \
      ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] \
      * ((t*)data)[i] * ((t*)data)[i] * a[5][0] \
      + ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] * a[4][0]\
      + ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] * a[3][0] \
      + ((t*)data)[i] * ((t*)data)[i] * a[2][0] \
      + ((t*)data)[i] * a[1][0] + a[0][0] \
      )

#define POLYNOM4(t,npts) \
  for (i = 0; i < npts; i++) ((t*)data)[i] = (t)( \
      ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] * a[4][0] \
      + ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] * a[3][0] \
      + ((t*)data)[i] * ((t*)data)[i] * a[2][0] \
      + ((t*)data)[i] * a[1][0] + a[0][0] \
      )

#define POLYNOM3(t,npts) \
  for (i = 0; i < npts; i++) ((t*)data)[i] = (t)( \
      ((t*)data)[i] * ((t*)data)[i] * ((t*)data)[i] * a[3][0] \
      + ((t*)data)[i] * ((t*)data)[i] * a[2][0] \
      + ((t*)data)[i] * a[1][0] + a[0][0] \
      )

#define POLYNOM2(t,npts) \
  for (i = 0; i < npts; i++) ((t*)data)[i] = (t)( \
      ((t*)data)[i] * ((t*)data)[i] * a[2][0] \
      + ((t*)data)[i] * a[1][0] + a[0][0] \
      )

#define POLYNOMC5(t,npts) \
  do { \
    for (i = 0; i < npts; i++) { \
      const double x = ((t*)data)[2 * i]; \
      const double x2 = x * x; \
      const double x3 = x2 * x; \
      const double x4 = x3 * x; \
      const double x5 = x4 * x; \
      const double y = ((t*)data)[2 * i + 1]; \
      const double y2 = y * y; \
      const double y3 = y2 * y; \
      const double y4 = y3 * y; \
      const double y5 = y4 * y; \
      ((t*)data)[2 * i] = (t)( \
        a[5][0] * (x5 - 10 * x3 * y2 + 5 * x * y4) - \
        a[5][1] * (5 * x4 * y - 10 * x3 * y2 + y5) + \
        a[4][0] * (x4 - 6 * x2 * y2 + y4) - \
        a[4][1] * (4 * x3 * y - 4 * x * y3) + \
        a[3][0] * (x3 - 3 * x * y2) - a[3][1] * (3 * x2 * y - y3) + \
        a[2][0] * (x2 - y2) - a[2][1] * 2 * x * y + \
        a[1][0] * x - a[1][1] * y + a[0][0] \
        ); \
      ((t*)data)[2 * i + 1] = (t)( \
        a[5][1] * (x5 - 10 * x3 * y2 + 5 * x * y4) + \
        a[5][0] * (5 * x4 * y - 10 * x3 * y2 + y5) + \
        a[4][1] * (x4 - 6 * x2 * y2 + y4) + \
        a[4][0] * (4 * x3 * y - 4 * x * y3) + \
        a[3][1] * (x3 - 3 * x * y2) + a[3][0] * (3 * x2 * y - y3) + \
        a[2][1] * (x2 - y2) + a[2][0] * 2 * x * y + \
        a[1][1] * x + a[1][0] * y + a[0][1] \
        ); \
    } \
  } while (0)

#define POLYNOMC4(t,npts) \
  do { \
    for (i = 0; i < npts; i++) { \
      const double x = ((t*)data)[2 * i]; \
      const double x2 = x * x; \
      const double x3 = x2 * x; \
      const double x4 = x3 * x; \
      const double y = ((t*)data)[2 * i + 1]; \
      const double y2 = y * y; \
      const double y3 = y2 * y; \
      const double y4 = y3 * y; \
      ((t*)data)[2 * i] = (t)( \
        a[4][0] * (x4 - 6 * x2 * y2 + y4) - \
        a[4][1] * (4 * x3 * y - 4 * x * y3) + \
        a[3][0] * (x3 - 3 * x * y2) - a[3][1] * (3 * x2 * y - y3) + \
        a[2][0] * (x2 - y2) - a[2][1] * 2 * x * y + \
        a[1][0] * x - a[1][1] * y + a[0][0] \
        ); \
      ((t*)data)[2 * i + 1] = (t)( \
        a[4][1] * (x4 - 6 * x2 * y2 + y4) + \
        a[4][0] * (4 * x3 * y - 4 * x * y3) + \
        a[3][1] * (x3 - 3 * x * y2) + a[3][0] * (3 * x2 * y - y3) + \
        a[2][1] * (x2 - y2) + a[2][0] * 2 * x * y + \
        a[1][1] * x + a[1][0] * y + a[0][1] \
        ); \
    } \
  } while (0)

#define POLYNOMC3(t,npts) \
  do { \
    for (i = 0; i < npts; i++) { \
      const double x = ((t*)data)[2 * i]; \
      const double x2 = x * x; \
      const double x3 = x2 * x; \
      const double y = ((t*)data)[2 * i + 1]; \
      const double y2 = y * y; \
      const double y3 = y2 * y; \
      ((t*)data)[2 * i] = (t)( \
        a[3][0] * (x3 - 3 * x * y2) - a[3][1] * (3 * x2 * y - y3) + \
        a[2][0] * (x2 - y2) - a[2][1] * 2 * x * y + \
        a[1][0] * x - a[1][1] * y + a[0][0] \
        ); \
      ((t*)data)[2 * i + 1] = (t)( \
        a[3][1] * (x3 - 3 * x * y2) + a[3][0] * (3 * x2 * y - y3) + \
        a[2][1] * (x2 - y2) + a[2][0] * 2 * x * y + \
        a[1][1] * x + a[1][0] * y + a[0][1] \
        ); \
    } \
  } while (0)

#define POLYNOMC2(t,npts) \
  do { \
    for (i = 0; i < npts; i++) { \
      const double x = ((t*)data)[2 * i]; \
      const double x2 = x * x; \
      const double y = ((t*)data)[2 * i + 1]; \
      const double y2 = y * y; \
      ((t*)data)[2 * i] = (t)( \
        a[2][0] * (x2 - y2) - a[2][1] * 2 * x * y + \
        a[1][0] * x - a[1][1] * y + a[0][0] \
        ); \
      ((t*)data)[2 * i + 1] = (t)( \
        a[2][1] * (x2 - y2) + a[2][0] * 2 * x * y + \
        a[1][1] * x + a[1][0] * y + a[0][1] \
        ); \
    } \
  } while (0)

#define POLYNOMC(t) \
  switch (n) { \
    case 2: POLYNOMC2(t,npts); break; \
    case 3: POLYNOMC3(t,npts); break; \
    case 4: POLYNOMC4(t,npts); break; \
    case 5: POLYNOMC5(t,npts); break; \
    default: _GD_InternalError(D); \
  }
#endif


/* _GD_CPolynomData: Compute data = Sum(i=0..n; data**i * a[i]), for complex
 * scalar a, and integer 2 <= n < GD_MAX_POLYORD
 */
static void _GD_CPolynomData(DIRFILE* D, void *data, gd_type_t type,
    size_t npts, int n, GD_DCOMPLEXV(a))
{
  size_t i;

  dtrace("%p, %p, 0x%x, %zu, %i, %p", D, data, type, npts, n, a);

  if (n == 1) {
    /* no need to duplicate this case */
    _GD_CLincomData(D, 1, data, type, NULL, NULL, a + 1, a, NULL, npts);
  } else {
    switch (type) {
      case GD_NULL:                          break;
      case GD_INT8:       POLYNOM(  int8_t); break;
      case GD_UINT8:      POLYNOM( uint8_t); break;
      case GD_INT16:      POLYNOM( int16_t); break;
      case GD_UINT16:     POLYNOM(uint16_t); break;
      case GD_INT32:      POLYNOM( int32_t); break;
      case GD_UINT32:     POLYNOM(uint32_t); break;
      case GD_INT64:      POLYNOM( int64_t); break;
      case GD_UINT64:     POLYNOM(uint64_t); break;
      case GD_FLOAT32:    POLYNOM(   float); break;
      case GD_FLOAT64:    POLYNOM(  double); break;
      case GD_COMPLEX64:  POLYNOMC(  float); break;
      case GD_COMPLEX128: POLYNOMC( double); break;
      default:            _GD_SetError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
                          break;
    }
  }

  dreturnvoid();
}

#ifdef GD_NO_C99_API
#define MULTIPLYC(t) \
  do { \
    for (i = 0; i < n; i++) { \
      ((t*)A)[2 * i] = (t)(((t*)A)[2 * i] * B[i * spfB / spfA]); \
      ((t*)A)[2 * i + 1] = (t)(((t*)A)[2 * i + 1] * B[i * spfB / spfA]); \
    } \
  } while (0)
#else
#define MULTIPLYC(t) MULTIPLY(complex t)
#endif

#define MULTIPLY(t) \
  for (i = 0; i < n; i++) ((t*)A)[i] = (t)(((t*)A)[i] * B[i * spfB / spfA])

/* MultiplyData: Multiply A by B.  B is unchanged.
*/
static void _GD_MultiplyData(DIRFILE* D, void *A, gd_spf_t spfA, double *B,
    gd_spf_t spfB, gd_type_t type, size_t n)
{
  size_t i;

  dtrace("%p, %p, %u, %p, %u, 0x%x, %zu", D, A, spfA, B, spfB, type, n);

  switch (type) {
    case GD_NULL:                           break;
    case GD_UINT8:      MULTIPLY( uint8_t); break;
    case GD_INT8:       MULTIPLY(  int8_t); break;
    case GD_UINT16:     MULTIPLY(uint16_t); break;
    case GD_INT16:      MULTIPLY( int16_t); break;
    case GD_UINT32:     MULTIPLY(uint32_t); break;
    case GD_INT32:      MULTIPLY( int32_t); break;
    case GD_UINT64:     MULTIPLY(uint64_t); break;
    case GD_INT64:      MULTIPLY( int64_t); break;
    case GD_FLOAT32:    MULTIPLY(   float); break;
    case GD_FLOAT64:    MULTIPLY(  double); break;
    case GD_COMPLEX64:  MULTIPLYC(  float); break;
    case GD_COMPLEX128: MULTIPLYC( double); break;
    default:            _GD_SetError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
                        break;
  }

  dreturnvoid();
}

#ifdef GD_NO_C99_API
#undef MULTIPLY
#undef MULTIPLYC

#define MULTIPLYC(t) \
  do { \
    for (i = 0; i < n; i++) { \
      const int i2 = 2 * (i * spfB / spfA); \
      const t x = ((t*)A)[2 * i]; \
      const t y = ((t*)A)[2 * i + 1]; \
      ((t*)A)[2 * i] = (t)(x * B[i2] - y * B[i2 + 1]); \
      ((t*)A)[2 * i + 1] = (t)(y * B[i2] + x * B[i2 + 1]); \
    } \
  } while (0)

#define MULTIPLY(t) \
  for (i = 0; i < n; i++) ((t*)A)[i] = (t)(((t*)A)[i] * \
      B[2 * (i * spfB / spfA)])

#endif

/* CMultiplyData: Multiply A by B.  B is complex.
*/
static void _GD_CMultiplyData(DIRFILE* D, void *A, gd_spf_t spfA,
    GD_DCOMPLEXP(B), gd_spf_t spfB, gd_type_t type, size_t n)
{
  size_t i;

  dtrace("%p, %p, %u, %p, %u, 0x%x, %zu", D, A, spfA, B, spfB, type, n);

  switch (type) {
    case GD_NULL:                           break;
    case GD_UINT8:      MULTIPLY( uint8_t); break;
    case GD_INT8:       MULTIPLY(  int8_t); break;
    case GD_UINT16:     MULTIPLY(uint16_t); break;
    case GD_INT16:      MULTIPLY( int16_t); break;
    case GD_UINT32:     MULTIPLY(uint32_t); break;
    case GD_INT32:      MULTIPLY( int32_t); break;
    case GD_UINT64:     MULTIPLY(uint64_t); break;
    case GD_INT64:      MULTIPLY( int64_t); break;
    case GD_FLOAT32:    MULTIPLY(   float); break;
    case GD_FLOAT64:    MULTIPLY(  double); break;
    case GD_COMPLEX64:  MULTIPLYC(  float); break;
    case GD_COMPLEX128: MULTIPLYC( double); break;
    default:            _GD_SetError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
                        break;
  }

  dreturnvoid();
}

#ifdef GD_NO_C99_API
#define DIVIDEC(t) \
  do { \
    for (i = 0; i < n; i++) { \
      ((t*)A)[2 * i] = (t)(((t*)A)[2 * i] / B[i * spfB / spfA]); \
      ((t*)A)[2 * i + 1] = (t)(((t*)A)[2 * i + 1] / B[i * spfB / spfA]); \
    } \
  } while(0)
#else
#define DIVIDEC(t) DIVIDE(complex t)
#endif

#define DIVIDE(t) \
  for (i = 0; i < n; i++) ((t*)A)[i] = (t)(((t*)A)[i] / B[i * spfB / spfA])

/* DivideData: Divide B by A.  B is unchanged.
*/
static void _GD_DivideData(DIRFILE *D, void *A, gd_spf_t spfA, double *B,
    gd_spf_t spfB, gd_type_t type, size_t n)
{
  size_t i;

  dtrace("%p, %p, %u, %p, %u, 0x%x, %zu", D, A, spfA, B, spfB, type, n);

  switch (type) {
    case GD_NULL:                         break;
    case GD_UINT8:      DIVIDE( uint8_t); break;
    case GD_INT8:       DIVIDE(  int8_t); break;
    case GD_UINT16:     DIVIDE(uint16_t); break;
    case GD_INT16:      DIVIDE( int16_t); break;
    case GD_UINT32:     DIVIDE(uint32_t); break;
    case GD_INT32:      DIVIDE( int32_t); break;
    case GD_UINT64:     DIVIDE(uint64_t); break;
    case GD_INT64:      DIVIDE( int64_t); break;
    case GD_FLOAT32:    DIVIDE(   float); break;
    case GD_FLOAT64:    DIVIDE(  double); break;
    case GD_COMPLEX64:  DIVIDEC(  float); break;
    case GD_COMPLEX128: DIVIDEC( double); break;
    default:            _GD_SetError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
                        break;
  }

  dreturnvoid();
}

#ifdef GD_NO_C99_API
#undef DIVIDE
#undef DIVIDEC

#define DIVIDEC(t) \
  do { \
    for (i = 0; i < n; i++) { \
      const int i2 = 2 * (i * spfB / spfA); \
      const t x = ((t*)A)[2 * i]; \
      const t y = ((t*)A)[2 * i + 1]; \
      const double d = B[i2] * B[i2] + B[i2 + 1] * B[i2 + 1]; \
      ((t*)A)[2 * i] = (t)((x * B[i2] + y * B[i2 + 1]) / d); \
      ((t*)A)[2 * i + 1] = (t)((x * B[i2] + y * B[i2 + 1]) / d); \
    } \
  } while (0)

#define DIVIDE(t) \
  for (i = 0; i < n; i++) ((t*)A)[i] = (t)(((t*)A)[i] / \
      B[2 * (i * spfB / spfA)])

#endif

/* CDivideData: Divide A by B.  B is complex.
*/
static void _GD_CDivideData(DIRFILE *D, void *A, gd_spf_t spfA,
    GD_DCOMPLEXP(B), gd_spf_t spfB, gd_type_t type, size_t n)
{
  size_t i;

  dtrace("%p, %p, %u, %p, %u, 0x%x, %zu", D, A, spfA, B, spfB, type, n);

  switch (type) {
    case GD_NULL:                         break;
    case GD_UINT8:      DIVIDE( uint8_t); break;
    case GD_INT8:       DIVIDE(  int8_t); break;
    case GD_UINT16:     DIVIDE(uint16_t); break;
    case GD_INT16:      DIVIDE( int16_t); break;
    case GD_UINT32:     DIVIDE(uint32_t); break;
    case GD_INT32:      DIVIDE( int32_t); break;
    case GD_UINT64:     DIVIDE(uint64_t); break;
    case GD_INT64:      DIVIDE( int64_t); break;
    case GD_FLOAT32:    DIVIDE(   float); break;
    case GD_FLOAT64:    DIVIDE(  double); break;
    case GD_COMPLEX64:  DIVIDEC(  float); break;
    case GD_COMPLEX128: DIVIDEC( double); break;
    default:            _GD_SetError(D, GD_E_BAD_TYPE, type, NULL, 0, NULL);
                        break;
  }

  dreturnvoid();
}

/* _GD_DoLincom:  Read from a lincom.  Returns number of samples read.
*/
static size_t _GD_DoLincom(DIRFILE *D, gd_entry_t *E, off64_t first_samp,
    size_t num_samp, gd_type_t return_type, void *data_out)
{
  gd_spf_t spf[GD_MAX_LINCOM];
  size_t n_read;
  int i;
  void *tmpbuf2 = NULL;
  void *tmpbuf3 = NULL;
  const gd_type_t ntype = (return_type & GD_COMPLEX) ? GD_COMPLEX128
    : GD_FLOAT64;

  dtrace("%p, %p, %lli, %zu, 0x%x, %p", D, E, first_samp, num_samp, return_type,
      data_out);

  /* input field checks */
  for (i = 0; i < E->EN(lincom,n_fields); ++i) {
    if (_GD_BadInput(D, E, i)) {
      dreturn("%i", 0);
      return 0;
    }

    spf[i] = _GD_GetSPF(D, E->e->entry[0]);
    if (D->error != GD_E_OK) {
      dreturn("%i", 0);
      return 0;
    }
  }

  /* read the first field and record the number of samples returned -- we can
   * safely store this in the output buffer, with the correct return type as
   * it will not aversely affect our later math */
  n_read = _GD_DoField(D, E->e->entry[0], E->e->repr[0], first_samp, num_samp,
      return_type, data_out);

  if (D->error) {
    dreturn("%i", 0);
    return 0;
  }

  /* Nothing to lincomise */
  if (n_read == 0) {
    dreturn("%i", 0);
    return 0;
  }

  /* Some dirfiles use "bar LINCOM foo 1 0" to rename <foo> to <bar>.  I
   * recommend using "bar PHASE foo 0" in this case, but we'll accomodate them
   * as much as we can.  Suggested by MDT. */
  if (E->EN(lincom,n_fields) == 1 && _gd_ccmpl(E->EN(lincom,cm)[0],1,0) &&
      _gd_ccmpl(E->EN(lincom,cb)[0],0,0))
  {
    dreturn("%zu", n_read);
    return n_read;
  }

  /* Read the second field, if present */
  if (E->EN(lincom,n_fields) > 1) {
    /* calculate the first sample, type and number of samples to read of the
     * second field */
    size_t n_read2;
    size_t num_samp2 = (int)ceil((double)n_read * spf[1] / spf[0]);
    off64_t first_samp2 = first_samp * spf[1] / spf[0];

    /* Allocate a temporary buffer for the next field */
    tmpbuf2 = _GD_Alloc(D, ntype, num_samp2);
    if (D->error) {
      free(tmpbuf2);
      dreturn("%i", 0);
      return 0;
    }

    /* read the second field */
    n_read2 = _GD_DoField(D, E->e->entry[1], E->e->repr[1], first_samp2,
        num_samp2, ntype, tmpbuf2);
    if (D->error || n_read2 == 0) {
      free(tmpbuf2);
      dreturn("%i", 0);
      return 0;
    }

    /* adjust n_read for a short read from field two */
    if (n_read2 * spf[0] != n_read * spf[1])
      n_read = n_read2 * spf[0] / spf[1];

    /* Do the same for the third field, if needed */
    if (E->EN(lincom,n_fields) > 2) {
      size_t n_read3;
      size_t num_samp3 = (int)ceil((double)n_read * spf[2] / spf[0]);
      off64_t first_samp3 = first_samp * spf[2] / spf[0];

      tmpbuf3 = _GD_Alloc(D, ntype, num_samp3);
      if (D->error) {
        free(tmpbuf2);
        free(tmpbuf3);
        dreturn("%i", 0);
        return 0;
      }

      n_read3 = _GD_DoField(D, E->e->entry[2], E->e->repr[2], first_samp3,
          num_samp3, ntype, tmpbuf3);
      if (D->error || n_read3 == 0) {
        free(tmpbuf2);
        free(tmpbuf3);
        dreturn("%i", 0);
        return 0;
      }

      if (n_read3 * spf[0] != n_read * spf[2])
        n_read = n_read3 * spf[0] / spf[2];
    }
  }

  /* Compute everything at once */
  if (E->comp_scal)
    _GD_CLincomData(D, E->EN(lincom,n_fields), data_out, return_type,
        (GD_DCOMPLEXP_t)tmpbuf2, (GD_DCOMPLEXP_t)tmpbuf3, E->EN(lincom,cm),
        E->EN(lincom,cb), spf, n_read);
  else
    _GD_LincomData(D, E->EN(lincom,n_fields), data_out, return_type,
        (double *)tmpbuf2, (double *)tmpbuf3, E->EN(lincom,m), E->EN(lincom,b),
        spf, n_read);

  /* free temporary buffers */
  free(tmpbuf2);
  free(tmpbuf3);

  if (D->error)
    n_read = 0;

  dreturn("%zu", n_read);
  return n_read;
}

/* _GD_DoMultiply:  Read from a multiply.  Returns number of samples read.
*/
static size_t _GD_DoMultiply(DIRFILE *D, gd_entry_t* E, off64_t first_samp,
    size_t num_samp, gd_type_t return_type, void *data_out)
{
  void *tmpbuf = NULL;
  gd_spf_t spf1, spf2;
  size_t n_read, n_read2, num_samp2;
  off64_t first_samp2;
  gd_type_t type2;

  dtrace("%p, %p, %lli, %zu, 0x%x, %p", D, E, first_samp, num_samp, return_type,
      data_out);

  /* Check input fields */
  if (_GD_BadInput(D, E, 0)) {
    dreturn("%i", 0);
    return 0;
  }

  if (_GD_BadInput(D, E, 1)) {
    dreturn("%i", 0);
    return 0;
  }

  /* find the samples per frame of the first field */
  spf1 = _GD_GetSPF(D, E->e->entry[0]);
  if (D->error != GD_E_OK) {
    dreturn("%i", 0);
    return 0;
  }

  /* read the first field and record the number of samples returned */
  n_read = _GD_DoField(D, E->e->entry[0], E->e->repr[0], first_samp, num_samp,
      return_type, data_out);

  if (D->error != GD_E_OK) {
    dreturn("%i", 0);
    return 0;
  }

  /* Nothing to multiply */
  if (n_read == 0) {
    dreturn("%i", 0);
    return 0;
  }

  /* find the samples per frame of the second field */
  spf2 = _GD_GetSPF(D, E->e->entry[1]);
  if (D->error != GD_E_OK) {
    dreturn("%i", 0);
    return 0;
  }

  /* calculate the first sample and number of samples to read of the
   * second field */
  num_samp2 = (int)ceil((double)n_read * spf2 / spf1);
  first_samp2 = first_samp * spf2 / spf1;

  /* find the native type of the second field */
  type2 = (_GD_NativeType(D, E->e->entry[1], E->e->repr[1]) & GD_COMPLEX) ?
    GD_COMPLEX128 : GD_FLOAT64;

  /* Allocate a temporary buffer for the second field */
  tmpbuf = _GD_Alloc(D, type2, num_samp2);

  if (D->error != GD_E_OK) {
    free(tmpbuf);
    dreturn("%i", 0);
    return 0;
  }

  /* read the second field */
  n_read2 = _GD_DoField(D, E->e->entry[1], E->e->repr[1], first_samp2,
      num_samp2, type2, tmpbuf);

  if (D->error != GD_E_OK) {
    free(tmpbuf);
    dreturn("%i", 0);
    return 0;
  }

  if (n_read2 > 0 && n_read2 * spf1 < n_read * spf2)
    n_read = n_read2 * spf1 / spf2;

  if (type2 & GD_COMPLEX)
    _GD_CMultiplyData(D, data_out, spf1, (GD_DCOMPLEXP_t)tmpbuf, spf2,
        return_type, n_read);
  else
    _GD_MultiplyData(D, data_out, spf1, (double *)tmpbuf, spf2, return_type,
        n_read);

  free(tmpbuf);

  dreturn("%zu", n_read);
  return n_read;
}

/* _GD_DoRecip:  Read from a recip.  Returns number of samples read.
*/
static size_t _GD_DoRecip(DIRFILE *D, gd_entry_t* E, off64_t first_samp,
    size_t num_samp, gd_type_t return_type, void *data_out)
{
  size_t n_read;

  dtrace("%p, %p, %lli, %zu, 0x%x, %p", D, E, first_samp, num_samp, return_type,
      data_out);

  /* Check input fields */
  if (_GD_BadInput(D, E, 0)) {
    dreturn("%i", 0);
    return 0;
  }

  /* read the first field and record the number of samples returned */
  n_read = _GD_DoField(D, E->e->entry[0], E->e->repr[0], first_samp, num_samp,
      return_type, data_out);

  if (D->error != GD_E_OK) {
    dreturn("%i", 0);
    return 0;
  }

  /* Nothing to divide */
  if (n_read == 0) {
    dreturn("%i", 0);
    return 0;
  }

  /* Compute a reciprocal */
  if (E->comp_scal)
    _GD_CInvertData(D, data_out, return_type, E->EN(recip,cdividend), num_samp);
  else
    _GD_InvertData(D, data_out, return_type, E->EN(recip,dividend), num_samp);

  dreturn("%zu", n_read);
  return n_read;
}

/* _GD_DoDivide:  Read from a divide.  Returns number of samples read.
*/
static size_t _GD_DoDivide(DIRFILE *D, gd_entry_t* E, off64_t first_samp,
    size_t num_samp, gd_type_t return_type, void *data_out)
{
  void *tmpbuf = NULL;
  gd_spf_t spf1, spf2;
  size_t n_read, n_read2, num_samp2;
  off64_t first_samp2;
  gd_type_t type2;

  dtrace("%p, %p, %lli, %zu, 0x%x, %p", D, E, first_samp, num_samp, return_type,
      data_out);

  /* Check input fields */
  if (_GD_BadInput(D, E, 0) || _GD_BadInput(D, E, 1)) {
    dreturn("%i", 0);
    return 0;
  }

  /* read the first field and record the number of samples returned */
  n_read = _GD_DoField(D, E->e->entry[0], E->e->repr[0], first_samp, num_samp,
      return_type, data_out);

  if (D->error != GD_E_OK) {
    dreturn("%i", 0);
    return 0;
  }

  /* Nothing to divide */
  if (n_read == 0) {
    dreturn("%i", 0);
    return 0;
  }

  /* compute a division */
  /* find the samples per frame of the dividend */
  spf1 = _GD_GetSPF(D, E->e->entry[0]);
  if (D->error != GD_E_OK) {
    dreturn("%i", 0);
    return 0;
  }

  /* find the samples per frame of the second field */
  spf2 = _GD_GetSPF(D, E->e->entry[1]);
  if (D->error != GD_E_OK) {
    dreturn("%i", 0);
    return 0;
  }

  /* calculate the first sample and number of samples to read of the
   * second field */
  num_samp2 = (int)ceil((double)n_read * spf2 / spf1);
  first_samp2 = first_samp * spf2 / spf1;

  /* find the native type of the second field */
  type2 = (_GD_NativeType(D, E->e->entry[1], E->e->repr[1]) & GD_COMPLEX) ?
    GD_COMPLEX128 : GD_FLOAT64;

  /* Allocate a temporary buffer for the second field */
  tmpbuf = _GD_Alloc(D, type2, num_samp2);

  if (D->error != GD_E_OK) {
    free(tmpbuf);
    dreturn("%i", 0);
    return 0;
  }

  /* read the second field */
  n_read2 = _GD_DoField(D, E->e->entry[1], E->e->repr[1], first_samp2,
      num_samp2, type2, tmpbuf);

  if (D->error != GD_E_OK) {
    free(tmpbuf);
    dreturn("%i", 0);
    return 0;
  }

  if (n_read2 > 0 && n_read2 * spf1 < n_read * spf2)
    n_read = n_read2 * spf1 / spf2;

  if (type2 & GD_COMPLEX)
    _GD_CDivideData(D, data_out, spf1, (GD_DCOMPLEXP_t)tmpbuf, spf2,
        return_type, n_read);
  else
    _GD_DivideData(D, data_out, spf1, (double *)tmpbuf, spf2, return_type,
        n_read);

  free(tmpbuf);

  dreturn("%zu", n_read);
  return n_read;
}

/* _GD_DoBit:  Read from a bitfield.  Returns number of samples read.
 *             This is used by both BIT and SBIT (is_signed distinguishes)
 */
static size_t _GD_DoBit(DIRFILE *D, gd_entry_t *E, int is_signed,
    off64_t first_samp, size_t num_samp, gd_type_t return_type, void *data_out)
{
  void *tmpbuf;
  size_t i;
  size_t n_read;
  const uint64_t mask = (E->EN(bit,numbits) == 64) ? 0xffffffffffffffffULL :
    ((uint64_t)1 << E->EN(bit,numbits)) - 1;

  dtrace("%p, %p, %i, %lli, %zu, 0x%x, %p", D, E, is_signed, first_samp,
      num_samp, return_type, data_out);

  if (_GD_BadInput(D, E, 0)) {
    dreturn("%i", 0);
    return 0;
  }

  if (is_signed)
    tmpbuf = (int64_t *)malloc(num_samp * sizeof(int64_t));
  else
    tmpbuf = (uint64_t *)malloc(num_samp * sizeof(uint64_t));
  if (tmpbuf == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  n_read = _GD_DoField(D, E->e->entry[0], E->e->repr[0], first_samp, num_samp,
      (is_signed) ? GD_INT64 : GD_UINT64, tmpbuf);

  if (D->error != GD_E_OK) {
    free(tmpbuf);
    dreturn("%i", 0);
    return 0;
  }

  /* extract bits */
  if (is_signed) {
    uint64_t sign = -1 << (E->EN(bit,numbits) - 1);
    for (i = 0; i < n_read; i++)
      ((int64_t *)tmpbuf)[i] =
        (((((uint64_t *)tmpbuf)[i] >> E->EN(bit,bitnum)) & mask) + sign) ^ sign;
  } else
    for (i = 0; i < n_read; i++)
      ((uint64_t *)tmpbuf)[i] = (((uint64_t *)tmpbuf)[i] >> E->EN(bit,bitnum))
        & mask;

  _GD_ConvertType(D, tmpbuf, (is_signed) ? GD_INT64 : GD_UINT64, data_out,
      return_type, n_read);
  free(tmpbuf);

  dreturn("%zu", n_read);
  return n_read;
}

/* _GD_DoPhase:  Read from a phase.  Returns number of samples read.
*/
static size_t _GD_DoPhase(DIRFILE *D, gd_entry_t *E, off64_t first_samp,
    size_t num_samp, gd_type_t return_type, void *data_out)
{
  size_t n_read;

  dtrace("%p, %p, %lli, %zu, 0x%x, %p", D, E, first_samp, num_samp, return_type,
      data_out);

  if (_GD_BadInput(D, E, 0)) {
    dreturn("%i", 0);
    return 0;
  }

  n_read = _GD_DoField(D, E->e->entry[0], E->e->repr[0], first_samp +
      E->EN(phase,shift), num_samp, return_type, data_out);

  dreturn("%zu", n_read);
  return n_read;
}

/* _GD_DoLinterp:  Read from a linterp.  Returns number of samples read.
*/
static size_t _GD_DoLinterp(DIRFILE *D, gd_entry_t* E, off64_t first_samp,
    size_t num_samp, gd_type_t return_type, void *data_out)
{
  size_t n_read = 0;
  double* data_in;

  dtrace("%p, %p, %lli, %zu, 0x%x, %p", D, E, first_samp, num_samp, return_type,
      data_out);

  if (E->e->u.linterp.table_len < 0) {
    _GD_ReadLinterpFile(D, E);
    if (D->error != GD_E_OK) {
      dreturn("%i", 0);
      return 0;
    }
  }

  if (_GD_BadInput(D, E, 0)) {
    dreturn("%i", 0);
    return 0;
  }

  /* allocate a temporary buffer */
  data_in = (double *)_GD_Alloc(D, GD_FLOAT64, num_samp);

  if (D->error) {
    free(data_in);
    dreturn("%i", 0);
    return 0;
  }

  n_read = _GD_DoField(D, E->e->entry[0], E->e->repr[0], first_samp, num_samp,
      GD_FLOAT64, data_in);

  if (D->error != GD_E_OK) {
    free(data_in);
    dreturn("%i", 0);
    return 0;
  }

  _GD_LinterpData(D, data_out, return_type, E->e->u.linterp.complex_table,
      data_in, n_read, E->e->u.linterp.lut, E->e->u.linterp.table_len);

  free(data_in);
  dreturn("%zu", n_read);
  return n_read;
}

/* _GD_DoPolynom:  Read from a polynom.  Returns number of samples read.
*/
static size_t _GD_DoPolynom(DIRFILE *D, gd_entry_t *E, off64_t first_samp,
    size_t num_samp, gd_type_t return_type, void *data_out)
{
  size_t n_read;

  dtrace("%p, %p, %lli, %zu, 0x%x, %p", D, E, first_samp, num_samp, return_type,
      data_out);

  if (_GD_BadInput(D, E, 0)) {
    dreturn("%i", 0);
    return 0;
  }

  /* read the input field */
  n_read = _GD_DoField(D, E->e->entry[0], E->e->repr[0], first_samp, num_samp,
      return_type, data_out);

  if (D->error != GD_E_OK) {
    dreturn("%i", 0);
    return 0;
  }

  /* Nothing to polynomise */
  if (n_read == 0) {
    dreturn("%i", 0);
    return 0;
  }

  if (E->comp_scal)
    _GD_CPolynomData(D, data_out, return_type, n_read, E->EN(polynom,poly_ord),
        E->EN(polynom,ca));
  else
    _GD_PolynomData(D, data_out, return_type, n_read, E->EN(polynom,poly_ord),
        E->EN(polynom,a));

  dreturn("%zu", n_read);
  return n_read;
}

/* _GD_DoConst:  Read from a const.  Returns number of samples read (ie. 1).
*/
static size_t _GD_DoConst(DIRFILE *D, const gd_entry_t *E, off64_t first,
    size_t len, gd_type_t return_type, void *data_out)
{
  gd_type_t type;

  dtrace("%p, %p, %lli, %zu, 0x%x, %p", D, E, first, len, return_type,
      data_out);

  type = _GD_ConstType(D, E->EN(scalar,const_type));
  _GD_ConvertType(D, (char *)E->e->u.scalar.d + first * GD_SIZE(type), type,
      data_out, return_type, len);

  if (D->error) { /* bad input type */
    dreturn("%i", 0);
    return 0;
  }

  dreturn("%i", 1);
  return 1;
}

/* _GD_DoString:  Read from a string.  Returns number of samples read (ie. the
 * length of the string plus 1).
 */
static size_t _GD_DoString(gd_entry_t *E, size_t num_samp, char *data_out)
{
  dtrace("%p, %zu, %p", E, num_samp, data_out);

  if (num_samp > 0 && data_out != NULL)
    strncpy(data_out, E->e->u.string, num_samp); 

  dreturn("%zu", strlen(E->e->u.string) + 1);
  return strlen(E->e->u.string) + 1;
}

/* _GD_DoField: Locate the field in the database and read it.
*/
size_t _GD_DoField(DIRFILE *D, gd_entry_t *E, int repr, off64_t first_samp,
    size_t num_samp, gd_type_t return_type, void *data_out)
{
  size_t n_read = 0;
  gd_type_t ntype;
  void *true_data_out = data_out;
  const gd_type_t true_return_type = return_type; 
  int out_of_place = 0;

  dtrace("%p, %p(%s), %i, %lli, %zu, 0x%x, %p", D, E, E->field, repr,
      first_samp, num_samp, return_type, data_out);

  if (++D->recurse_level >= GD_MAX_RECURSE_LEVEL) {
    _GD_SetError(D, GD_E_RECURSE_LEVEL, 0, NULL, 0, E->field);
    D->recurse_level--;
    dreturn("%i", 0);
    return 0;
  }

  if (!E->e->calculated)
    _GD_CalculateEntry(D, E);

  if (D->error) {
    dreturn("%i", 0);
    return 0;
  }

  /* calculate the native type */
  ntype = _GD_NativeType(D, E, GD_REPR_NONE); 

  if (D->error) {
    dreturn("%i", 0);
    return 0;
  }

  if (first_samp == GD_HERE) {
    first_samp = _GD_GetFilePos(D, E, -1);
    if (D->error) {
      dreturn("%i", 0);
      return 0;
    }
  }

  /* short circuit for purely real native types */
  if (~ntype & GD_COMPLEX) {
    if (repr == GD_REPR_IMAG) {
      memset(data_out, 0, GD_SIZE(return_type) * num_samp);
      dreturn("%zu", num_samp);
      return num_samp;
    } else if (repr == GD_REPR_REAL)
      repr = GD_REPR_NONE;
  }

  /* if the native type is complex valued, but our return type is purely real,
   * we compute the field out-of-place, and then cast it to the return type
   * later, otherwise we just compute things in-place and don't worry too much
   * about accuracy */
  if (ntype & GD_COMPLEX && ~return_type & GD_COMPLEX) {
    out_of_place = 1;
    return_type = GD_COMPLEX128;
    data_out = _GD_Alloc(D, GD_COMPLEX128, num_samp);
    if (repr == GD_REPR_NONE)
      repr = GD_REPR_AUTO;
  }

  switch (E->field_type) {
    case GD_RAW_ENTRY:
      n_read = _GD_DoRaw(D, E, first_samp, num_samp, return_type, data_out);
      break;
    case GD_LINTERP_ENTRY:
      n_read = _GD_DoLinterp(D, E, first_samp, num_samp, return_type, data_out);
      break;
    case GD_LINCOM_ENTRY:
      n_read = _GD_DoLincom(D, E, first_samp, num_samp, return_type, data_out);
      break;
    case GD_BIT_ENTRY:
      n_read = _GD_DoBit(D, E, 0, first_samp, num_samp, return_type, data_out);
      break;
    case GD_RECIP_ENTRY:
      n_read = _GD_DoRecip(D, E, first_samp, num_samp, return_type, data_out);
      break;
    case GD_DIVIDE_ENTRY:
      n_read = _GD_DoDivide(D, E, first_samp, num_samp, return_type, data_out);
      break;
    case GD_MULTIPLY_ENTRY:
      n_read = _GD_DoMultiply(D, E, first_samp, num_samp, return_type,
          data_out);
      break;
    case GD_PHASE_ENTRY:
      n_read = _GD_DoPhase(D, E, first_samp, num_samp, return_type, data_out);
      break;
    case GD_INDEX_ENTRY:
      /* if Asking for "INDEX", just return it */
      _GD_FillFileFrame(data_out, return_type, first_samp, n_read = num_samp);
      break;
    case GD_POLYNOM_ENTRY:
      n_read = _GD_DoPolynom(D, E, first_samp, num_samp, return_type, data_out);
      break;
    case GD_SBIT_ENTRY:
      n_read = _GD_DoBit(D, E, 1, first_samp, num_samp, return_type, data_out);
      break;
    case GD_CONST_ENTRY:
    case GD_CARRAY_ENTRY:
      n_read = _GD_DoConst(D, E, first_samp, num_samp, return_type, data_out);
      break;
    case GD_STRING_ENTRY:
      n_read = _GD_DoString(E, num_samp, (char *)data_out);
      break;
    case GD_NO_ENTRY:
      /* Can't get here */
      _GD_InternalError(D);
      n_read = 0;
  }

  /* extract the requested representation */
  if (!D->error && repr != GD_REPR_NONE)
    _GD_ExtractRepr(D, data_out, return_type, true_data_out, true_return_type,
        n_read, repr);

  if (out_of_place)
    free(data_out);

  D->recurse_level--;
  dreturn("%zu", n_read);
  return n_read;
}

/* this function is little more than a public boilerplate for _GD_DoField */
size_t gd_getdata64(DIRFILE* D, const char *field_code_in, off64_t first_frame,
    off64_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t return_type, void *data_out)
{
  size_t n_read = 0;
  gd_entry_t* entry;
  char* field_code;
  int repr;
  gd_spf_t spf;

  dtrace("%p, \"%s\", %lli, %lli, %zu, %zu, 0x%x, %p", D, field_code_in,
      first_frame, first_samp, num_frames, num_samp, return_type, data_out);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  _GD_ClearError(D);

  entry = _GD_FindFieldAndRepr(D, field_code_in, &field_code, &repr, NULL, 1);

  if (D->error) {
    dreturn("%i", 0);
    return 0;
  }

  if (entry->field_type & GD_SCALAR_ENTRY)
    _GD_SetError(D, GD_E_DIMENSION, GD_E_DIM_CALLER, NULL, 0, field_code);

  if (field_code != field_code_in)
    free(field_code);

  if (D->error) {
    dreturn("%i", 0);
    return 0;
  }

  if (first_frame == GD_HERE || first_samp == GD_HERE) {
    first_samp = GD_HERE;
    first_frame = 0;
  }

  if (first_frame > 0 || num_frames > 0) {
    /* get the samples per frame */
    spf = _GD_GetSPF(D, entry);

    if (D->error) {
      dreturn("%i", 0);
      return 0;
    }

    first_samp += spf * first_frame;
    num_samp += spf * num_frames;
  }

  if (first_samp < 0 && (first_samp != GD_HERE || first_frame != 0)) {
    _GD_SetError(D, GD_E_RANGE, GD_E_OUT_OF_RANGE, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  n_read = _GD_DoField(D, entry, repr, first_samp, num_samp, return_type,
      data_out);

  dreturn("%zu", n_read);
  return n_read;
}

/* 32(ish)-bit wrapper for the 64-bit version, when needed */
size_t gd_getdata(DIRFILE* D, const char *field_code, off_t first_frame,
    off_t first_samp, size_t num_frames, size_t num_samp,
    gd_type_t return_type, void *data_out)
{
  return gd_getdata64(D, field_code, first_frame, first_samp, num_frames,
      num_samp, return_type, data_out);
}
/* vim: ts=2 sw=2 et tw=80
*/
