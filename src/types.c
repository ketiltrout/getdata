/* Copyright (C) 2002-2005 C. Barth Netterfield
 * Copyright (C) 2005-2012, 2014, 2015, 2016 D. V. Wiebe
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

#ifdef GD_NO_C99_API

#define TO_COMPLEX(ot,it) \
  do { \
    for (i = 0; i < n; i++) { \
      ((ot *)data_out)[2 * i] = (ot)((it *)data_in)[i]; \
      ((ot *)data_out)[2 * i + 1] = 0; \
    } \
  } while (0)
#define FROM_COMPLEX(ot,it) \
  do { \
    for (i = 0; i < n; i++) { \
      ((ot *)data_out)[i] = (ot)((it *)data_in)[2 * i]; \
    } \
  } while (0)

#else

#define TO_COMPLEX(ot,it) \
  do { \
    for (i = 0; i < n; i++) { \
      ((complex ot *)data_out)[i] = (complex ot)((it *)data_in)[i]; \
    } \
  } while (0)
#define FROM_COMPLEX(ot,it) \
  do { \
    for (i = 0; i < n; i++) { \
      ((ot *)data_out)[i] = (ot)((complex it *)data_in)[i]; \
    } \
  } while (0)

#endif


/* _GD_ConvertType: copy data to output buffer while converting type.
*/
void _GD_ConvertType(DIRFILE *restrict D, const void *restrict data_in,
    gd_type_t in_type, void *restrict data_out, gd_type_t out_type, size_t n)
gd_nothrow
{
  size_t i;

  dtrace("%p, %p, 0x%x, %p, 0x%x, %" PRIuSIZE, D, data_in, in_type, data_out,
      out_type, n);
  dreturnvoid();

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
            ((uint8_t *)data_out)[i] = (uint8_t)((int8_t *)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t *)data_out)[i] = (int16_t)((int8_t *)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t *)data_out)[i] = (uint16_t)((int8_t *)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t *)data_out)[i] = (int32_t)((int8_t *)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t *)data_out)[i] = (uint32_t)((int8_t *)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t *)data_out)[i] = (int64_t)((int8_t *)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t *)data_out)[i] = (uint64_t)((int8_t *)data_in)[i];
          return;
        case GD_FLOAT32:
          for (i = 0; i < n; i++)
            ((float *)data_out)[i] = (float)((int8_t *)data_in)[i];
          return;
        case GD_FLOAT64:
          for (i = 0; i < n; i++)
            ((double *)data_out)[i] = (double)((int8_t *)data_in)[i];
          return;
        case GD_COMPLEX64:
          TO_COMPLEX(float,int8_t);
          return;
        case GD_COMPLEX128:
          TO_COMPLEX(double,int8_t);
          return;
        default:
          _GD_InternalError(D);
          break;
      }
      break;
    case GD_UINT8:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t *)data_out)[i] = (int8_t)((uint8_t *)data_in)[i];
          return;
        case GD_UINT8:
          memcpy(data_out, data_in, n * sizeof(uint8_t));
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t *)data_out)[i] = (int16_t)((uint8_t *)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t *)data_out)[i] = (uint16_t)((uint8_t *)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t *)data_out)[i] = (int32_t)((uint8_t *)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t *)data_out)[i] = (uint32_t)((uint8_t *)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t *)data_out)[i] = (int64_t)((uint8_t *)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t *)data_out)[i] = (uint64_t)((uint8_t *)data_in)[i];
          return;
        case GD_FLOAT32:
          for (i = 0; i < n; i++)
            ((float *)data_out)[i] = (float)((uint8_t *)data_in)[i];
          return;
        case GD_FLOAT64:
          for (i = 0; i < n; i++)
            ((double *)data_out)[i] = (double)((uint8_t *)data_in)[i];
          return;
        case GD_COMPLEX64:
          TO_COMPLEX(float,uint8_t);
          return;
        case GD_COMPLEX128:
          TO_COMPLEX(double,uint8_t);
          return;
        default:
          _GD_InternalError(D);
          break;
      }
      break;
    case GD_INT16:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t *)data_out)[i] = (int8_t)((int16_t *)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t *)data_out)[i] = (uint8_t)((int16_t *)data_in)[i];
          return;
        case GD_INT16:
          memcpy(data_out, data_in, n * sizeof(int16_t));
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t *)data_out)[i] = (uint16_t)((int16_t *)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t *)data_out)[i] = (int32_t)((int16_t *)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t *)data_out)[i] = (uint32_t)((uint16_t *)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t *)data_out)[i] = (int64_t)((int16_t *)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((int64_t *)data_out)[i] = (uint64_t)((int16_t *)data_in)[i];
          return;
        case GD_FLOAT32:
          for (i = 0; i < n; i++)
            ((float *)data_out)[i] = (float)((int16_t *)data_in)[i];
          return;
        case GD_FLOAT64:
          for (i = 0; i < n; i++)
            ((double *)data_out)[i] = (double)((int16_t *)data_in)[i];
          return;
        case GD_COMPLEX64:
          TO_COMPLEX(float,int16_t);
          return;
        case GD_COMPLEX128:
          TO_COMPLEX(double,int16_t);
          return;
        default:
          _GD_InternalError(D);
          break;
      }
      break;
    case GD_UINT16:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t *)data_out)[i] = (int8_t)((uint16_t *)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t *)data_out)[i] = (uint8_t)((uint16_t *)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t *)data_out)[i] = (int16_t)((uint16_t *)data_in)[i];
          return;
        case GD_UINT16:
          memcpy(data_out, data_in, n * sizeof(uint16_t));
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t *)data_out)[i] = (int32_t)((uint16_t *)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t *)data_out)[i] = (uint32_t)((uint16_t *)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t *)data_out)[i] = (int64_t)((uint16_t *)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t *)data_out)[i] = (uint64_t)((uint16_t *)data_in)[i];
          return;
        case GD_FLOAT32:
          for (i = 0; i < n; i++)
            ((float *)data_out)[i] = (float)((uint16_t *)data_in)[i];
          return;
        case GD_FLOAT64:
          for (i = 0; i < n; i++)
            ((double *)data_out)[i] = (double)((uint16_t *)data_in)[i];
          return;
        case GD_COMPLEX64:
          TO_COMPLEX(float,uint16_t);
          return;
        case GD_COMPLEX128:
          TO_COMPLEX(double,uint16_t);
          return;
        default:
          _GD_InternalError(D);
          break;
      }
      break;
    case GD_INT32:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t *) data_out)[i] = (int8_t)((int32_t *)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t *) data_out)[i] = (uint8_t)((int32_t *)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t *)data_out)[i] = (int16_t)((int32_t *)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t *)data_out)[i] = (uint16_t)((int32_t *)data_in)[i];
          return;
        case GD_INT32:
          memcpy(data_out, data_in, n * sizeof(int32_t));
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t *)data_out)[i] = (uint32_t)((int32_t *)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t *)data_out)[i] = (int64_t)((int32_t *)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t *)data_out)[i] = (uint64_t)((int32_t *)data_in)[i];
          return;
        case GD_FLOAT32:
          for (i = 0; i < n; i++)
            ((float *)data_out)[i] = (float)((int32_t *)data_in)[i];
          return;
        case GD_FLOAT64:
          for (i = 0; i < n; i++)
            ((double *)data_out)[i] = (double)((int32_t *)data_in)[i];
          return;
        case GD_COMPLEX64:
          TO_COMPLEX(float,int32_t);
          return;
        case GD_COMPLEX128:
          TO_COMPLEX(double,int32_t);
          return;
        default:
          _GD_InternalError(D);
          break;
      }
      break;
    case GD_UINT32:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t *) data_out)[i] = (int8_t)((uint32_t *)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t *) data_out)[i] = (uint8_t)((uint32_t *)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t *)data_out)[i] = (int16_t)((uint32_t *)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t *)data_out)[i] = (uint16_t)((uint32_t *)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t *)data_out)[i] = (int32_t)((uint32_t *)data_in)[i];
          return;
        case GD_UINT32:
          memcpy(data_out, data_in, n * sizeof(uint32_t));
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t *)data_out)[i] = (int64_t)((uint32_t *)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t *)data_out)[i] = (uint64_t)((uint32_t *)data_in)[i];
          return;
        case GD_FLOAT32:
          for (i = 0; i < n; i++)
            ((float *)data_out)[i] = (float)((uint32_t *)data_in)[i];
          return;
        case GD_FLOAT64:
          for (i = 0; i < n; i++)
            ((double *)data_out)[i] = (double)((uint32_t *)data_in)[i];
          return;
        case GD_COMPLEX64:
          TO_COMPLEX(float,uint32_t);
          return;
        case GD_COMPLEX128:
          TO_COMPLEX(double,uint32_t);
          return;
        default:
          break;
      }
      break;
    case GD_INT64:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t *) data_out)[i] = (int64_t)((int64_t *)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t *) data_out)[i] = (uint8_t)((int64_t *)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t *)data_out)[i] = (int16_t)((int64_t *)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t *)data_out)[i] = (uint16_t)((int64_t *)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t *)data_out)[i] = (int32_t)((int64_t *)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t *)data_out)[i] = (uint32_t)((int64_t *)data_in)[i];
          return;
        case GD_INT64:
          memcpy(data_out, data_in, n * sizeof(int64_t));
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t *)data_out)[i] = (uint64_t)((int64_t *)data_in)[i];
          return;
        case GD_FLOAT32:
          for (i = 0; i < n; i++)
            ((float *)data_out)[i] = (float)((int64_t *)data_in)[i];
          return;
        case GD_FLOAT64:
          for (i = 0; i < n; i++)
            ((double *)data_out)[i] = (double)((int64_t *)data_in)[i];
          return;
        case GD_COMPLEX64:
          TO_COMPLEX(float,int64_t);
          return;
        case GD_COMPLEX128:
          TO_COMPLEX(double,int64_t);
          return;
        default:
          _GD_InternalError(D);
          break;
      }
      break;
    case GD_UINT64:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t *) data_out)[i] = (int8_t)((uint64_t *)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t *) data_out)[i] = (uint8_t)((uint64_t *)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t *)data_out)[i] = (int16_t)((uint64_t *)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t *)data_out)[i] = (uint16_t)((uint64_t *)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t *)data_out)[i] = (int32_t)((uint64_t *)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t *)data_out)[i] = (uint32_t)((uint64_t *)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t *)data_out)[i] = (int64_t)((uint64_t *)data_in)[i];
          return;
        case GD_UINT64:
          memcpy(data_out, data_in, n * sizeof(uint64_t));
          return;
        case GD_FLOAT32:
          for (i = 0; i < n; i++)
            ((float *)data_out)[i] = (float)((uint64_t *)data_in)[i];
          return;
        case GD_FLOAT64:
          for (i = 0; i < n; i++)
            ((double *)data_out)[i] = (double)((uint64_t *)data_in)[i];
          return;
        case GD_COMPLEX64:
          TO_COMPLEX(float,uint64_t);
          return;
        case GD_COMPLEX128:
          TO_COMPLEX(double,uint64_t);
          return;
        default:
          _GD_InternalError(D);
          break;
      }
      break;
    case GD_FLOAT32:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t *) data_out)[i] = (int8_t)((float *)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t *) data_out)[i] = (uint8_t)((float *)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t *)data_out)[i] = (int16_t)((float *)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t *)data_out)[i] = (uint16_t)((float *)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t *)data_out)[i] = (int32_t)((float *)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t *)data_out)[i] = (uint32_t)((float *)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t *)data_out)[i] = (int64_t)((float *)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t *)data_out)[i] = (uint64_t)((float *)data_in)[i];
          return;
        case GD_FLOAT32:
          memcpy(data_out, data_in, n * sizeof(float));
          return;
        case GD_FLOAT64:
          for (i = 0; i < n; i++)
            ((double *)data_out)[i] = (double)((float *)data_in)[i];
          return;
        case GD_COMPLEX64:
          TO_COMPLEX(float,float);
          return;
        case GD_COMPLEX128:
          TO_COMPLEX(double,float);
          return;
        default:
          _GD_InternalError(D);
          break;
      }
      break;
    case GD_FLOAT64:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t *) data_out)[i] = (int8_t)((double *)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t *) data_out)[i] = (uint8_t)((double *)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t *)data_out)[i] = (int16_t)((double *)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t *)data_out)[i] = (uint16_t)((double *)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t *)data_out)[i] = (int32_t)((double *)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t *)data_out)[i] = (int32_t)((double *)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t *)data_out)[i] = (int64_t)((double *)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t *)data_out)[i] = (uint64_t)((double *)data_in)[i];
          return;
        case GD_FLOAT32:
          for (i = 0; i < n; i++)
            ((float *)data_out)[i] = (float)((double *)data_in)[i];
          return;
        case GD_FLOAT64:
          memcpy(data_out, data_in, n * sizeof(double));
          return;
        case GD_COMPLEX64:
          TO_COMPLEX(float,double);
          return;
        case GD_COMPLEX128:
          TO_COMPLEX(double,double);
          return;
        default:
          _GD_InternalError(D);
          break;
      }
      break;
    case GD_COMPLEX64:
      switch (out_type) {
        case GD_INT8:
          FROM_COMPLEX(int8_t,float);
          return;
        case GD_UINT8:
          FROM_COMPLEX(uint8_t,float);
          return;
        case GD_INT16:
          FROM_COMPLEX(int16_t,float);
          return;
        case GD_UINT16:
          FROM_COMPLEX(uint16_t,float);
          return;
        case GD_INT32:
          FROM_COMPLEX(int32_t,float);
          return;
        case GD_UINT32:
          FROM_COMPLEX(uint32_t,float);
          return;
        case GD_INT64:
          FROM_COMPLEX(int64_t,float);
          return;
        case GD_UINT64:
          FROM_COMPLEX(uint64_t,float);
          return;
        case GD_FLOAT32:
          FROM_COMPLEX(float,float);
          return;
        case GD_FLOAT64:
          FROM_COMPLEX(double,float);
          return;
        case GD_COMPLEX64:
          memcpy(data_out, data_in, n * 2 * sizeof(float));
          return;
        case GD_COMPLEX128:
          for (i = 0; i < 2 * n; i++)
            ((double *)data_out)[i] = (double)((float *)data_in)[i];
          return;
        default:
          _GD_InternalError(D);
          break;
      }
      break;
    case GD_COMPLEX128:
      switch (out_type) {
        case GD_INT8:
          FROM_COMPLEX(int8_t,double);
          return;
        case GD_UINT8:
          FROM_COMPLEX(uint8_t,double);
          return;
        case GD_INT16:
          FROM_COMPLEX(int16_t,double);
          return;
        case GD_UINT16:
          FROM_COMPLEX(uint16_t,double);
          return;
        case GD_INT32:
          FROM_COMPLEX(int32_t,double);
          return;
        case GD_UINT32:
          FROM_COMPLEX(uint32_t,double);
          return;
        case GD_INT64:
          FROM_COMPLEX(int64_t,double);
          return;
        case GD_UINT64:
          FROM_COMPLEX(uint64_t,double);
          return;
        case GD_FLOAT32:
          FROM_COMPLEX(float,double);
          return;
        case GD_FLOAT64:
          FROM_COMPLEX(double,double);
          return;
        case GD_COMPLEX64:
          for (i = 0; i < 2 * n; i++)
            ((float *)data_out)[i] = (float)((double *)data_in)[i];
          return;
        case GD_COMPLEX128:
          memcpy(data_out, data_in, 2 * n * sizeof(double));
          return;
        default:
          _GD_InternalError(D);
          break;
      }
      break;
    default:
      _GD_InternalError(D);
      break;
  }
}

/* vim: ts=2 sw=2 et tw=80
*/
