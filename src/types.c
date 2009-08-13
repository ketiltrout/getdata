/* (C) 2002-2005 C. Barth Netterfield
 * (C) 2005-2008 D. V. Wiebe
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
#include <math.h>
#include <stdlib.h>
#include <string.h>
#endif

/* _GD_ConvertType: copy data to output buffer while converting type.
*/
void _GD_ConvertType(DIRFILE* D, const void *data_in, gd_type_t in_type,
    void *data_out, gd_type_t out_type, size_t n)
{
  size_t i;

  dtrace("%p, %p, 0x%x, %p, 0x%x, %zi", D, data_in, in_type, data_out, out_type,
      n);
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
            ((uint8_t*)data_out)[i] = (uint8_t)((int8_t*)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t*)data_out)[i] = (int16_t)((int8_t*)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t*)data_out)[i] = (uint16_t)((int8_t*)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t*)data_out)[i] = (int32_t)((int8_t*)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t*)data_out)[i] = (uint32_t)((int8_t*)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t*)data_out)[i] = (int64_t)((int8_t*)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t*)data_out)[i] = (uint64_t)((int8_t*)data_in)[i];
          return;
        case GD_FLOAT:
          for (i = 0; i < n; i++)
            ((float*)data_out)[i] = (float)((int8_t*)data_in)[i];
          return;
        case GD_DOUBLE:
          for (i = 0; i < n; i++)
            ((double*)data_out)[i] = (double)((int8_t*)data_in)[i];
          return;
        default:
          _GD_SetError(D, GD_E_BAD_TYPE, out_type, NULL, 0, NULL);
          break;
      }
      break;
    case GD_UINT8:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t*)data_out)[i] = (int8_t)((uint8_t*)data_in)[i];
          return;
        case GD_UINT8:
          memcpy(data_out, data_in, n * sizeof(uint8_t));
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t*)data_out)[i] = (int16_t)((uint8_t*)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t*)data_out)[i] = (uint16_t)((uint8_t*)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t*)data_out)[i] = (int32_t)((uint8_t*)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t*)data_out)[i] = (uint32_t)((uint8_t*)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t*)data_out)[i] = (int64_t)((uint8_t*)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t*)data_out)[i] = (uint64_t)((uint8_t*)data_in)[i];
          return;
        case GD_FLOAT:
          for (i = 0; i < n; i++)
            ((float*)data_out)[i] = (float)((uint8_t*)data_in)[i];
          return;
        case GD_DOUBLE:
          for (i = 0; i < n; i++)
            ((double*)data_out)[i] = (double)((uint8_t*)data_in)[i];
          return;
        default:
          _GD_SetError(D, GD_E_BAD_TYPE, out_type, NULL, 0, NULL);
          break;
      }
      break;
    case GD_INT16:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t*)data_out)[i] = (int8_t)((int16_t*)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t*)data_out)[i] = (uint8_t)((int16_t*)data_in)[i];
          return;
        case GD_INT16:
          memcpy(data_out, data_in, n * sizeof(int16_t));
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t*)data_out)[i] = (uint16_t)((int16_t*)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t*)data_out)[i] = (int32_t)((int16_t*)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t*)data_out)[i] = (uint32_t)((uint16_t*)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t*)data_out)[i] = (int64_t)((int16_t*)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t*)data_out)[i] = (uint64_t)((uint16_t*)data_in)[i];
          return;
        case GD_FLOAT:
          for (i = 0; i < n; i++)
            ((float*)data_out)[i] = (float)((uint16_t*)data_in)[i];
          return;
        case GD_DOUBLE:
          for (i = 0; i < n; i++)
            ((double*)data_out)[i] = (double)((uint16_t*)data_in)[i];
          return;
        default:
          _GD_SetError(D, GD_E_BAD_TYPE, out_type, NULL, 0, NULL);
          break;
      }
      break;
    case GD_UINT16:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t*)data_out)[i] = (int8_t)((uint16_t*)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t*)data_out)[i] = (uint8_t)((uint16_t*)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t*)data_out)[i] = (int16_t)((uint16_t*)data_in)[i];
          return;
        case GD_UINT16:
          memcpy(data_out, data_in, n * sizeof(uint16_t));
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t*)data_out)[i] = (int32_t)((uint16_t*)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t*)data_out)[i] = (uint32_t)((uint16_t*)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t*)data_out)[i] = (int64_t)((uint16_t*)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t*)data_out)[i] = (uint64_t)((uint16_t*)data_in)[i];
          return;
        case GD_FLOAT:
          for (i = 0; i < n; i++)
            ((float*)data_out)[i] = (float)((uint16_t*)data_in)[i];
          return;
        case GD_DOUBLE:
          for (i = 0; i < n; i++)
            ((double*)data_out)[i] = (double)((uint16_t*)data_in)[i];
          return;
        default:
          _GD_SetError(D, GD_E_BAD_TYPE, out_type, NULL, 0, NULL);
          break;
      }
      break;
    case GD_INT32:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t*) data_out)[i] = (int8_t)((int32_t*)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t*) data_out)[i] = (uint8_t)((int32_t*)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t*)data_out)[i] = (int16_t)((int32_t*)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t*)data_out)[i] = (uint16_t)((int32_t*)data_in)[i];
          return;
        case GD_INT32:
          memcpy(data_out, data_in, n * sizeof(int32_t));
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t*)data_out)[i] = (uint32_t)((int32_t*)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t*)data_out)[i] = (int64_t)((int32_t*)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t*)data_out)[i] = (uint64_t)((int32_t*)data_in)[i];
          return;
        case GD_FLOAT:
          for (i = 0; i < n; i++)
            ((float*)data_out)[i] = (float)((int32_t*)data_in)[i];
          return;
        case GD_DOUBLE:
          for (i = 0; i < n; i++)
            ((double*)data_out)[i] = (double)((int32_t*)data_in)[i];
          return;
        default:
          _GD_SetError(D, GD_E_BAD_TYPE, out_type, NULL, 0, NULL);
          break;
      }
      break;
    case GD_UINT32:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t*) data_out)[i] = (int8_t)((uint32_t*)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t*) data_out)[i] = (uint8_t)((uint32_t*)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t*)data_out)[i] = (int16_t)((uint32_t*)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t*)data_out)[i] = (uint16_t)((uint32_t*)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t*)data_out)[i] = (int32_t)((uint32_t*)data_in)[i];
          return;
        case GD_UINT32:
          memcpy(data_out, data_in, n * sizeof(uint32_t));
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t*)data_out)[i] = (int64_t)((uint32_t*)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t*)data_out)[i] = (uint64_t)((uint32_t*)data_in)[i];
          return;
        case GD_FLOAT:
          for (i = 0; i < n; i++)
            ((float*)data_out)[i] = (float)((uint32_t*)data_in)[i];
          return;
        case GD_DOUBLE:
          for (i = 0; i < n; i++)
            ((double*)data_out)[i] = (double)((uint32_t*)data_in)[i];
          return;
        default:
          break;
      }
      break;
    case GD_INT64:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t*) data_out)[i] = (int64_t)((int64_t*)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t*) data_out)[i] = (uint8_t)((int64_t*)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t*)data_out)[i] = (int16_t)((int64_t*)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t*)data_out)[i] = (uint16_t)((int64_t*)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t*)data_out)[i] = (int32_t)((int64_t*)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t*)data_out)[i] = (uint32_t)((int64_t*)data_in)[i];
          return;
        case GD_INT64:
          memcpy(data_out, data_in, n * sizeof(int64_t));
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t*)data_out)[i] = (uint64_t)((int64_t*)data_in)[i];
          return;
        case GD_FLOAT:
          for (i = 0; i < n; i++)
            ((float*)data_out)[i] = (float)((int64_t*)data_in)[i];
          return;
        case GD_DOUBLE:
          for (i = 0; i < n; i++)
            ((double*)data_out)[i] = (double)((int64_t*)data_in)[i];
          return;
        default:
          _GD_SetError(D, GD_E_BAD_TYPE, out_type, NULL, 0, NULL);
          break;
      }
      break;
    case GD_UINT64:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t*) data_out)[i] = (int8_t)((uint64_t*)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t*) data_out)[i] = (uint8_t)((uint64_t*)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t*)data_out)[i] = (int16_t)((uint64_t*)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t*)data_out)[i] = (uint16_t)((uint64_t*)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t*)data_out)[i] = (int32_t)((uint64_t*)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t*)data_out)[i] = (uint32_t)((uint64_t*)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t*)data_out)[i] = (int64_t)((uint64_t*)data_in)[i];
          return;
        case GD_UINT64:
          memcpy(data_out, data_in, n * sizeof(uint64_t));
          return;
        case GD_FLOAT:
          for (i = 0; i < n; i++)
            ((float*)data_out)[i] = (float)((uint64_t*)data_in)[i];
          return;
        case GD_DOUBLE:
          for (i = 0; i < n; i++)
            ((double*)data_out)[i] = (double)((uint64_t*)data_in)[i];
          return;
        default:
          _GD_SetError(D, GD_E_BAD_TYPE, out_type, NULL, 0, NULL);
          break;
      }
      break;
    case GD_FLOAT:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t*) data_out)[i] = (int8_t)((float*)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t*) data_out)[i] = (uint8_t)((float*)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t*)data_out)[i] = (int16_t)((float*)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t*)data_out)[i] = (uint16_t)((float*)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t*)data_out)[i] = (int32_t)((float*)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t*)data_out)[i] = (uint32_t)((float*)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t*)data_out)[i] = (int64_t)((float*)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t*)data_out)[i] = (uint64_t)((float*)data_in)[i];
          return;
        case GD_FLOAT:
          memcpy(data_out, data_in, n * sizeof(float));
          return;
        case GD_DOUBLE:
          for (i = 0; i < n; i++)
            ((double*)data_out)[i] = (double)((float*)data_in)[i];
          return;
        default:
          _GD_SetError(D, GD_E_BAD_TYPE, out_type, NULL, 0, NULL);
          break;
      }
      break;
    case GD_DOUBLE:
      switch (out_type) {
        case GD_INT8:
          for (i = 0; i < n; i++)
            ((int8_t*) data_out)[i] = (int8_t)((double*)data_in)[i];
          return;
        case GD_UINT8:
          for (i = 0; i < n; i++)
            ((uint8_t*) data_out)[i] = (uint8_t)((double*)data_in)[i];
          return;
        case GD_INT16:
          for (i = 0; i < n; i++)
            ((int16_t*)data_out)[i] = (int16_t)((double*)data_in)[i];
          return;
        case GD_UINT16:
          for (i = 0; i < n; i++)
            ((uint16_t*)data_out)[i] = (uint16_t)((double*)data_in)[i];
          return;
        case GD_INT32:
          for (i = 0; i < n; i++)
            ((int32_t*)data_out)[i] = (int32_t)((double*)data_in)[i];
          return;
        case GD_UINT32:
          for (i = 0; i < n; i++)
            ((uint32_t*)data_out)[i] = (int32_t)((double*)data_in)[i];
          return;
        case GD_INT64:
          for (i = 0; i < n; i++)
            ((int64_t*)data_out)[i] = (int64_t)((double*)data_in)[i];
          return;
        case GD_UINT64:
          for (i = 0; i < n; i++)
            ((uint64_t*)data_out)[i] = (uint64_t)((double*)data_in)[i];
          return;
        case GD_FLOAT:
          for (i = 0; i < n; i++)
            ((float*)data_out)[i] = (float)((double*)data_in)[i];
          return;
        case GD_DOUBLE:
          memcpy(data_out, data_in, n * sizeof(double));
          return;
        default:
          _GD_SetError(D, GD_E_BAD_TYPE, out_type, NULL, 0, NULL);
          break;
      }
      break;
    default:
      _GD_SetError(D, GD_E_BAD_TYPE, in_type, NULL, 0, NULL);
      break;
  }
}

/* vim: ts=2 sw=2 et tw=80
 */
