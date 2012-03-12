/* Copyright (C) 2009-2011 D. V. Wiebe
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

#include "nan.h"

static double _GD_Extrapolate(DIRFILE *D, gd_entry_t *E, int repr, double value,
    off64_t limit, int eof)
{
  off64_t n;
  double sample = NAN;
  double data[2];

  dtrace("%p, %p, %i, %g, %lli, %i", D, E, repr, value, (long long)limit, eof);

  /* load data */
  n = _GD_DoField(D, E, repr, limit - eof, 2, GD_FLOAT64, data);

  if (D->error) {
    dreturn("%g", sample);
    return sample;
  } else if (n < 2) {
    _GD_SetError(D, GD_E_DOMAIN, GD_E_DOMAIN_EMPTY, NULL, 0, NULL);
    dreturn("%g", sample);
    return sample;
  }

  sample = limit + (value - data[eof]) / (data[1] - data[0]);
  dreturn("%g", sample);
  return sample;
}

/* find the (fractional) frame number based on a monotonic look-up */
static double _GD_GetIndex(DIRFILE* D, gd_entry_t *E, int repr, double value,
    off64_t field_start, off64_t field_end)
{
  double sample = NAN;
  int dir = -1; /* -1 = unknown; 0 = ascending; 1 = descending */
  off64_t low = field_start;
  off64_t high = field_end;
  off64_t c;
  double low_v, high_v, field_start_v, c_v;
  size_t n;

  dtrace("%p, %p, %i, %g, %lli, %lli", D, E, repr, value,
      (long long)field_start, (long long)field_end);

  /* find the end-points */
  n = _GD_DoField(D, E, repr, field_start, 1, GD_FLOAT64, &low_v);
  field_start_v = low_v;

  if (D->error) {
    dreturn("%g", sample);
    return sample;
  }

  if (n == 0) {
    _GD_SetError(D, GD_E_DOMAIN, GD_E_DOMAIN_EMPTY, NULL, 0, NULL);
    dreturn("%g", sample);
    return sample;
  }

  n = _GD_DoField(D, E, repr, field_end - 1, 1, GD_FLOAT64, &high_v);

  if (D->error) {
    dreturn("%g", sample);
    return sample;
  }

  if (n > 0) {
    if (high_v == low_v) {
      _GD_SetError(D, GD_E_RANGE, GD_E_SINGULAR_RANGE, NULL, 0, NULL);
      dreturn("%g", sample);
      return sample;
    }

    dir = (high_v < low_v) ? 1 : 0;

    /* extrapolate, if necessary */
    if ((!dir && low_v > value) || (dir && low_v < value)) {
      sample = _GD_Extrapolate(D, E, repr, value, low, 0);
      dreturn("%g", sample);
      return sample;
    } else if ((!dir && high_v < value) || (dir && high_v > value)) {
      sample = _GD_Extrapolate(D, E, repr, value, high - 1, 1);
      dreturn("%g", sample);
      return sample;
    }
  } else {
    /* binary search until either we find the end or we find a subdomain in
     * which our value lies */
    for (;;) {
      c = (high + low) / 2;
      n = _GD_DoField(D, E, repr, c, 1, GD_FLOAT64, &c_v);

      if (D->error) {
        dreturn("%g", sample);
        return sample;
      }

      if (n == 0) {
        if (c - low == 1) {
          if (low == field_start) {
            _GD_SetError(D, GD_E_DOMAIN, GD_E_DOMAIN_EMPTY, NULL, 0, NULL);
            dreturn("%g", sample);
            return sample;
          }

          /* low is the EOF -- so, extrapolate */
          sample = _GD_Extrapolate(D, E, repr, value, low - 1, 1);
          dreturn("%g", sample);
          return sample;
        } else {
          /* haven't found the EOF yet */
          high = c;
        }
      } else {
        if (dir == -1) {
          if (c_v == low_v) {
            /* in this case, the range may not be singular, so use our guess
             * as the new lower limit and keep looking */
            low = c;
            continue;
          }

          dir = (c_v < field_start_v) ? 1 : 0;

          if ((!dir && field_start_v > value) ||
              (dir && field_start_v < value))
          {
            /* extrapolate BOF */
            sample = _GD_Extrapolate(D, E, repr, value, field_start_v, 0);
            dreturn("%g", sample);
            return sample;
          }
        }
        if ((!dir && c_v > value) || (dir && c_v < value)) {
          /* below our guess -- finding the end is no longer relevant */
          high = c;
          high_v = c_v;
          break;
        } else if ((!dir && c_v < value) || (dir && c_v > value)) {
          /* above our guess -- still need to look for the end */
          low = c;
          low_v = c_v;
        }
      }
    }
  }

  /* Step 2: binary search until we find the value */
  for (;high - low > 1;) {
    /* load data */
    c = (high + low) / 2;
    n = _GD_DoField(D, E, repr, c, 1, GD_FLOAT64, &c_v);

    if (D->error) {
      dreturn("%g", sample);
      return sample;
    }

    if (n == 0) {
      /* someone's been stealing our data.  How Rude! */
      _GD_SetError(D, GD_E_DOMAIN, GD_E_DOMAIN_EMPTY, NULL, 0, NULL);
      dreturn("%g", sample);
      return sample;
    }

    if ((!dir && c_v > value) || (dir && c_v < value)) {
      /* before our guess */
      high_v = c_v;
      high = c;
    } else if ((!dir && c_v < value) || (dir && c_v > value)) {
      /* after our guess */
      low_v = c_v;
      low = c;
    } else {
      /* our guess was unexpectedly correct */
      sample = (double)c;
      dreturn("%g", sample);
      return sample;
    }
  }

  sample = low + (value - low_v) / (high_v - low_v);
  dreturn("%g", sample);
  return sample;
}

double gd_framenum_subset64(DIRFILE* D, const char* field_code_in,
    double value, off64_t field_start, off64_t field_end)
{
  double frame = NAN;
  gd_entry_t* entry;
  char* field_code;
  int repr = GD_REPR_NONE;
  gd_spf_t spf;

  dtrace("%p, \"%s\", %g, %lli, %lli", D, field_code_in, value,
      (long long)field_start, (long long)field_end);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%g", frame);
    return frame;
  }

  _GD_ClearError(D);

  entry = _GD_FindFieldAndRepr(D, field_code_in, &field_code, &repr, NULL, 1,
      1);

  if (D->error) {
    dreturn("%g", frame);
    return frame;
  }

  if (_GD_NativeType(D, entry, repr) & GD_COMPLEX)
    _GD_SetError(D, GD_E_DOMAIN, GD_E_DOMAIN_COMPLEX, NULL, 0, NULL);
  else if (entry->field_type & GD_SCALAR_ENTRY)
    _GD_SetError(D, GD_E_DIMENSION, GD_E_DIM_CALLER, NULL, 0, field_code);

  if (field_code != field_code_in)
    free(field_code);

  if (D->error) {
    dreturn("%g", frame);
    return frame;
  }

  spf = _GD_GetSPF(D, entry);
  if (field_start == 0)
    field_start = D->fragment[entry->fragment_index].frame_offset * spf;
  else
    field_start *= spf;

  if (field_end == 0)
    field_end = (gd_nframes64(D) + 1) * spf - 1;
  else
    field_end = field_end * spf - 1;

  if (field_end - field_start < 2)
    _GD_SetError(D, GD_E_DOMAIN, GD_E_DOMAIN_EMPTY, NULL, 0, NULL);

  if (!D->error)
    frame = _GD_GetIndex(D, entry, repr, value, field_start, field_end) / spf;

  dreturn("%g", frame);
  return frame;
}

double gd_framenum_subset(DIRFILE* D, const char* field_code, double value,
    off_t field_start, off_t field_end)
{
  double frame;

  dtrace("%p, \"%s\", %g, %lli, %lli", D, field_code, value,
      (long long int)field_start, (long long int)field_end);

  frame = gd_framenum_subset64(D, field_code,  value, (off64_t)field_start,
      (off64_t)field_end);

  dreturn("%g", frame);
  return frame;
}

double gd_framenum(DIRFILE* D, const char* field_code, double value)
{
  double frame;

  dtrace("%p, \"%s\", %g", D, field_code, value);

  frame = gd_framenum_subset64(D, field_code,  value, 0, 0);

  dreturn("%g", frame);
  return frame;
}
/* vim: ts=2 sw=2 et tw=80
*/
