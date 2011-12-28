/* Copyright (C) 2008-2011 D. V. Wiebe
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

static void _GD_ShiftFragment(DIRFILE* D, off64_t offset, int fragment,
    int move)
{
  unsigned int i, n_raw = 0;

  dtrace("%p, %lli, %i, %i", D, (long long)offset, fragment, move);

  /* check protection */
  if (D->fragment[fragment].protection & GD_PROTECT_FORMAT) {
    _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[fragment].cname);
    dreturnvoid();
    return;
  }

  if (move && offset != D->fragment[fragment].frame_offset) {
    gd_entry_t **raw_entry = (gd_entry_t **)_GD_Malloc(D, sizeof(gd_entry_t*) *
        D->n_entries);

    if (raw_entry == NULL) {
      dreturnvoid();
      return;
    }

    /* Because it may fail, the move must occur out-of-place and then be copied
     * back over the affected files once success is assured */
    for (i = 0; i < D->n_entries; ++i)
      if (D->entry[i]->fragment_index == fragment &&
          D->entry[i]->field_type == GD_RAW_ENTRY)
      {
        /* determine encoding scheme */
        if (!_GD_Supports(D, D->entry[i], 0))
          break;

        /* add this raw field to the list */
        raw_entry[n_raw++] = D->entry[i];

        if (_GD_MogrifyFile(D, D->entry[i],
              D->fragment[D->entry[i]->fragment_index].encoding,
              D->fragment[D->entry[i]->fragment_index].byte_sex, offset, 0, -1,
              NULL))
          break;
      }

    /* If successful, move the temporary file over the old file, otherwise
     * remove the temporary files */
    if (D->error) {
      for (i = 0; i < n_raw; ++i)
        _GD_FiniRawIO(D, raw_entry[i], fragment, GD_FINIRAW_DISCARD |
            GD_FINIRAW_CLOTEMP);
    } else {
      for (i = 0; i < n_raw; ++i)
        _GD_FiniRawIO(D, raw_entry[i], fragment, GD_FINIRAW_KEEP |
            GD_FINIRAW_CLOTEMP);
    }

    free(raw_entry);

    if (D->error) {
      dreturnvoid();
      return;
    }
  }

  D->fragment[fragment].frame_offset = offset;
  D->fragment[fragment].modified = 1;
  D->flags &= ~GD_HAVE_VERSION;

  dreturnvoid();
}

int gd_alter_frameoffset64(DIRFILE* D, off64_t offset, int fragment, int move)
{
  int i;

  dtrace("%p, %lli, %i, %i", D, (long long)offset, fragment, move);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if ((D->flags & GD_ACCMODE) != GD_RDWR) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (fragment < GD_ALL_FRAGMENTS || fragment >= D->n_fragment) {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (offset < 0) {
    _GD_SetError(D, GD_E_RANGE, GD_E_OUT_OF_RANGE, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ClearError(D);

  if (fragment == GD_ALL_FRAGMENTS) {
    for (i = 0; i < D->n_fragment; ++i) {
      _GD_ShiftFragment(D, offset, i, move);

      if (D->error)
        break;
    }
  } else
    _GD_ShiftFragment(D, offset, fragment, move);

  dreturn("%i", (D->error) ? -1 : 0);
  return (D->error) ? -1 : 0;
}

off64_t gd_frameoffset64(DIRFILE* D, int fragment)
{
  dtrace("%p, %i", D, fragment);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (fragment < 0 || fragment >= D->n_fragment) {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ClearError(D);

  dreturn("%lli", (long long)D->fragment[fragment].frame_offset);
  return D->fragment[fragment].frame_offset;
}

/* 32(ish)-bit wrappers for the 64-bit versions, when needed */
int gd_alter_frameoffset(DIRFILE* D, off_t offset, int fragment, int move)
{
  return gd_alter_frameoffset64(D, offset, fragment, move);
}

off_t gd_frameoffset(DIRFILE* D, int fragment) gd_nothrow
{
  return gd_frameoffset64(D, fragment);
}

off64_t _GD_GetEOF(DIRFILE *D, gd_entry_t* E, const char *parent, int *is_index)
{
  off64_t ns = -1, ns1;
  gd_spf_t spf0, spf1;
  int i, is_index1;

  dtrace("%p, %p, \"%s\", %p", D, E, parent, is_index);

  if (++D->recurse_level >= GD_MAX_RECURSE_LEVEL) {
    _GD_SetError(D, GD_E_RECURSE_LEVEL, GD_E_RECURSE_CODE, NULL, 0, E->field);
    D->recurse_level--;
    dreturn("%i ?", -1);
    return -1;
  }

  *is_index = 0;
  switch (E->field_type) {
    case GD_RAW_ENTRY:
      if (!_GD_Supports(D, E, GD_EF_NAME | GD_EF_SIZE))
        break;

      if ((*_gd_ef[E->e->u.raw.file[0].subenc].name)(D, E->e->u.raw.file,
            E->e->u.raw.filebase, 0, 0))
      {
        break;
      }

      ns = (*_gd_ef[E->e->u.raw.file[0].subenc].size)(
          D->fragment[E->fragment_index].dirfd, E->e->u.raw.file,
          E->EN(raw,data_type), _GD_FileSwapBytes(D, E->fragment_index));

      if (ns < 0) {
        _GD_SetError(D, GD_E_RAW_IO, 0, E->e->u.raw.file[0].name, errno, NULL);
        ns = -1;
        break;
      }

      ns += D->fragment[E->fragment_index].frame_offset * E->EN(raw,spf);
      break;
    case GD_BIT_ENTRY:
    case GD_LINTERP_ENTRY:
    case GD_SBIT_ENTRY:
    case GD_POLYNOM_ENTRY:
    case GD_RECIP_ENTRY:
    case GD_WINDOW_ENTRY:
      if (_GD_BadInput(D, E, 0, 1))
        break;

      ns = _GD_GetEOF(D, E->e->entry[0], E->field, is_index);
      break;
    case GD_DIVIDE_ENTRY:
    case GD_MULTIPLY_ENTRY:
      if (_GD_BadInput(D, E, 0, 1) || _GD_BadInput(D, E, 1, 1))
        break;

      ns = _GD_GetEOF(D, E->e->entry[0], E->field, is_index);

      if (D->error)
        break;

      spf0 = _GD_GetSPF(D, E->e->entry[0]);

      if (D->error) {
        ns = -1;
        break;
      }

      ns1 = _GD_GetEOF(D, E->e->entry[1], E->field, &is_index1);

      if (D->error) {
        ns = -1;
        break;
      }

      if (!is_index1) {
        spf1 = _GD_GetSPF(D, E->e->entry[1]);

        if (D->error) {
          ns = -1;
          break;
        }

        ns1 = ns1 * spf0 / spf1;
        if (*is_index || ns1 < ns) {
          *is_index = is_index1;
          ns = ns1;
        }
      }
      break;
    case GD_LINCOM_ENTRY:
      if (_GD_BadInput(D, E, 0, 1))
        break;

      ns = _GD_GetEOF(D, E->e->entry[0], E->field, is_index);

      if (D->error) {
        ns = -1;
        break;
      }

      if (E->EN(lincom,n_fields) == 1)
        break;

      spf0 = _GD_GetSPF(D, E->e->entry[0]);

      if (D->error) {
        ns = -1;
        break;
      }

      for (i = 1; i < E->EN(lincom,n_fields); ++i) {
        if (_GD_BadInput(D, E, i, 1)) {
          ns = -1;
          break;
        }

        ns1 = _GD_GetEOF(D, E->e->entry[i], E->field, &is_index1);

        if (D->error) {
          ns = -1;
          break;
        }

        if (!is_index1) {
          spf1 = _GD_GetSPF(D, E->e->entry[i]);

          if (D->error) {
            ns = -1;
            break;
          }

          ns1 = ns1 * spf0 / spf1;
          if (*is_index || ns1 < ns) {
            *is_index = is_index1;
            ns = ns1;
          }
        }
      }
      break;
    case GD_PHASE_ENTRY:
      if (_GD_BadInput(D, E, 0, 1))
        break;

      ns = _GD_GetEOF(D, E->e->entry[0], E->field, is_index);
      if (!*is_index && !D->error)
        ns -= E->EN(phase,shift);

      /* The EOF may never be negative. */
      if (ns < 0)
        ns = 0;

      break;
    case GD_INDEX_ENTRY:
      *is_index = 1;
      break;
    case GD_CONST_ENTRY:
    case GD_CARRAY_ENTRY:
    case GD_STRING_ENTRY:
      if (parent)
        _GD_SetError(D, GD_E_DIMENSION, GD_E_DIM_FORMAT, parent, 0, E->field);
      else
        _GD_SetError(D, GD_E_DIMENSION, GD_E_DIM_CALLER, NULL, 0, E->field);
      break;
    case GD_NO_ENTRY:
      _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, E->field);
      break;
  }

  D->recurse_level--;

  dreturn("%lli %i", (unsigned long long)ns, *is_index);
  return ns;
}

off64_t gd_eof64(DIRFILE* D, const char *field_code_in)
{
  off64_t ns;
  gd_entry_t *entry;
  int repr, is_index;
  char* field_code;

  dtrace("%p, \"%s\"", D, field_code_in);

  _GD_ClearError(D);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  entry = _GD_FindFieldAndRepr(D, field_code_in, &field_code, &repr, NULL, 1,
      1);

  if (D->error) {
    dreturn("%i", -1);
    return -1;
  }

  ns = _GD_GetEOF(D, entry, NULL, &is_index);

  if (is_index)
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);

  if (field_code != field_code_in)
    free(field_code);

  dreturn("%lli", (unsigned long long)ns);
  return ns;
}

/* 32(ish)-bit wrapper for the 64-bit version, when needed */
off_t gd_eof(DIRFILE* D, const char *field_code)
{
  return (off_t)gd_eof64(D, field_code);
}

static off64_t _GD_GetBOF(DIRFILE *D, gd_entry_t* E, const char *parent,
    gd_spf_t *spf, long long *ds)
{
  off64_t bof = -1, bof1;
  gd_spf_t spf1;
  long long ds1;
  int i;

  dtrace("%p, %p, \"%s\", %p, %p", D, E, parent, spf, ds);

  if (++D->recurse_level >= GD_MAX_RECURSE_LEVEL) {
    _GD_SetError(D, GD_E_RECURSE_LEVEL, GD_E_RECURSE_CODE, NULL, 0, E->field);
    D->recurse_level--;
    dreturn("%i", -1);
    return -1;
  }

  switch (E->field_type) {
    case GD_RAW_ENTRY:
      bof = D->fragment[E->fragment_index].frame_offset;
      *spf = E->EN(raw,spf);
      *ds = 0;
      break;
    case GD_BIT_ENTRY:
    case GD_SBIT_ENTRY:
    case GD_LINTERP_ENTRY:
    case GD_POLYNOM_ENTRY:
    case GD_RECIP_ENTRY:
    case GD_WINDOW_ENTRY:
      if (_GD_BadInput(D, E, 0, 1))
        break;

      bof = _GD_GetBOF(D, E->e->entry[0], E->field, spf, ds);
      break;
    case GD_PHASE_ENTRY:
      if (_GD_BadInput(D, E, 0, 1))
        break;

      bof = _GD_GetBOF(D, E->e->entry[0], E->field, spf, ds);

      if (!D->error) {
        *ds -= E->EN(phase,shift);

        while (*ds < 0) {
          *ds += *spf;
          bof--;
        }

        while (*ds >= *spf) {
          *ds -= *spf;
          bof++;
        }

        /* The beginning-of-frame may not be before frame zero */
        if (bof < 0)
          bof = 0;
      }

      break;
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
      if (_GD_BadInput(D, E, 0, 1) || _GD_BadInput(D, E, 1, 1))
        break;

      bof = _GD_GetBOF(D, E->e->entry[0], E->field, spf, ds);

      if (D->error) {
        bof = -1;
        break;
      }

      bof1 = _GD_GetBOF(D, E->e->entry[1], E->field, &spf1, &ds1);

      if (D->error) {
        bof = -1;
        break;
      }

      if (bof1 > bof ||
          (bof1 == bof && (double)ds1 / spf1 > (double)*ds / *spf))
      {
        bof = bof1;
        *ds = ds1 * *spf / spf1;
      }
      break;
    case GD_LINCOM_ENTRY:
      if (_GD_BadInput(D, E, 0, 1))
        break;

      bof = _GD_GetBOF(D, E->e->entry[0], E->field, spf, ds);

      if (D->error) {
        bof = -1;
        break;
      }

      for (i = 1; i < E->EN(lincom,n_fields); ++i) {
        if (_GD_BadInput(D, E, i, 1)) {
          bof = -1;
          break;
        }

        bof1 = _GD_GetBOF(D, E->e->entry[i], E->field, &spf1, &ds1);

        if (D->error) {
          bof = -1;
          break;
        }

        if (bof1 > bof ||
            (bof1 == bof && (double)ds1 / spf1 > (double)*ds / *spf))
        {
          bof = bof1;
          *ds = ds1 * *spf / spf1;
        }
      }
      break;
    case GD_INDEX_ENTRY:
      bof = 0;
      *spf = 1;
      *ds = 0;
      break;
    case GD_CONST_ENTRY:
    case GD_CARRAY_ENTRY:
    case GD_STRING_ENTRY:
      if (parent)
        _GD_SetError(D, GD_E_DIMENSION, GD_E_DIM_FORMAT, parent, 0, E->field);
      else
        _GD_SetError(D, GD_E_DIMENSION, GD_E_DIM_CALLER, NULL, 0, E->field);
      break;
    case GD_NO_ENTRY:
      _GD_InternalError(D);
      break;
  }

  D->recurse_level--;

  dreturn("%llu %lu %lli", (unsigned long long)bof, (unsigned long)*spf, *ds);
  return bof;
}

off64_t gd_bof64(DIRFILE* D, const char *field_code_in) gd_nothrow
{
  off64_t bof;
  gd_entry_t *entry;
  int repr;
  char *field_code;
  gd_spf_t spf;
  long long ds;

  dtrace("%p, \"%s\"", D, field_code_in);

  _GD_ClearError(D);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  entry = _GD_FindFieldAndRepr(D, field_code_in, &field_code, &repr, NULL, 1,
      1);

  if (D->error) {
    dreturn("%i", -1);
    return -1;
  }

  bof = _GD_GetBOF(D, entry, NULL, &spf, &ds);

  if (bof != -1)
    bof = bof * spf + ds;

  if (field_code != field_code_in)
    free(field_code);

  dreturn("%lli", (unsigned long long)bof);
  return bof;
}

/* 32(ish)-bit wrapper for the 64-bit version, when needed */
off_t gd_bof(DIRFILE* D, const char *field_code) gd_nothrow
{
  return (off_t)gd_bof64(D, field_code);
}
/* vim: ts=2 sw=2 et tw=80
*/
