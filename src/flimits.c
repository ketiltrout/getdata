/* Copyright (C) 2008-2016 D. V. Wiebe
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

  dtrace("%p, %" PRId64 ", %i, %i", D, (int64_t)offset, fragment, move);

  /* check protection */
  if (D->fragment[fragment].protection & GD_PROTECT_FORMAT) {
    _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[fragment].cname);
    dreturnvoid();
    return;
  }

  if (move && offset != D->fragment[fragment].frame_offset) {
    gd_entry_t **raw_entry = _GD_Malloc(D, sizeof(*raw_entry) * D->n_entries);

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

  dtrace("%p, %" PRId64 ", %i, %i", D, (int64_t)offset, fragment, move);

  GD_RETURN_ERR_IF_INVALID(D);

  if ((D->flags & GD_ACCMODE) != GD_RDWR)
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
  else if (fragment < GD_ALL_FRAGMENTS || fragment >= D->n_fragment)
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, 0, NULL);
  else if (offset < 0)
    _GD_SetError(D, GD_E_RANGE, GD_E_OUT_OF_RANGE, NULL, 0, NULL);
  else if (fragment == GD_ALL_FRAGMENTS) {
    for (i = 0; i < D->n_fragment; ++i) {
      _GD_ShiftFragment(D, offset, i, move);

      if (D->error)
        break;
    }
  } else
    _GD_ShiftFragment(D, offset, fragment, move);

  GD_RETURN_ERROR(D);
}

off64_t gd_frameoffset64(DIRFILE* D, int fragment)
{
  dtrace("%p, %i", D, fragment);

  GD_RETURN_ERR_IF_INVALID(D);

  if (fragment < 0 || fragment >= D->n_fragment)
    GD_SET_RETURN_ERROR(D, GD_E_BAD_INDEX, 0, NULL, 0, NULL);

  dreturn("%" PRId64, (int64_t)D->fragment[fragment].frame_offset);
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

off64_t _GD_GetEOF(DIRFILE *restrict D, gd_entry_t *restrict E,
    const char *restrict parent, int *restrict is_index)
{
  off64_t ns = -1, ns1;
  unsigned int spf0, spf1;
  int i, is_index1;

  dtrace("%p, %p, \"%s\", %p", D, E, parent, is_index);

  if (++D->recurse_level >= GD_MAX_RECURSE_LEVEL) {
    D->recurse_level--;
    GD_SET_RETURN_ERROR(D, GD_E_RECURSE_LEVEL, GD_E_RECURSE_CODE, NULL, 0,
        E->field);
  }

  if (_GD_FindInputs(D, E, 1)) {
    D->recurse_level--;
    dreturn("%i", D->error);
    return D->error;
  }

  *is_index = 0;
  switch (E->field_type) {
    case GD_RAW_ENTRY:
      if (!_GD_Supports(D, E, GD_EF_NAME | GD_EF_SIZE))
        break;

      if ((*_GD_ef[E->e->u.raw.file[0].subenc].name)(D,
            (const char*)D->fragment[E->fragment_index].enc_data,
            E->e->u.raw.file, E->e->u.raw.filebase, 0, 0))
      {
        break;
      }

      ns = (*_GD_ef[E->e->u.raw.file[0].subenc].size)(
          D->fragment[E->fragment_index].dirfd, E->e->u.raw.file,
          E->EN(raw,data_type), _GD_FileSwapBytes(D, E));

      if (ns < 0) {
        _GD_SetEncIOError(D, GD_E_IO_READ, E->e->u.raw.file + 0);
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
    case GD_INDIR_ENTRY:
    case GD_SINDIR_ENTRY:
      ns = _GD_GetEOF(D, E->e->entry[0], E->field, is_index);
      break;
    case GD_DIVIDE_ENTRY:
    case GD_MULTIPLY_ENTRY:
    case GD_WINDOW_ENTRY:
    case GD_MPLEX_ENTRY:
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
    case GD_SARRAY_ENTRY:
    case GD_STRING_ENTRY:
      if (parent)
        _GD_SetError(D, GD_E_DIMENSION, GD_E_DIM_FORMAT, parent, 0, E->field);
      else
        _GD_SetError(D, GD_E_DIMENSION, GD_E_DIM_CALLER, NULL, 0, E->field);
      break;
    case GD_NO_ENTRY:
      _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, E->field);
      break;
    case GD_ALIAS_ENTRY:
      _GD_InternalError(D);
      break;
  }

  D->recurse_level--;

  if (D->error) 
    GD_RETURN_ERROR(D);

  dreturn("%" PRId64 " %i", (int64_t)ns, *is_index);
  return ns;
}

off64_t gd_eof64(DIRFILE* D, const char *field_code)
{
  off64_t ns;
  gd_entry_t *entry;
  int is_index;

  dtrace("%p, \"%s\"", D, field_code);

  GD_RETURN_ERR_IF_INVALID(D);

  entry = _GD_FindEntry(D, field_code);

  if (D->error)
    GD_RETURN_ERROR(D);

  ns = _GD_GetEOF(D, entry, NULL, &is_index);

  if (!D->error && is_index)
    GD_SET_RETURN_ERROR(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0,
        field_code);

  dreturn("%" PRId64, (int64_t)ns);
  return ns;
}

/* 32(ish)-bit wrapper for the 64-bit version, when needed */
off_t gd_eof(DIRFILE* D, const char *field_code)
{
  return (off_t)gd_eof64(D, field_code);
}

static off64_t _GD_GetBOF(DIRFILE *restrict D, gd_entry_t *restrict E,
    const char *restrict parent, unsigned int *restrict spf,
    int64_t *restrict ds)
{
  off64_t bof = -1, bof1;
  unsigned int spf1;
  int64_t ds1;
  int i;

  dtrace("%p, %p, \"%s\", %p, %p", D, E, parent, spf, ds);

  if (++D->recurse_level >= GD_MAX_RECURSE_LEVEL) {
    D->recurse_level--;
    GD_SET_RETURN_ERROR(D, GD_E_RECURSE_LEVEL, GD_E_RECURSE_CODE, NULL, 0,
        E->field);
  }

  if (_GD_FindInputs(D, E, 1)) {
    D->recurse_level--;
    dreturn("%i", D->error);
    return D->error;
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
    case GD_INDIR_ENTRY:
    case GD_SINDIR_ENTRY:
      bof = _GD_GetBOF(D, E->e->entry[0], E->field, spf, ds);
      break;
    case GD_PHASE_ENTRY:
      bof = _GD_GetBOF(D, E->e->entry[0], E->field, spf, ds);

      if (!D->error) {
        *ds -= E->EN(phase,shift);

        /* remove whole frames from delta-samples */
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
          bof = *ds = 0;
      }

      break;
    case GD_MULTIPLY_ENTRY:
    case GD_DIVIDE_ENTRY:
    case GD_WINDOW_ENTRY:
    case GD_MPLEX_ENTRY:
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
      bof = _GD_GetBOF(D, E->e->entry[0], E->field, spf, ds);

      if (D->error) {
        bof = -1;
        break;
      }

      for (i = 1; i < E->EN(lincom,n_fields); ++i) {
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
    case GD_SARRAY_ENTRY:
    case GD_STRING_ENTRY:
      if (parent)
        _GD_SetError(D, GD_E_DIMENSION, GD_E_DIM_FORMAT, parent, 0, E->field);
      else
        _GD_SetError(D, GD_E_DIMENSION, GD_E_DIM_CALLER, NULL, 0, E->field);
      break;
    case GD_NO_ENTRY:
    case GD_ALIAS_ENTRY:
      _GD_InternalError(D);
      break;
  }

  D->recurse_level--;

  if (D->error)
    GD_RETURN_ERROR(D);

  dreturn("%" PRIu64 " %u %" PRId64, bof, *spf, *ds);
  return bof;
}

off64_t gd_bof64(DIRFILE* D, const char *field_code) gd_nothrow
{
  off64_t bof;
  gd_entry_t *entry;
  unsigned int spf;
  int64_t ds;

  dtrace("%p, \"%s\"", D, field_code);

  GD_RETURN_ERR_IF_INVALID(D);

  entry = _GD_FindEntry(D, field_code);

  if (D->error)
    GD_RETURN_ERROR(D);

  bof = _GD_GetBOF(D, entry, NULL, &spf, &ds);

  if (bof >= 0) /* i.e. not an error code */
    bof = bof * spf + ds;

  dreturn("%" PRId64, (int64_t)bof);
  return bof;
}

/* 32(ish)-bit wrapper for the 64-bit version, when needed */
off_t gd_bof(DIRFILE* D, const char *field_code) gd_nothrow
{
  return (off_t)gd_bof64(D, field_code);
}
/* vim: ts=2 sw=2 et tw=80
*/
