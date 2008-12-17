/* (C) 2008 D. V. Wiebe
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
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#endif

static void _GD_ShiftFragment(DIRFILE* D, off64_t offset, int fragment,
    int move)
{
  unsigned int i, n_raw = 0;

  dtrace("%p, %lli, %i, %i\n", D, (long long)offset, fragment, move);

  /* check protection */
  if (D->fragment[fragment].protection & GD_PROTECT_FORMAT) {
    _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[fragment].cname);
    dreturnvoid();
    return;
  }

  if (move && offset != D->fragment[fragment].frame_offset) {
    gd_entry_t **raw_entry = malloc(sizeof(gd_entry_t*) * D->n_entries);

    if (raw_entry == NULL) {
      _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
      dreturnvoid();
      return;
    }

    /* Because it may fail, the move must occur out-of-place and then be copied
     * back over the affected files once success is assured */
    for (i = 0; i < D->n_entries; ++i)
      if (D->entry[i]->fragment_index == fragment &&
          D->entry[i]->field_type == GD_RAW_ENTRY)
      {
        if (!_GD_Supports(D, D->entry[i], GD_EF_TEMP))
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
        if ((*ef[raw_entry[i]->e->file[0].encoding].temp)(raw_entry[i]->e->file,
              GD_TEMP_DESTROY))
          _GD_SetError(D, GD_E_RAW_IO, 0, raw_entry[i]->e->file[0].name,
              errno, NULL);
    } else {
      for (i = 0; i < n_raw; ++i)
        if ((*ef[raw_entry[i]->e->file[0].encoding].temp)(raw_entry[i]->e->file,
              GD_TEMP_MOVE))
        {
          _GD_SetError(D, GD_E_UNCLEAN_DB, 0,
              D->fragment[D->entry[i]->fragment_index].cname, 0, NULL);
          D->flags |= GD_INVALID;
        }
    }

    free(raw_entry);

    if (D->error) {
      dreturnvoid();
      return;
    }
  }

  D->fragment[fragment].frame_offset = offset;
  D->fragment[fragment].modified = 1;

  dreturnvoid();
}

int dirfile_alter_frameoffset64(DIRFILE* D, off64_t offset, int fragment,
    int move)
{
  int i;

  dtrace("%p, %lli, %i, %i\n", D, (long long)offset, fragment, move);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if ((D->flags & GD_ACCMODE) != GD_RDWR) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%zi", -1);
    return -1;
  }

  if (fragment < GD_ALL_FRAGMENTS || fragment >= D->n_fragment) {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (offset < 0) {
    _GD_SetError(D, GD_E_RANGE, 0, NULL, 0, NULL);
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

off64_t get_frameoffset64(DIRFILE* D, int fragment)
{
  dtrace("%p, %i\n", D, fragment);

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
int dirfile_alter_frameoffset(DIRFILE* D, off_t offset, int fragment, int move)
{
  return dirfile_alter_frameoffset64(D, offset, fragment, move);
}

off_t get_frameoffset(DIRFILE* D, int fragment)
{
  return get_frameoffset64(D, fragment);
}
