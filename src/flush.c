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
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef STDC_HEADERS
#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#endif

#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif

#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#endif

#include "internal.h"

void _GD_Flush(DIRFILE* D, gd_entry_t *entry, const char* field_code)
{
  int i;

  dtrace("%p, %p", D, entry);

  if (entry == NULL) {
    _GD_SetError(D, GD_E_BAD_CODE, 0, NULL, 0, field_code);
    dreturnvoid();
    return;
  }

  if (++D->recurse_level >= GD_MAX_RECURSE_LEVEL) {
    _GD_SetError(D, GD_E_RECURSE_LEVEL, 0, NULL, 0, field_code);
    D->recurse_level--;
    dreturnvoid();
    return;
  }

  switch(entry->field_type) {
    case GD_RAW_ENTRY:
      if (entry->fp >= 0) {
        if (fsync(entry->fp)) {
          _GD_SetError(D, GD_E_RAW_IO, 0, entry->file, errno, NULL);
        } else if (close(entry->fp)) {
          _GD_SetError(D, GD_E_RAW_IO, 0, entry->file, errno, NULL);
          D->recurse_level--;
        } else 
          entry->fp = -1;
      }
      break;
    case GD_LINCOM_ENTRY:
      for (i = 2; i < GD_MAX_LINCOM; ++i)
        if (entry->in_fields[i])
          _GD_Flush(D, _GD_FindField(D, entry->in_fields[i]), field_code);
      /* fallthrough */
    case GD_MULTIPLY_ENTRY:
      if (entry->in_fields[1])
        _GD_Flush(D, _GD_FindField(D, entry->in_fields[1]), field_code);
      /* fallthrough */
    case GD_LINTERP_ENTRY:
    case GD_BIT_ENTRY:
    case GD_PHASE_ENTRY:
      if (entry->in_fields[0])
        _GD_Flush(D, _GD_FindField(D, entry->in_fields[0]), field_code);
  }

  D->recurse_level--;
  dreturnvoid();
}

void dirfile_flush(DIRFILE* D, const char* field_code)
{
  int i;

  dtrace("%p, \"%s\"", D, field_code);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturnvoid();
    return;
  }

  _GD_ClearError(D);

  if (field_code == NULL) {
    for (i = 0; i < D->n_entries; ++i)
      if (D->entry[i]->field_type == GD_RAW_ENTRY)
        _GD_Flush(D, D->entry[i], field_code);
  } else
    _GD_Flush(D, _GD_FindField(D, field_code), field_code);

  dreturnvoid();
}
/* vim: ts=2 sw=2 et tw=80
*/
