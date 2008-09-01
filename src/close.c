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
 * You should have received a copy of the GNU General Public
 * License along with GetData; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 * 02111-1307 USA.
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

static void _GD_Flush(DIRFILE* D, gd_entry_t *entry, const char* field_code)
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

/* _GD_FreeD: free the DIRFILE and its subordinates
*/
static void _GD_FreeD(DIRFILE* D)
{
  int i, j;

  dtrace("%p", D);

  for (i = 0; i < D->n_entries; ++i) 
    if (D->entry[i] != NULL) {
      free((char*)D->entry[i]->field); /* cast away bogus constness */
      switch(D->entry[i]->field_type) {
        case GD_RAW_ENTRY:
          free(D->entry[i]->file);
          break;
        case GD_LINTERP_ENTRY:
          free(D->entry[i]->in_fields[0]);
          free(D->entry[i]->table);
          if (D->entry[i]->count > 0) {
            free(D->entry[i]->x);
            free(D->entry[i]->y);
          }
          break;
        case GD_LINCOM_ENTRY:
          for (j = 2; j < D->entry[i]->count; ++j)
            free(D->entry[i]->in_fields[j]);
          /* fall through */
        case GD_MULTIPLY_ENTRY:
          free(D->entry[i]->in_fields[1]);
          /* fall through */
        case GD_BIT_ENTRY:
        case GD_PHASE_ENTRY:
          free(D->entry[i]->in_fields[0]);
          break;
      }
    }

  free(D->entry);
  free(D->error_string);
  free(D->error_file);
  free(D->field_list);

  dreturnvoid();
}

/* dirfile_close: Close the specified dirfile and free memory.  This must
 * return an error indicator, since checking D->error after this call won't
 * work if the function was a success.
*/
int dirfile_close(DIRFILE* D)
{
  int i;

  dtrace("%p", D);

  if (D == NULL) {
    dreturn("%i", 0);
    return 0;
  }

  _GD_ClearError(D);

  for(i = 0; i < D->n_entries; ++i)
    if (D->entry[i]->field_type == GD_RAW_ENTRY)
      _GD_Flush(D, D->entry[i], D->entry[i]->field);

  if (D->error) {
    dreturn("%i", 1);
    return 1;
  }

  _GD_FreeD(D);

  dreturn("%i", 0);
  return 0;
}
/* vim: ts=2 sw=2 et tw=80
*/
