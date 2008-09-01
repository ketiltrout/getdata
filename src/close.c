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

/* _GD_FreeD: free the DIRFILE and its subordinates
*/
static void _GD_FreeD(DIRFILE* D)
{
  int i, j;

  dtrace("%p", D);

  for (i = 0; i < D->n_entries; ++i) 
    if (D->entries[i] != NULL) {
      free((char*)D->entries[i]->field); /* cast away bogus constness */
      switch(D->entries[i]->field_type) {
        case GD_RAW_ENTRY:
          free(D->entries[i]->file);
          break;
        case GD_LINCOM_ENTRY:
          for (j = 0; j < D->entries[i]->count; ++j)
            free(D->entries[i]->in_fields[j]);
          break;
        case GD_LINTERP_ENTRY:
          free(D->entries[i]->in_fields[0]);
          free(D->entries[i]->file);
          if (D->entries[i]->count > 0) {
            free(D->entries[i]->x);
            free(D->entries[i]->y);
          }
          break;
        case GD_MULTIPLY_ENTRY:
          free(D->entries[i]->in_fields[0]);
          free(D->entries[i]->in_fields[1]);
          break;
        case GD_BIT_ENTRY:
        case GD_PHASE_ENTRY:
          free(D->entries[i]->in_fields[0]);
          break;
      }
    }

  free(D->entries);
  free(D->error_string);
  free(D->error_file);
  free(D->field_list);

  dreturnvoid();
}

/* dirfile_close: Close the specified dirfile and free memory
*/
void dirfile_close(DIRFILE* D)
{
  int i;

  dtrace("%p", D);

  for(i = 0; i < D->n_entries; ++i)
    if (D->entries[i]->field_type == GD_RAW_ENTRY)
      close(D->entries[i]->fp);

  _GD_ClearError(D);
  _GD_FreeD(D);

  dreturnvoid();
}
/* vim: ts=2 sw=2 et tw=80
*/
