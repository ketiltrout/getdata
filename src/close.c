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
#include <stdlib.h>
#endif

#include "internal.h"

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
          if (D->entry[i]->table_len > 0) {
            free(D->entry[i]->x);
            free(D->entry[i]->y);
          }
          break;
        case GD_LINCOM_ENTRY:
          for (j = 2; j < D->entry[i]->n_fields; ++j)
            free(D->entry[i]->in_fields[j]);
          /* fall through */
        case GD_MULTIPLY_ENTRY:
          free(D->entry[i]->in_fields[1]);
          /* fall through */
        case GD_BIT_ENTRY:
        case GD_PHASE_ENTRY:
          free(D->entry[i]->in_fields[0]);
          /* fall through */
        case GD_NO_ENTRY:
          break;
      }
    }

  /* Item zero of include_list is always a static string */
  for(i = 1; i < D->n_include; ++i)
    free((char*)D->include_list[i].name);

  free(D->entry);
  free(D->error_string);
  free(D->error_file);
  free(D->field_list);
  free(D->include_list);

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
    return -1;
  }

  _GD_FreeD(D);

  dreturn("%i", 0);
  return 0;
}
/* vim: ts=2 sw=2 et tw=80
*/
