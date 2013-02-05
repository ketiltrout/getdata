/* Copyright (C) 2008-2010 D. V. Wiebe
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

size_t gd_get_string(DIRFILE* D, const char *field_code, size_t len,
    char *data_out) gd_nothrow
{
  size_t n_read = 0;
  gd_entry_t *entry;

  dtrace("%p, \"%s\", %" PRNsize_t ", %p", D, field_code, len, data_out);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  _GD_ClearError(D);

  entry = _GD_FindField(D, field_code, D->entry, D->n_entries, 1, NULL);
    
  if (entry == NULL)
    _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_MISSING, NULL, 0, field_code);
  else if (entry && entry->field_type != GD_STRING_ENTRY)
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
  else {
    if (len > 0 && data_out != NULL)
      strncpy(data_out, entry->e->u.string, len);

    n_read = strlen(entry->e->u.string) + 1;
  }

  dreturn("%" PRNsize_t, n_read);
  return n_read;
}

size_t _GD_DoStringOut(DIRFILE *restrict D, gd_entry_t *restrict E,
    const char *data_in)
{
  char* ptr = E->e->u.string;

  dtrace("%p, %p, %p", D, E, data_in);

  /* check protection */
  if (D->fragment[E->fragment_index].protection & GD_PROTECT_FORMAT) {
    _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[E->fragment_index].cname);
    dreturn("%i", 0);
    return 0;
  }

  E->e->u.string = _GD_Strdup(D, data_in);
  if (E->e->u.string == NULL) {
    E->e->u.string = ptr;
    dreturn("%i", 0);
    return 0;
  }
  free(ptr);
  D->fragment[E->fragment_index].modified = 1;

  dreturn("%" PRNsize_t, strlen(E->e->u.string) + 1);
  return strlen(E->e->u.string) + 1;
}

/* this function is little more than a public boilerplate for _GD_DoFieldOut */
size_t gd_put_string(DIRFILE* D, const char *field_code, const char *data_in)
  gd_nothrow
{
  size_t n_wrote = 0;
  gd_entry_t *entry;

  dtrace("%p, \"%s\", \"%s\"", D, field_code, data_in);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  if ((D->flags & GD_ACCMODE) != GD_RDWR) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  _GD_ClearError(D);

  entry = _GD_FindField(D, field_code, D->entry, D->n_entries, 1, NULL);

  if (entry == NULL)
    _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_MISSING, NULL, 0, field_code);
  else if (entry->field_type != GD_STRING_ENTRY)
    _GD_SetError(D, GD_E_BAD_FIELD_TYPE, GD_E_FIELD_BAD, NULL, 0, field_code);
  else 
    n_wrote = _GD_DoStringOut(D, entry, data_in);

  dreturn("%" PRNsize_t, n_wrote);
  return n_wrote;
}
/* vim: ts=2 sw=2 et tw=80
*/
