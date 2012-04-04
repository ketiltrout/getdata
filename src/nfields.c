/* Copyright (C) 2012 D. V. Wiebe
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

unsigned int _GD_NEntries(DIRFILE *D, struct _gd_private_entry *p,
    unsigned int type, unsigned int flags)
{
  int i;
  unsigned int u, n = 0;
  const unsigned int special = (type & GD_SPECIAL_ENTRY_BIT) ? type : 0;
  const gd_entype_t ctype = (type & GD_SPECIAL_ENTRY_BIT) ? GD_NO_ENTRY :
    (gd_entype_t)type;
  const int hidden = (flags & GD_ENTRIES_HIDDEN);
  const int noalias = (flags & GD_ENTRIES_NOALIAS);
  dtrace("%p, %p, 0x%X, 0x%X", D, p, type, flags);

  /* check for invalid type */
  if (ctype && _GD_InvalidEntype(ctype)) {
    _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_ENTRY_TYPE, NULL, type, NULL);
    dreturn("%u", 0);
    return 0;
  }

  if (p) {
    for (i = 0; i < p->n_meta; ++i)
      if (_GD_ListEntry(p->p.meta_entry[i], 1, hidden, noalias, special,
            ctype))
        n++;
  } else {
    for (u = 0; u < D->n_entries; ++u)
      if (_GD_ListEntry(D->entry[u], 0, hidden, noalias, special, ctype))
        n++;
  }

  dreturn("%u", n);
  return n;
}

unsigned int gd_nentries(DIRFILE *D, const char *parent, unsigned int type,
    unsigned int flags) gd_nothrow
{
  unsigned int n;
  struct _gd_private_entry *p = NULL;

  dtrace("%p, \"%s\", 0x%X, 0x%X", D, parent, type, flags);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%u", 0);
    return 0;
  }

  if (parent) {
    gd_entry_t *P = _GD_FindField(D, parent, D->entry, D->n_entries, 1, NULL);

    if (P == NULL || P->e->n_meta == -1) {
      _GD_SetError(D, GD_E_BAD_CODE, P ? GD_E_CODE_INVALID : GD_E_CODE_MISSING,
          NULL, 0, parent);
      dreturn("%u", 0);
      return 0;
    }
    p = P->e;
  }

  _GD_ClearError(D);

  n = _GD_NEntries(D, p, type, flags);
  dreturn("%u", n);
  return n;
}

unsigned int gd_nfields(DIRFILE* D) gd_nothrow
{
  unsigned int n;
  dtrace("%p", D);

  n = gd_nentries(D, NULL, 0, 0);
  dreturn("%u", n);
  return n;
}

unsigned int gd_nvectors(DIRFILE* D) gd_nothrow
{
  unsigned int n;
  dtrace("%p", D);

  n = gd_nentries(D, NULL, GD_VECTOR_ENTRIES, 0);
  dreturn("%u", n);
  return n;
}

unsigned int gd_nfields_by_type(DIRFILE* D, gd_entype_t type) gd_nothrow
{
  unsigned int n;
  dtrace("%p, 0x%X", D, type);

  n = gd_nentries(D, NULL, type, 0);
  dreturn("%u", n);
  return n;
}

unsigned int gd_nmfields(DIRFILE* D, const char* parent) gd_nothrow
{
  unsigned int n;
  dtrace("%p, \"%s\"", D, parent);

  n = gd_nentries(D, parent, 0, 0);
  dreturn("%u", n);
  return n;
}

unsigned int gd_nmvectors(DIRFILE* D, const char* parent) gd_nothrow
{
  unsigned int n;
  dtrace("%p, \"%s\"", D, parent);

  n = gd_nentries(D, parent, GD_VECTOR_ENTRIES, 0);
  dreturn("%u", n);
  return n;
}

unsigned int gd_nmfields_by_type(DIRFILE* D, const char* parent,
    gd_entype_t type) gd_nothrow
{
  unsigned int n;
  dtrace("%p, \"%s\", %i", D, parent, type);

  n = gd_nentries(D, parent, type, 0);
  dreturn("%u", n);
  return n;
}
