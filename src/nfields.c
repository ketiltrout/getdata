/* Copyright (C) 2008, 2010, 2012, 2013, 2016 D. V. Wiebe
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

static unsigned int _GD_NEntries(DIRFILE *D, struct gd_private_entry_ *p,
    int type, unsigned int flags)
{
  int i;
  unsigned int u, n = 0;
  int special;
  gd_entype_t ctype;
  const int hidden = (flags & GD_ENTRIES_HIDDEN);
  const int noalias = (flags & GD_ENTRIES_NOALIAS);

  dtrace("%p, %p, 0x%X, 0x%X", D, p, type, flags);

  if (type == GD_VECTOR_ENTRIES || type == GD_SCALAR_ENTRIES ||
      type == GD_ALIAS_ENTRIES)
  {
    special = type;
    ctype = GD_NO_ENTRY;
  } else if (type != GD_NO_ENTRY && _GD_InvalidEntype(type)) {
    _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_ENTRY_TYPE, NULL, type, NULL);
    dreturn("%u", 0);
    return 0;
  } else {
    special = 0;
    ctype = (gd_entype_t)type;
  }

  if (p) {
    for (i = 0; i < p->n_meta; ++i)
      if (_GD_ListEntry(p->p.meta_entry[i], 1, hidden, noalias, special,
            GD_ALL_FRAGMENTS, ctype))
      {
        n++;
      }
  } else {
    for (u = 0; u < D->n_entries; ++u)
      if (_GD_ListEntry(D->entry[u], 0, hidden, noalias, special,
            GD_ALL_FRAGMENTS, ctype))
      {
        n++;
      }
  }

  dreturn("%u", n);
  return n;
}

unsigned int gd_nentries(DIRFILE *D, const char *parent,
    int type, unsigned int flags) gd_nothrow
{
  unsigned int n;
  struct gd_private_entry_ *p = NULL;

  dtrace("%p, \"%s\", 0x%X, 0x%X", D, parent, type, flags);

  GD_RETURN_IF_INVALID(D, "%u", 0);

  if (parent) {
    gd_entry_t *P = _GD_FindEntry(D, parent);

    if (P && P->e->n_meta == -1)
      _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_INVALID, NULL, 0, parent);

    if (D->error) {
      dreturn("%u", 0);
      return 0;
    }
    p = P->e;
  }

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
