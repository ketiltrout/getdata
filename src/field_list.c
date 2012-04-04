/* Copyright (C) 2008-2012 D. V. Wiebe
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

/* zero length lists */
static const char* zero_list[1] = { NULL };
static const gd_carray_t zero_carrays[1] = { {0, NULL} };

_gd_static_inline int _GD_EntryIndex(unsigned int t)
{
  int i;

  dtrace("%u", t);
  switch(t) {
    case GD_RAW_ENTRY:
      i = 0;
      break;
    case GD_LINCOM_ENTRY:
      i = 1;
      break;
    case GD_LINTERP_ENTRY:
      i = 2;
      break;
    case GD_BIT_ENTRY:
      i = 3;
      break;
    case GD_MULTIPLY_ENTRY:
      i = 4;
      break;
    case GD_PHASE_ENTRY:
      i = 5;
      break;
    case GD_INDEX_ENTRY:
      i = 6;
      break;
    case GD_POLYNOM_ENTRY:
      i = 7;
      break;
    case GD_SBIT_ENTRY:
      i = 8;
      break;
    case GD_DIVIDE_ENTRY:
      i = 9;
      break;
    case GD_RECIP_ENTRY:
      i = 10;
      break;
    case GD_WINDOW_ENTRY:
      i = 11;
      break;
    case GD_MPLEX_ENTRY:
      i = 12;
      break;
    case GD_CONST_ENTRY:
      i = 13;
      break;
    case GD_CARRAY_ENTRY:
      i = 14;
      break;
    case GD_STRING_ENTRY:
      i = 15;
      break;
    case GD_VECTOR_ENTRIES:
      i = 16;
      break;
    case GD_SCALAR_ENTRIES:
      i = 17;
      break;
    case GD_ALL_ENTRIES:
      i = 18;
      break;
    default:
      i = -1;
      break;
  }

  dreturn("%i", i);
  return i;
}

/* returns true if E a member of the given list */
int _GD_ListEntry(const gd_entry_t *E, int meta, int hidden, int noalias,
    unsigned int special, gd_entype_t type)
{
  dtrace("%p{%s}, %i, %i, %i, %u, 0x%X", E, E->field, meta, hidden, noalias,
      special, type);

  /* check hidden */
  if (!hidden && E->hidden) {
    dreturn("%i (hidden)", 0);
    return 0;
  }

  /* check meta */
  if (!meta && E->e->n_meta == -1) {
    dreturn("%i (meta)", 0);
    return 0;
  }

  /* aliases */
  if (E->field_type == GD_ALIAS_ENTRY) {
    if (noalias) {
      dreturn("%i (alias)", 0);
      return 0;
    }
    int ret = 0;
    if (E->e->entry[0])
      ret = _GD_ListEntry(E->e->entry[0], meta, hidden, 0, special, type);
    dreturn("%i", ret);
    return ret;
  }

  /* type check */
  if (special == GD_VECTOR_ENTRIES && (E->field_type & GD_SCALAR_ENTRY_BIT)) {
    dreturn("%i (vector)", 0);
    return 0;
  } else if (special == GD_SCALAR_ENTRIES &&
      !(E->field_type & GD_SCALAR_ENTRY_BIT))
  {
    dreturn("%i (scalar)", 0);
    return 0;
  } else if (type && E->field_type != type) {
    dreturn("%i (type)", 0);
    return 0;
  }

  dreturn("%i", 1);
  return 1;
}

static const char **_GD_EntryList(DIRFILE *D, struct _gd_private_entry *p,
    size_t offs, unsigned int type, unsigned int flags) gd_nothrow
{
  char** el;
  int i, index;
  unsigned int u, n = 0;
  const unsigned int special = (type & GD_SPECIAL_ENTRY_BIT) ? type : 0;
  const gd_entype_t ctype = (type & GD_SPECIAL_ENTRY_BIT) ? GD_NO_ENTRY :
    (gd_entype_t)type;
  const int hidden = (flags & GD_ENTRIES_HIDDEN);
  const int noalias = (flags & GD_ENTRIES_NOALIAS);

  dtrace("%p, %p, %zu, 0x%X, 0x%X", D, p, offs, type, flags);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_ClearError(D);

  index = _GD_EntryIndex(type);
  if (index < 0) {
    _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_ENTRY_TYPE, NULL, type, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  if (p) {
    if (p->entry_list_validity & (1 << index) &&
      p->entry_list_flags[index] == flags)
    {
      /* list already made */
      dreturn("%p", p->entry_list[index]);
      return p->entry_list[index];
    }
  } else if (D->entry_list_validity & (1 << index) &&
      D->entry_list_flags[index] == flags)
  {
    /* list already made */
    dreturn("%p", D->entry_list[index]);
    return D->entry_list[index];
  }

  n = _GD_NEntries(D, p, type, flags);

  if (D->error) {
    dreturn("%p", NULL);
    return NULL;
  }

  if (n == 0) {
    dreturn("%p", zero_list);
    return zero_list;
  }

  el = (char **)_GD_Malloc(D, sizeof(const char*) * (n + 1));

  if (el == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  n = 0;
  if (p) {
    for (i = 0; i < p->n_meta; ++i)
      if (_GD_ListEntry(p->p.meta_entry[i], 1, hidden, noalias, special, ctype))
        el[n++] = p->p.meta_entry[i]->field + offs;

    free(p->entry_list[index]);
    p->entry_list[index] = (const char **)el;
    p->entry_list_flags[index] = flags;
    p->entry_list_validity |= 1 << index;
  } else {
    for (u = 0; u < D->n_entries; ++u)
      if (_GD_ListEntry(D->entry[u], 0, hidden, noalias, special, ctype))
        el[n++] = D->entry[u]->field;

    free(D->entry_list[index]);
    D->entry_list[index] = (const char **)el;
    D->entry_list_flags[index] = flags;
    D->entry_list_validity |= 1 << index;
  }
  el[n] = NULL;

  dreturn("%p", (const char **)el);
  return (const char **)el;
}

const char **gd_entry_list(DIRFILE* D, const char *parent, unsigned int type,
    unsigned int flags) gd_nothrow
{
  const char **el;
  size_t offs = 0;
  struct _gd_private_entry *p = NULL;

  dtrace("%p, \"%s\", %u, %u", D, parent, type, flags);

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
    offs = strlen(P->field) + 1;
  }

  _GD_ClearError(D);

  el = _GD_EntryList(D, p, offs, type, flags);
  dreturn("%p", el);
  return el;
}

const void *gd_constants(DIRFILE* D, gd_type_t return_type) gd_nothrow
{
  unsigned int i, n;
  char* fl;

  dtrace("%p, 0x%x", D, return_type);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_ClearError(D);

  if ((n = _GD_NEntries(D, NULL, GD_CONST_ENTRY, 0)) == 0) {
    dreturn("%p", NULL);
    return NULL;
  }

  fl = (char *)_GD_Alloc(D, return_type, n);

  if (fl == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  for (i = n = 0; i < D->n_entries; ++i) {
    if (_GD_ListEntry(D->entry[i], 0, 0, 0, 0, GD_CONST_ENTRY))
      if (_GD_DoField(D, D->entry[i], 0, 0, 1, return_type,
            fl + n++ * GD_SIZE(return_type)) != 1)
        break;
  }

  free(D->const_value_list);
  D->const_value_list = fl;

  dreturn("%p", D->error ? NULL : D->const_value_list);
  return D->error ? NULL : D->const_value_list;
}

const gd_carray_t *gd_carrays(DIRFILE* D, gd_type_t return_type) gd_nothrow
{
  unsigned int i, n;
  gd_carray_t* fl;

  dtrace("%p, 0x%x", D, return_type);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_ClearError(D);

  if ((n = _GD_NEntries(D, NULL, GD_CARRAY_ENTRY, 0)) == 0) {
    dreturn("%p", zero_carrays);
    return zero_carrays;
  }

  fl = (gd_carray_t *)_GD_Malloc(D, sizeof(gd_carray_t) * (n + 1));

  if (fl == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  memset(fl, 0, sizeof(gd_carray_t) * (n + 1));

  for (i = n = 0; i < D->n_entries; ++i) {
    if (_GD_ListEntry(D->entry[i], 0, 0, 0, 0, GD_CARRAY_ENTRY)) {
      fl[n].n = D->entry[i]->EN(scalar,array_len);
      fl[n].d = _GD_Alloc(D, return_type, fl[n].n);
      if (D->error || _GD_DoField(D, D->entry[i], 0, 0, fl[n].n, return_type,
            fl[n].d) != 1)
        break;
      n++;
    }
  }
  fl[n].n = 0;

  if (D->carray_value_list)
    for (i = 0; D->carray_value_list[i].n != 0; ++i)
      free(D->carray_value_list[i].d);
  free(D->carray_value_list);
  D->carray_value_list = fl;

  dreturn("%p", D->error ? NULL : fl);
  return D->error ? NULL : fl;
}

const char **gd_strings(DIRFILE* D) gd_nothrow
{
  unsigned int i, n;
  char** fl;

  dtrace("%p", D);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_ClearError(D);

  if ((n = _GD_NEntries(D, NULL, GD_STRING_ENTRY, 0)) == 0) {
    dreturn("%p", zero_list);
    return zero_list;
  }

  if (D->value_list_validity & GD_LIST_VALID_STRING_VALUE) {
    /* list already made */
    dreturn("%p", D->string_value_list);
    return D->string_value_list;
  }

  fl = (char **)_GD_Malloc(D, sizeof(const char*) * (n + 1));

  if (fl == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  for (i = n = 0; i < D->n_entries; ++i) {
    if (_GD_ListEntry(D->entry[i], 0, 0, 0, 0, GD_STRING_ENTRY))
      fl[n++] = D->entry[i]->e->u.string;
  }
  fl[n] = NULL;

  free(D->string_value_list);
  D->string_value_list = (const char **)fl;
  D->value_list_validity |= GD_LIST_VALID_STRING_VALUE;

  dreturn("%p", D->string_value_list);
  return D->string_value_list;
}

const char **gd_field_list_by_type(DIRFILE* D, gd_entype_t type) gd_nothrow
{
  const char** el;
  dtrace("%p, 0x%X", D, type);

  el = gd_entry_list(D, NULL, type, 0);
  dreturn("%p", el);
  return el;
}

const char **gd_vector_list(DIRFILE* D) gd_nothrow
{
  const char **el;
  dtrace("%p", D);

  el = gd_entry_list(D, NULL, GD_VECTOR_ENTRIES, 0);
  dreturn("%p", el);
  return el;
}

const char **gd_field_list(DIRFILE* D) gd_nothrow
{
  const char **el;

  dtrace("%p", D);

  el = gd_entry_list(D, NULL, GD_ALL_ENTRIES, 0);
  dreturn("%p", el);
  return el;
}

const void *gd_mconstants(DIRFILE* D, const char* parent,
    gd_type_t return_type) gd_nothrow
{
  int i, n;
  char* fl;
  gd_entry_t *P;
  struct _gd_private_entry *e;

  dtrace("%p, \"%s\", 0x%x", D, parent, return_type);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_ClearError(D);

  P = _GD_FindField(D, parent, D->entry, D->n_entries, 1, NULL);

  if (P == NULL || P->e->n_meta == -1) {
    _GD_SetError(D, GD_E_BAD_CODE, P ? GD_E_CODE_INVALID : GD_E_CODE_MISSING,
        NULL, 0, parent);
    dreturn("%p", NULL);
    return NULL;
  }

  e = P->e;

  if ((n = gd_nmfields_by_type(D, parent, GD_CONST_ENTRY)) == 0) {
    dreturn("%p", NULL);
    return NULL;
  }

  fl = (char *)_GD_Alloc(D, return_type, n);

  if (fl == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  /* DoField will implicitly choose GD_REPR_AUTO for complex data being returned
   * as purely real */
  for (i = n = 0; i < e->n_meta; ++i) {
    if (_GD_ListEntry(e->p.meta_entry[i], 1, 0, 0, 0, GD_CONST_ENTRY))
      if (_GD_DoField(D, e->p.meta_entry[i], 0, 0, 1, return_type,
            fl + n++ * GD_SIZE(return_type)) != 1)
        break;
  }

  free(e->const_value_list);
  e->const_value_list = fl;

  dreturn("%p", e->const_value_list);
  return e->const_value_list;
}

const gd_carray_t *gd_mcarrays(DIRFILE* D, const char* parent,
    gd_type_t return_type) gd_nothrow
{
  int i, n;
  gd_carray_t *fl;
  gd_entry_t *P;
  struct _gd_private_entry *e;

  dtrace("%p, \"%s\", 0x%x", D, parent, return_type);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_ClearError(D);

  P = _GD_FindField(D, parent, D->entry, D->n_entries, 1, NULL);

  if (P == NULL || P->e->n_meta == -1) {
    _GD_SetError(D, GD_E_BAD_CODE, P ? GD_E_CODE_INVALID : GD_E_CODE_MISSING,
        NULL, 0, parent);
    dreturn("%p", NULL);
    return NULL;
  }

  e = P->e;

  if ((n = gd_nmfields_by_type(D, parent, GD_CARRAY_ENTRY)) == 0) {
    dreturn("%p", zero_carrays);
    return zero_carrays;
  }

  fl = (gd_carray_t *)_GD_Malloc(D, sizeof(gd_carray_t) * (n + 1));

  if (fl == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  /* DoField will implicitly choose GD_REPR_AUTO for complex data being returned
   * as purely real */
  for (i = n = 0; i < e->n_meta; ++i) {
    if (_GD_ListEntry(e->p.meta_entry[i], 1, 0, 0, 0, GD_CARRAY_ENTRY)) {
      fl[n].n = e->p.meta_entry[i]->EN(scalar,array_len);
      fl[n].d = _GD_Alloc(D, return_type, fl[n].n);
      if (D->error || _GD_DoField(D, e->p.meta_entry[i], 0, 0, fl[n].n,
            return_type, fl[n].d) != 1)
        break;
      n++;
    }
  }
  fl[n].n = 0;

  if (e->carray_value_list)
    for (i = 0; e->carray_value_list[i].n != 0; ++i)
      free(e->carray_value_list[i].d);
  free(e->carray_value_list);

  e->carray_value_list = fl;

  dreturn("%p", D->error ? NULL : fl);
  return D->error ? NULL : fl;
}

const char **gd_mstrings(DIRFILE* D, const char* parent) gd_nothrow
{
  int i, n;
  char** fl;
  gd_entry_t *P;
  struct _gd_private_entry *e;

  dtrace("%p, \"%s\"", D, parent);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  _GD_ClearError(D);

  P = _GD_FindField(D, parent, D->entry, D->n_entries, 1, NULL);

  if (P == NULL || P->e->n_meta == -1) {
    _GD_SetError(D, GD_E_BAD_CODE, P ? GD_E_CODE_INVALID : GD_E_CODE_MISSING,
        NULL, 0, parent);
    dreturn("%p", NULL);
    return NULL;
  }

  e = P->e;

  if ((n = gd_nmfields_by_type(D, parent, GD_STRING_ENTRY)) == 0) {
    dreturn("%p", zero_list);
    return zero_list;
  }

  fl = (char **)_GD_Realloc(D, (char **)e->string_value_list,
      sizeof(const char*) * (n + 1));

  if (fl == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  for (i = n = 0; i < e->n_meta; ++i)
    if (_GD_ListEntry(e->p.meta_entry[i], 1, 0, 0, 0, GD_STRING_ENTRY))
      fl[n++] = e->p.meta_entry[i]->e->u.string;
  fl[n] = NULL;

  e->string_value_list = (const char **)fl;

  dreturn("%p", e->string_value_list);
  return e->string_value_list;
}

const char **gd_mfield_list_by_type(DIRFILE* D, const char* parent,
    gd_entype_t type) gd_nothrow
{
  const char **el;
  dtrace("%p, \"%s\", 0x%X", D, parent, type);

  el = gd_entry_list(D, parent, type, 0);
  dreturn("%p", el);
  return el;
}

const char **gd_mvector_list(DIRFILE* D, const char* parent) gd_nothrow
{
  const char **el;
  dtrace("%p, \"%s\"", D, parent);

  el = gd_entry_list(D, parent, GD_VECTOR_ENTRIES, 0);
  dreturn("%p", el);
  return el;
}

const char **gd_mfield_list(DIRFILE* D, const char* parent) gd_nothrow
{
  const char **el;
  dtrace("%p, \"%s\"", D, parent);

  el = gd_entry_list(D, parent, GD_ALL_ENTRIES, 0);
  dreturn("%p", el);
  return el;
}
