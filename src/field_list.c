/* Copyright (C) 2008-2017 D. V. Wiebe
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

#ifdef HAVE_PCRE_H
#include <pcre.h>
#endif

/* zero length lists */
static const char *zero_list[1] = { NULL };
static gd_carray_t zero_carrays[1] = { {0, NULL} };

gd_static_inline_ int _GD_EntryIndex(unsigned int t)
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
    case GD_INDIR_ENTRY:
      i = 13;
      break;
    case GD_SINDIR_ENTRY:
      i = 14;
      break;
    case GD_CONST_ENTRY:
      i = 15;
      break;
    case GD_CARRAY_ENTRY:
      i = 16;
      break;
    case GD_STRING_ENTRY:
      i = 17;
      break;
    case GD_SARRAY_ENTRY:
      i = 18;
      break;

    case GD_VECTOR_ENTRIES:
      i = 19;
      break;
    case GD_SCALAR_ENTRIES:
      i = 20;
      break;
    case GD_ALIAS_ENTRY:
      i = 21;
      break;
    case GD_ALL_ENTRIES:
      i = 22;
      break;
    default:
      i = -1;
      break;
  }

  dreturn("%i", i);
  return i;
}

/* returns true if E a member of the given list */
int _GD_ListEntry(const gd_entry_t *E, int meta_ok, int hidden_ok, int noalias,
    int special, int fragment, gd_entype_t type)
{
  dtrace("%p{%s}, %i, %i, %i, %i, %i, 0x%X", E, E->field, meta_ok, hidden_ok,
      noalias, special, fragment, type);

  /* check hidden */
  if (!hidden_ok && (E->flags & GD_EN_HIDDEN)) {
    dreturn("%i (hidden)", 0);
    return 0;
  }

  /* check meta */
  if (!meta_ok && E->e->n_meta == -1) {
    dreturn("%i (meta)", 0);
    return 0;
  }

  /* check fragment */
  if (fragment != GD_ALL_FRAGMENTS && E->fragment_index != fragment) {
    dreturn("%i (fragment)", 0);
    return 0;
  }

  /* aliases */
  if (E->field_type == GD_ALIAS_ENTRY) {
    int ret = 0;

    if (noalias) {
      dreturn("%i (alias)", 0);
      return 0;
    }

    /* that's right: GD_ALIAS_ENTRY + noalias gets you what you deserve */
    if (special == GD_ALIAS_ENTRIES) {
      dreturn("%i (aliases)", 1);
      return 1;
    }
      
    if (E->e->entry[0])
      ret = _GD_ListEntry(E->e->entry[0], 1, 1, 0, special, GD_ALL_FRAGMENTS,
          type);
    dreturn("%i", ret);
    return ret;
  }

  /* type check */
  if (special == GD_VECTOR_ENTRIES && ((E->field_type & GD_SCALAR_ENTRY_BIT) ||
        (E->field_type == GD_SINDIR_ENTRY)))
  {
    dreturn("%i (vector)", 0);
    return 0;
  } else if (special == GD_SCALAR_ENTRIES &&
      !(E->field_type & GD_SCALAR_ENTRY_BIT))
  {
    dreturn("%i (scalar)", 0);
    return 0;
  } else if (special == GD_ALIAS_ENTRIES) { /* we weeded out aliases earlier */
    dreturn("%i (aliases)", 0);
    return 0;
  } else if (type && E->field_type != type) {
    dreturn("%i (type)", 0);
    return 0;
  }

  dreturn("%i", 1);
  return 1;
}

static const char **_GD_EntryList(DIRFILE *D, struct gd_private_entry_ *p,
    size_t offs, int type, unsigned int flags) gd_nothrow
{
  char** el;
  int i, index;
  unsigned int n;
  const int special = (type & GD_SPECIAL_ENTRY_BIT) ? type : 0;
  const gd_entype_t ctype = (type & GD_SPECIAL_ENTRY_BIT) ? GD_NO_ENTRY :
    (gd_entype_t)type;
  const int hidden = (flags & GD_ENTRIES_HIDDEN);
  const int noalias = (flags & GD_ENTRIES_NOALIAS);
  size_t len = 10;
  gd_entry_t **entry;
  struct gd_flist_ *l;
  int nentries;

  dtrace("%p, %p, %" PRIuSIZE ", 0x%X, 0x%X", D, p, offs, type, flags);

  index = _GD_EntryIndex(type);
  if (index < 0) {
    _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_ENTRY_TYPE, NULL, type, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  if (p) {
    nentries = p->n_meta;
    entry = p->p.meta_entry;
    l = &p->fl;
  } else {
    nentries = D->n_entries;
    entry = D->entry;
    l = &D->fl;
  }

  if (l->entry_list_validity & (1 << index) &&
      l->entry_list_flags[index] == flags)
  {
    /* list already made */
    dreturn("%p", l->entry_list[index]);
    return l->entry_list[index];
  }

  el = _GD_Malloc(D, sizeof(*el) * len);

  if (el == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  for (i = n = 0; i < nentries; ++i) {
    if (n == len - 1) { /* leave space for the terminating NULL */
      void *ptr = _GD_Realloc(D, el, sizeof(*el) * (len *= 2));
      if (ptr == NULL) {
        free(el);
        dreturn("%p", NULL);
        return NULL;
      }
      el = ptr;
    }

    if (_GD_ListEntry(entry[i], p ? 1 : 0, hidden, noalias, special,
          GD_ALL_FRAGMENTS, ctype))
    {
      el[n++] = entry[i]->field + offs;
    }
  }

  if (n == 0) {
    free(el);
    dreturn("%p", zero_list);
    return zero_list;
  }

  el[n] = NULL;

  free(l->entry_list[index]);
  l->entry_list[index] = (const char **)el;
  l->entry_list_flags[index] = flags;
  l->entry_list_validity |= 1 << index;

  dreturn("%p", (const char **)el);
  return (const char **)el;
}

const char **gd_entry_list(DIRFILE* D, const char *parent, int type,
    unsigned int flags) gd_nothrow
{
  const char **el;
  size_t offs = 0;
  struct gd_private_entry_ *p = NULL;

  dtrace("%p, \"%s\", %i, %u", D, parent, type, flags);

  GD_RETURN_IF_INVALID(D, "%p", NULL);

  if (parent) {
    gd_entry_t *P = _GD_FindEntry(D, parent);

    if (P && P->e->n_meta == -1)
      _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_INVALID, NULL, 0, parent);

    if (D->error) {
      dreturn("%u", 0);
      return 0;
    }
    p = P->e;
    offs = strlen(P->field) + 1;
  }

  el = _GD_EntryList(D, p, offs, type, flags);
  dreturn("%p", el);
  return el;
}

static void *_GD_Constants(DIRFILE* D, const char* parent,
    gd_type_t return_type) gd_nothrow
{
  int i, nentries;
  char *fl;
  gd_entry_t *P;
  gd_entry_t **entry;
  void **list;
  struct gd_private_entry_ *e = NULL;
  unsigned n;
  size_t len = 10;

  dtrace("%p, \"%s\", 0x%x", D, parent, return_type);

  GD_RETURN_IF_INVALID(D, "%p", NULL);

  if (return_type == GD_NULL) {
    _GD_SetError(D, GD_E_BAD_TYPE, GD_E_TYPE_NULL, NULL, return_type, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  if (parent) {
    P = _GD_FindEntry(D, parent);

    if (P && P->e->n_meta == -1)
      _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_INVALID, NULL, 0, parent);

    if (D->error) {
      dreturn("%p", NULL);
      return NULL;
    }

    e = P->e;
    nentries = e->n_meta;
    entry = e->p.meta_entry;
    list = &e->fl.const_value_list;
  } else {
    nentries = D->n_entries;
    entry = D->entry;
    list = &D->fl.const_value_list;
  }

  fl = (char *)_GD_Alloc(D, return_type, len);

  if (fl == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  if (return_type != GD_NULL &&
      _GD_BadType(GD_DIRFILE_STANDARDS_VERSION, return_type))
  {
    _GD_SetError(D, GD_E_BAD_TYPE, 0, NULL, return_type, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  /* DoField will implicitly choose GD_REPR_AUTO for complex data being returned
   * as purely real */
  for (i = n = 0; i < nentries; ++i) {
    if (n == len) {
      void *ptr = _GD_Realloc(D, fl, GD_SIZE(return_type) * (len *= 2));
      if (ptr == NULL) {
        free(fl);
        dreturn("%p", NULL);
        return NULL;
      }
      fl = ptr;
    }

    if (_GD_ListEntry(entry[i], e ? 1 : 0, 0, 0, 0, GD_ALL_FRAGMENTS,
          GD_CONST_ENTRY))
    {
      gd_entry_t *E = entry[i];

      if (E->field_type == GD_ALIAS_ENTRY)
        E = E->e->entry[0];

      if (_GD_DoField(D, E, 0, 0, 1, return_type,
            fl + n++ * GD_SIZE(return_type)) != 1)
      {
        break; /* error */
      }
    }
  }

  if (n == 0 || D->error) {
    free(fl);
    fl = NULL;
  }

  free(*list);
  *list = fl;

  dreturn("%p", fl);
  return fl;
}

static gd_carray_t *_GD_Carrays(DIRFILE* D, const char* parent,
    gd_type_t return_type) gd_nothrow
{
  int i, nentries;
  gd_carray_t *fl;
  gd_entry_t *P;
  gd_entry_t **entry;
  gd_carray_t **list;
  struct gd_private_entry_ *e = NULL;
  unsigned n;
  size_t len = 10;

  dtrace("%p, \"%s\", 0x%x", D, parent, return_type);

  GD_RETURN_IF_INVALID(D, "%p", NULL);

  if (parent) {
    P = _GD_FindEntry(D, parent);

    if (P && P->e->n_meta == -1)
      _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_INVALID, NULL, 0, parent);

    if (D->error) {
      dreturn("%p", NULL);
      return NULL;
    }

    e = P->e;
    nentries = e->n_meta;
    entry = e->p.meta_entry;
    list = &e->fl.carray_value_list;
  } else {
    nentries = D->n_entries;
    entry = D->entry;
    list = &D->fl.carray_value_list;
  }

  fl = _GD_Malloc(D, sizeof(*fl) * len);

  if (fl == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  if (return_type != GD_NULL &&
      _GD_BadType(GD_DIRFILE_STANDARDS_VERSION, return_type))
  {
    _GD_SetError(D, GD_E_BAD_TYPE, 0, NULL, return_type, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  /* DoField will implicitly choose GD_REPR_AUTO for complex data being returned
   * as purely real */
  for (i = n = 0; i < nentries; ++i) {
    if (n == len - 1) {
      void *ptr = _GD_Realloc(D, fl, sizeof(*fl) * (len *= 2));
      if (ptr == NULL) {
        free(fl);
        dreturn("%p", NULL);
        return NULL;
      }
      fl = ptr;
    }

    if (_GD_ListEntry(entry[i], e ? 1 : 0, 0, 0, 0, GD_ALL_FRAGMENTS,
          GD_CARRAY_ENTRY))
    {
      gd_entry_t *E = entry[i];

      if (E->field_type == GD_ALIAS_ENTRY)
        E = E->e->entry[0];

      fl[n].n = E->EN(scalar,array_len);
      fl[n].d = _GD_Alloc(D, return_type, fl[n].n);
      if (D->error || _GD_DoField(D, E, 0, 0, fl[n].n, return_type,
            fl[n].d) < 1)
      {
        break;
      }
      n++;
    }
  }

  if (n == 0) {
    free(fl);
    dreturn("%p", zero_carrays);
    return zero_carrays;
  }

  fl[n].n = 0;

  if (*list) {
    for (i = 0; (*list)[i].n != 0; ++i)
      free((*list)[i].d);
    free(*list);
  }

  *list = fl;

  dreturn("%p", D->error ? NULL : fl);
  return D->error ? NULL : fl;
}

static const char **_GD_Strings(DIRFILE* D, const char* parent) gd_nothrow
{
  int i, nentries;
  const char** fl;
  gd_entry_t *P;
  gd_entry_t **entry;
  const char ***list;
  struct gd_private_entry_ *e = NULL;
  unsigned n;
  size_t len = 10;

  dtrace("%p, \"%s\"", D, parent);

  GD_RETURN_IF_INVALID(D, "%p", NULL);

  if (parent) {
    P = _GD_FindEntry(D, parent);

    if (P && P->e->n_meta == -1)
      _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_INVALID, NULL, 0, parent);
    
    if (D->error) {
      dreturn("%p", NULL);
      return NULL;
    }

    e = P->e;
    nentries = e->n_meta;
    entry = e->p.meta_entry;
    list = &e->fl.string_value_list;
  } else {
    nentries = D->n_entries;
    entry = D->entry;
    list = &D->fl.string_value_list;
  }

  fl = _GD_Malloc(D, sizeof(*fl) * len);

  if (fl == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  for (i = n = 0; i < nentries; ++i) {
    if (n == len - 1) {
      void *ptr = _GD_Realloc(D, fl, sizeof(*fl) * (len *= 2));
      if (ptr == NULL) {
        free(fl);
        dreturn("%p", NULL);
        return NULL;
      }
      fl = ptr;
    }

    if (_GD_ListEntry(entry[i], e ? 1 : 0, 0, 0, 0, GD_ALL_FRAGMENTS,
          GD_STRING_ENTRY))
    {
      if (entry[i]->field_type == GD_ALIAS_ENTRY)
        fl[n++] = entry[i]->e->entry[0]->e->u.string;
      else
        fl[n++] = entry[i]->e->u.string;
    }
  }

  if (n == 0) {
    free(fl);
    dreturn("%p", zero_list);
    return zero_list;
  }

  fl[n] = NULL;

  free(*list);
  *list = fl;

  dreturn("%p", fl);
  return fl;
}

static const char ***_GD_SArrays(DIRFILE* D, const char* parent) gd_nothrow
{
  int i, nentries;
  const char ***fl;
  gd_entry_t *P;
  gd_entry_t **entry;
  const char ****list;
  struct gd_private_entry_ *e = NULL;
  size_t n, len = 10;

  dtrace("%p, \"%s\"", D, parent);

  GD_RETURN_IF_INVALID(D, "%p", NULL);

  if (parent) {
    P = _GD_FindEntry(D, parent);

    if (P && P->e->n_meta == -1)
      _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_INVALID, NULL, 0, parent);

    if (D->error) {
      dreturn("%p", NULL);
      return NULL;
    }

    e = P->e;
    nentries = e->n_meta;
    entry = e->p.meta_entry;
    list = &e->fl.sarray_value_list;
  } else {
    nentries = D->n_entries;
    entry = D->entry;
    list = &D->fl.sarray_value_list;
  }

  fl = _GD_Malloc(D, sizeof(*fl) * len);

  if (fl == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  for (i = n = 0; i < nentries; ++i) {
    if (n == len - 1) {
      void *ptr = _GD_Realloc(D, fl, sizeof(*fl) * (len *= 2));
      if (ptr == NULL)
        goto SARRAYS_ERROR;
      fl = ptr;
    }

    if (_GD_ListEntry(entry[i], e ? 1 : 0, 0, 0, 0, GD_ALL_FRAGMENTS,
          GD_SARRAY_ENTRY))
    {
      gd_entry_t *E = entry[i];
      if (E->field_type == GD_ALIAS_ENTRY)
        E = E->e->entry[0];

      /* We do it this way so we can append a NULL */
      fl[n] = _GD_Malloc(D, sizeof(**fl) * (E->EN(scalar,array_len) + 1));
      if (fl[n] == NULL)
        goto SARRAYS_ERROR;

      memcpy(fl[n], E->e->u.scalar.d, sizeof(char*) * E->EN(scalar,array_len));
      fl[n++][E->EN(scalar,array_len)] = NULL;
    }
  }
  fl[n] = NULL;

  if (n == 0) {
    free(fl);
    dreturn("%p", zero_list);
    return (const char***)zero_list;
  }

  if (*list) {
    for (i = 0; (*list)[i] != NULL; ++i)
      free((*list)[i]);
    free(*list);
  }
  *list = fl;

  dreturn("%p", fl);
  return fl;

SARRAYS_ERROR:
  for (i = 0; i < (int)n; ++i)
    free(fl[i]);
  free(fl);
  dreturn("%p", NULL);
  return NULL;
}

const void *gd_constants(DIRFILE* D, gd_type_t return_type) gd_nothrow
{
  void *ret;

  dtrace("%p, 0x%x", D, return_type);

  ret = _GD_Constants(D, NULL, return_type);

  dreturn("%p", ret);
  return ret;
}

const gd_carray_t *gd_carrays(DIRFILE* D, gd_type_t return_type) gd_nothrow
{
  gd_carray_t *ret;

  dtrace("%p, 0x%x", D, return_type);

  ret = _GD_Carrays(D, NULL, return_type);
  
  dreturn("%p", ret);
  return ret;
}

const char **gd_strings(DIRFILE* D) gd_nothrow
{
  const char **ret;

  dtrace("%p", D);

  ret = _GD_Strings(D, NULL);

  dreturn("%p", ret);
  return ret;
}

const char ***gd_sarrays(DIRFILE *D) gd_nothrow
{
  const char ***ret;

  dtrace("%p", D);

  ret = _GD_SArrays(D, NULL);

  dreturn("%p", ret);
  return ret;
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
  void *ret;

  dtrace("%p, \"%s\", 0x%x", D, parent, return_type);

  ret = _GD_Constants(D, parent, return_type);

  dreturn("%p", ret);
  return ret;
}

const gd_carray_t *gd_mcarrays(DIRFILE* D, const char* parent,
    gd_type_t return_type) gd_nothrow
{
  gd_carray_t *ret;

  dtrace("%p, \"%s\", 0x%x", D, parent, return_type);

  ret = _GD_Carrays(D, parent, return_type);

  dreturn("%p", ret);
  return ret;
}

const char **gd_mstrings(DIRFILE* D, const char* parent) gd_nothrow
{
  const char **ret;

  dtrace("%p, \"%s\"", D, parent);

  ret = _GD_Strings(D, parent);

  dreturn("%p", ret);
  return ret;
}

const char ***gd_msarrays(DIRFILE* D, const char* parent) gd_nothrow
{
  const char ***ret;

  dtrace("%p, \"%s\"", D, parent);

  ret = _GD_SArrays(D, parent);

  dreturn("%p", ret);
  return ret;
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

#ifndef GD_NO_REGEX
static void gd_compile_regex(DIRFILE *D, const char *regex, int cflags,
    regex_t *preg)
{
  dtrace("%p, \"%s\", 0x%X, %p", D, regex, cflags, preg);

  int regerr = regcomp(preg, regex, cflags);
  if (regerr) {
    /* Get error string length */
    size_t rerror_len = regerror(regerr, preg, NULL, 0);

    /* Allocate error buffer and save error string, if possible */
    char *rerror = _GD_Malloc(D, rerror_len);
    if (rerror) {
      regerror(regerr, preg, rerror, rerror_len);
      _GD_SetError(D, GD_E_ARGUMENT, GD_E_ARG_REGEX, NULL, 0, rerror);
      free(rerror);
    }

    regfree(preg);
  }
  
  dreturnvoid();
}
#endif

#ifndef GD_NO_PCRE
static void gd_compile_pcre(DIRFILE *D, const char *regex, int options,
    pcre **code)
{
  const char *errptr;
  int erroffset;

  dtrace("%p, \"%s\", 0x%X, %p", D, regex, options, code);

  *code = pcre_compile(regex, options, &errptr, &erroffset, NULL);

  if (*code == NULL) /* Error */
    _GD_SetError(D, GD_E_ARGUMENT, GD_E_ARG_PCRE, NULL, erroffset, errptr);

  dreturnvoid();
}
#endif

unsigned int gd_match_entries(DIRFILE *D, const char *regex, int fragment,
    int type, unsigned int flags, const char ***list) gd_nothrow
{
#ifndef GD_NO_REGEX
  regex_t preg;
  const int regex_flags = ((flags & GD_REGEX_EXTENDED) ? REG_EXTENDED : 0)
    | ((flags & GD_REGEX_ICASE) ? REG_ICASE : 0) | REG_NOSUB;
#endif
#ifndef GD_NO_PCRE
  pcre *pcre_code = NULL;
  const int pcre_exec_options =
    (flags & GD_REGEX_UNICODE) ? PCRE_BSR_UNICODE : PCRE_BSR_ANYCRLF;
  const int pcre_compile_options = pcre_exec_options
    | PCRE_DOLLAR_ENDONLY | PCRE_DOTALL
    | ((flags & GD_REGEX_JAVASCRIPT) ? PCRE_JAVASCRIPT_COMPAT : 0)
    | ((flags & GD_REGEX_EXTENDED) ? PCRE_EXTENDED : 0)
    | ((flags & GD_REGEX_ICASE) ? PCRE_CASELESS : 0)
    | ((flags & GD_REGEX_UNICODE) ? PCRE_UTF8 : 0)
    ;
#endif
  unsigned int len = 10, n = 0, i;

  const int special = (type & GD_SPECIAL_ENTRY_BIT) ? type : 0;
  const gd_entype_t ctype =
    (type & GD_SPECIAL_ENTRY_BIT) ? GD_NO_ENTRY : (gd_entype_t)type;
  const int hidden = (flags & GD_ENTRIES_HIDDEN);
  const int noalias = (flags & GD_ENTRIES_NOALIAS);
  const int use_pcre = flags & GD_REGEX_PCRE;

  dtrace("%p, \"%s\", %i, %i, 0x%X, %p", D, regex, fragment, type, flags, list);

  if (list)
    *list = NULL;

  GD_RETURN_IF_INVALID(D, "%i", 0);

  /* We don't cache the result of this function */
  if (D->regex_list) {
    free(D->regex_list);
    D->regex_list = NULL;
  }

  /* Check type */
  if (_GD_EntryIndex(type) < 0) {
    _GD_SetError(D, GD_E_BAD_ENTRY, GD_E_ENTRY_TYPE, NULL, type, NULL);
    dreturn("%i", 0);
    return 0;
  }

  if (list) {
    D->regex_list = _GD_Malloc(D, sizeof(D->regex_list[0]) * len);
    if (D->regex_list == NULL) {
      dreturn("%i", 0);
      return 0;
    }
  }

  if (regex) {
    /* Compile */
    if (use_pcre) {
#ifndef GD_NO_PCRE
      gd_compile_pcre(D, regex, pcre_compile_options, &pcre_code);
#else
      _GD_SetError(D, GD_E_UNSUPPORTED, GD_E_SUPPORT_REGEX, NULL, 0, "PCRE");
#endif
    } else {
      /* POSIX regex */
#ifndef GD_NO_REGEX
      gd_compile_regex(D, regex, regex_flags, &preg);
#else
      _GD_SetError(D, GD_E_UNSUPPORTED, GD_E_SUPPORT_REGEX, NULL, 0, "POSIX");
#endif
    }

    if (D->error) {
      free(D->regex_list);
      D->regex_list = NULL;
      dreturn("%i", 0);
      return 0;
    }
  }

  /* Regex searches are always performed against the full entry list */
  for (i = n = 0; i < D->n_entries; ++i) {
    if (list && n == len - 1) { /* leave space for the terminating NULL */
      void *ptr = _GD_Realloc(D, D->regex_list, sizeof(D->regex_list[0])
          * (len *= 2));
      if (ptr == NULL) {
#ifndef GD_NO_REGEX
        if (regex)
          regfree(&preg);
#endif
        free(D->regex_list);
        D->regex_list = NULL;
        dreturn("%i", 0);
        return 0;
      }
      D->regex_list = ptr;
    }

    /* Check Dirfile metadata */
    if (_GD_ListEntry(D->entry[i], 1, hidden, noalias, special, fragment,
          ctype))
    {
      /* Run the regex, if provided */
      if (regex == NULL
#ifndef GD_NO_PCRE
          || (use_pcre && pcre_exec(pcre_code, NULL, D->entry[i]->field,
              D->entry[i]->e->len, 0, pcre_exec_options, NULL, 0) == 0)
#endif
#ifndef GD_NO_REGEX
          || (!use_pcre && regexec(&preg, D->entry[i]->field, 0, NULL, 0) == 0)
#endif
         )
      {
        if (list)
          D->regex_list[n] = D->entry[i]->field;
        n++;
      }
    }
  }

  if (regex) {
#ifndef GD_NO_REGEX
    if (!use_pcre)
      regfree(&preg);
#endif
#ifndef GD_NO_PCRE
    if (use_pcre)
      pcre_free(pcre_code);
#endif
  }

  if (list) {
    if (n == 0) {
      free(D->regex_list);
      D->regex_list = NULL;
      *list = zero_list;
    } else {
      D->regex_list[n] = NULL;
      *list = D->regex_list;
    }
  }
  dreturn("%u", n);
  return n;
}
