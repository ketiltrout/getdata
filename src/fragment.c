/* Copyright (C) 2008,2010,2011 D. V. Wiebe
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

const char *gd_fragmentname(DIRFILE* D, int index) gd_nothrow
{
  dtrace("%p, %i", D, index);

  _GD_ClearError(D);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  if (index < 0 || index >= D->n_fragment) {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  dreturn("\"%s\"", D->fragment[index].cname);
  return D->fragment[index].cname;
}

int gd_fragment_affixes(DIRFILE *D, int index, char **prefix, char **suffix)
  gd_nothrow
{
  char *p = NULL, *s = NULL;
  dtrace("%p, %i, %p, %p", D, index, prefix, suffix);

  _GD_ClearError(D);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (index < 0 || index >= D->n_fragment) {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (D->fragment[index].prefix)
    p = _GD_Strdup(D, D->fragment[index].prefix);
  if (D->fragment[index].suffix)
    s = _GD_Strdup(D, D->fragment[index].suffix);

  if (D->error) {
    free(p);
    free(s);
    dreturn("%i", -1);
    return -1;
  }

  *prefix = p;
  *suffix = s;
  dreturn("%i", 0);
  return 0;
}

static char **_GD_CheckAffixes(DIRFILE *D, int i, const char *prefix,
    const char *suffix, char **codes, unsigned *n)
{
  int j, dummy;
  unsigned u, nn = *n;
  char **new_codes = codes, **ptr;

  dtrace("%p, %i, \"%s\", \"%s\", %p, %p", D, i, prefix, suffix, new_codes, n);

  ptr = _GD_Realloc(D, codes, sizeof(char*) * (nn + 2));
  if (!ptr) {
    dreturn("%p (%i)", codes, *n);
    return codes;
  }
  new_codes = ptr;

  /* push the new prefix and suffix onto the code stack */
  new_codes[nn++] = prefix ? _GD_Strdup(D, prefix) : NULL;
  new_codes[nn++] = suffix ? _GD_Strdup(D, suffix) : NULL;
  *n = nn;

  if (D->error) {
    dreturn("%p (%i)", new_codes, *n);
    return new_codes;
  }

  /* Propagate changes downward */
  for (j = 0; j < D->n_fragment; ++j)
    if (D->fragment[j].parent == i) {
      char *subprefix = _GD_MungeCode(D, NULL, D->fragment[i].prefix, NULL,
          prefix, NULL, D->fragment[j].prefix, &dummy);
      char *subsuffix = _GD_MungeCode(D, NULL, NULL, D->fragment[i].suffix,
          NULL, suffix, D->fragment[j].suffix, &dummy);
      if (D->error) {
        dreturn("%p (%i)", new_codes, *n);
        return new_codes;
      }

      new_codes = _GD_CheckAffixes(D, j, subprefix, subsuffix, new_codes, n);
      nn = *n;

      if (D->error) {
        dreturn("%p (%i)", new_codes, *n);
        return new_codes;
      }
    }

  /* Check for namespace clashes in our files */
  for (u = 0; u < D->n_entries; ++u)
    if (D->entry[u]->fragment_index == i && D->entry[u]->e->n_meta != -1) {
      ptr = _GD_Realloc(D, new_codes, sizeof(char*) * ++nn);
      if (ptr) {
        new_codes = ptr;
        /* remunge the code */
        new_codes[nn - 1] = _GD_MungeCode(D, NULL, D->fragment[i].prefix,
            D->fragment[i].suffix, prefix, suffix, D->entry[u]->field, &dummy);

        /* look for a duplicate and validate */
        if (new_codes[nn - 1] && _GD_FindField(D, new_codes[nn - 1], D->entry,
              D->n_entries, 1, NULL))
        {
          _GD_SetError(D, GD_E_DUPLICATE, 0, NULL, 0, new_codes[nn - 1]);
        } else if (_GD_ValidateField(new_codes[nn - 1], D->standards, 1, 0,
              NULL))
        {
          _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_INVALID, NULL, 0,
              new_codes[nn - 1]);
        }
      } else
        nn--;

      if (D->error)
        break;
    }

  *n = nn;
  dreturn("%p (%i)", new_codes, *n);
  return new_codes;
}

static int _GD_ChangeAffixes(DIRFILE *D, int i, char **codes, int *resort)
{
  int j;
  unsigned u, n = 2;

  dtrace("%p, %i, %p, %p", D, i, codes, resort);

  free(D->fragment[i].prefix);
  free(D->fragment[i].suffix);
  D->fragment[i].prefix = codes[0];
  D->fragment[i].suffix = codes[1];

  /* Propagate changes downward */
  for (j = 0; j < D->n_fragment; ++j)
    if (D->fragment[j].parent == i)
      n += _GD_ChangeAffixes(D, j, codes + n, resort);

  /* rename all the fields */
  for (u = 0; u < D->n_entries; ++u)
    if (D->entry[u]->fragment_index == i && D->entry[u]->e->n_meta != -1) {
      *resort = 1;
      free(D->entry[u]->field);
      D->entry[u]->field = codes[n++];
    }

  dreturn("%i", n);
  return n;
}

int gd_alter_affixes(DIRFILE *D, int index, const char *prefix,
    const char *suffix) gd_nothrow
{
  unsigned u, n = 0;
  char **new_codes;
  int resort = 0;
  dtrace("%p, %i, \"%s\", \"%s\"", D, index, prefix, suffix);

  _GD_ClearError(D);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (index <= 0 || index >= D->n_fragment) {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* affixes to keep */
  if (!prefix)
    prefix = D->fragment[index].prefix;

  if (!suffix)
    suffix = D->fragment[index].suffix;

  /* nothing to do */
  if (strcmp(prefix, D->fragment[index].prefix) == 0 &&
      strcmp(suffix, D->fragment[index].suffix) == 0)
  {
    dreturn("%i", 0);
    return 0;
  }

  new_codes = _GD_CheckAffixes(D, index, prefix, suffix, NULL, &n);

  if (D->error) {
    for (u = 0; u < n; ++u)
      free(new_codes[u]);
    free(new_codes);
    dreturn("%i", -1);
    return -1;
  }

  _GD_ChangeAffixes(D, index, new_codes, &resort);

  free(new_codes);

  if (resort) {
    /* resort */
    qsort(D->entry, D->n_entries, sizeof(gd_entry_t*), _GD_EntryCmp);
    qsort(D->dot_list, D->n_dot, sizeof(gd_entry_t*), _GD_EntryCmp);
  }

  dreturn("%i", 0);
  return 0;
}

int gd_nfragments(DIRFILE* D) gd_nothrow
{
  dtrace("%p", D);

  _GD_ClearError(D);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", 0);
    return 0;
  }

  dreturn("%i", D->n_fragment);
  return D->n_fragment;
}

int gd_parent_fragment(DIRFILE* D, int fragment_index) gd_nothrow
{
  dtrace("%p, %i", D, fragment_index);

  _GD_ClearError(D);

  if (D->flags & GD_INVALID) {
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (fragment_index <= 0 || fragment_index >= D->n_fragment) {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  dreturn("%i", D->fragment[fragment_index].parent);
  return D->fragment[fragment_index].parent;
}
