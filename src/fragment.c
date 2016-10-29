/* Copyright (C) 2008, 2010, 2011, 2012, 2014, 2015, 2016 D. V. Wiebe
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

  GD_RETURN_IF_INVALID(D, "%p", NULL);

  if (index < 0 || index >= D->n_fragment) {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, index, NULL);
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

  GD_RETURN_ERR_IF_INVALID(D);

  if (index < 0 || index >= D->n_fragment)
    GD_SET_RETURN_ERROR(D, GD_E_BAD_INDEX, 0, NULL, index, NULL);

  /* Copy to caller's heap */
  if (D->fragment[index].prefix) {
    p = _GD_CStrdup(D->fragment[index].prefix);
    if (p == NULL)
      _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
  }
  if (D->fragment[index].suffix) {
    s = _GD_CStrdup(D->fragment[index].suffix);
    if (s == NULL)
      _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
  }

  if (D->error) {
    free(p);
    free(s);
    GD_RETURN_ERROR(D);
  }

  *prefix = p;
  *suffix = s;
  dreturn("%i", 0);
  return 0;
}

static char **_GD_CheckAffixes(DIRFILE *D, int i, const char *prefix,
    const char *suffix, char **codes, unsigned *n)
{
  int j;
  unsigned u, nn = *n;
  char **new_codes = codes, **ptr;

  dtrace("%p, %i, \"%s\", \"%s\", %p, %p", D, i, prefix, suffix, new_codes, n);

  ptr = _GD_Realloc(D, codes, sizeof(*ptr) * (nn + 2));
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
      char *subprefix = _GD_MungeCode(D, NULL, 0, D->fragment[i].prefix,
          NULL, prefix, NULL, D->fragment[j].prefix, NULL, NULL,
          GD_MC_RQ_PARTS | GD_MC_ERROR_OK);
      char *subsuffix = _GD_MungeCode(D, NULL, 0, NULL,
          D->fragment[i].suffix, NULL, suffix, D->fragment[j].suffix, NULL,
          NULL, GD_MC_RQ_PARTS | GD_MC_ERROR_OK);
      if (D->error) {
        free(subprefix);
        dreturn("%p (%i)", new_codes, *n);
        return new_codes;
      }

      new_codes = _GD_CheckAffixes(D, j, subprefix, subsuffix, new_codes, n);
      free(subprefix);
      free(subsuffix);
      nn = *n;

      if (D->error) {
        dreturn("%p (%i)", new_codes, *n);
        return new_codes;
      }
    }

  /* Check for namespace clashes in our files */
  for (u = 0; u < D->n_entries; ++u)
    if (D->entry[u]->fragment_index == i && D->entry[u]->e->n_meta != -1) {
      ptr = _GD_Realloc(D, new_codes, sizeof(*ptr) * ++nn);
      if (ptr) {
        char *nso;
        new_codes = ptr;
        /* remunge the code */
        new_codes[nn - 1] = _GD_MungeCode(D, NULL, 0, D->fragment[i].prefix,
            D->fragment[i].suffix, prefix, suffix, D->entry[u]->field, &nso,
            NULL, GD_MC_RQ_PARTS);

        /* look for a duplicate and validate */
        if (new_codes[nn - 1] && _GD_FindField(D, new_codes[nn - 1], D->entry,
              D->n_entries, 0, NULL))
        {
          _GD_SetError(D, GD_E_DUPLICATE, 0, NULL, 0, new_codes[nn - 1]);
        } else if (_GD_ValidateField(nso, D->standards, 1, GD_VF_NAME, NULL)) {
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

  GD_RETURN_ERR_IF_INVALID(D);

  if (index <= 0 || index >= D->n_fragment) 
    GD_SET_RETURN_ERROR(D, GD_E_BAD_INDEX, 0, NULL, index, NULL);
  else if ((D->flags & GD_ACCMODE) == GD_RDONLY)
    GD_SET_RETURN_ERROR(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
  else if (D->fragment[D->fragment[index].parent].protection &
      GD_PROTECT_FORMAT)
  {
    GD_SET_RETURN_ERROR(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[D->fragment[index].parent].cname);
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
    GD_RETURN_ERROR(D);
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

  GD_RETURN_ERR_IF_INVALID(D);

  dreturn("%i", D->n_fragment);
  return D->n_fragment;
}

int gd_parent_fragment(DIRFILE* D, int fragment_index) gd_nothrow
{
  dtrace("%p, %i", D, fragment_index);

  GD_RETURN_ERR_IF_INVALID(D);

  if (fragment_index <= 0 || fragment_index >= D->n_fragment)
    GD_SET_RETURN_ERROR(D, GD_E_BAD_INDEX, 0, NULL, fragment_index, NULL);

  dreturn("%i", D->fragment[fragment_index].parent);
  return D->fragment[fragment_index].parent;
}

/* returns non-zero if the metadata has changed on disk since the dirfile was
 * opened and, optionally, re-opens the dirfile.
 */
int gd_desync(DIRFILE *D, unsigned int flags)
{
  int changed = 0, i;
  struct stat statbuf;

  dtrace("%p, 0x%x", D, flags);

  GD_RETURN_ERR_IF_INVALID(D);

  /* if we can't open directories, we're stuck with the full path method */
#ifdef GD_NO_DIR_OPEN
  flags |= GD_DESYNC_PATHCHECK;
#endif

  for (i = 0; i < D->n_fragment; ++i) {
    if (flags & GD_DESYNC_PATHCHECK) {
      /* stat the file via it's path relative to the original filedir */
      char *buffer;
      if (D->fragment[i].sname) {
        buffer = _GD_Malloc(D, strlen(D->name) + strlen(D->fragment[i].bname) +
            strlen(D->fragment[i].sname) + 3);
        if (buffer == NULL)
          GD_RETURN_ERROR(D);
        sprintf(buffer, "%s%c%s%c%s", D->name, GD_DIRSEP, D->fragment[i].sname,
            GD_DIRSEP, D->fragment[i].bname);
      } else {
        buffer = _GD_Malloc(D, strlen(D->name) + strlen(D->fragment[i].bname) +
            2);
        if (buffer == NULL)
          GD_RETURN_ERROR(D);
        sprintf(buffer, "%s%c%s", D->name, GD_DIRSEP, D->fragment[i].bname);
      }
      if (stat(buffer, &statbuf)) {
        _GD_SetError(D, GD_E_IO, 0, buffer, 0, NULL);
        free(buffer);
        GD_RETURN_ERROR(D);
      }
      free(buffer);
    } else
      /* stat the file based on it's name and our cached dirfd */
      if (gd_StatAt(D, D->fragment[i].dirfd, D->fragment[i].bname, &statbuf, 0))
        GD_SET_RETURN_ERROR(D, GD_E_IO, 0, D->fragment[i].cname, 0, NULL);

    if (statbuf.st_mtime != D->fragment[i].mtime) {
      changed = 1;
      break;
    }
  }

  if (changed && flags & GD_DESYNC_REOPEN) {
    /* reopening is easy: just delete everything and start again.  In the
     * non-PATHCHECK case, we also have to cache the dirfd to the root directory
     */

    /* remember how we were called */
    char *name = D->name;
    gd_parser_callback_t sehandler = D->sehandler;
    void *extra = D->sehandler_extra;
    unsigned long int flags = D->open_flags;
    int dirfd = -1;

    if (!(flags & GD_DESYNC_PATHCHECK)) {
      dirfd = dup(D->fragment[0].dirfd);
      if (dirfd == -1)
        GD_SET_RETURN_ERROR(D, GD_E_IO, GD_E_OPEN, D->name, 0, NULL);
    }

    D->name = NULL; /* so FreeD doesn't delete it */
    if (_GD_ShutdownDirfile(D, 0, 1)) {
      D->name = name;
      if (dirfd >= 0)
        close(dirfd);
      GD_RETURN_ERROR(D);
    }
    _GD_Open(D, dirfd, name, flags, sehandler, extra);
    free(name);

    if (D->error)
      changed = D->error;
  }

  dreturn("%i", changed);
  return changed;
}

/* create a list of indices to subfragments, including this one */
static int _GD_SubFragmentList(DIRFILE *restrict D, int i, int **restrict ilist)
{
  int *list;
  int j, n = 1;

  dtrace("%p, %i, %p", D, i, ilist);

  /* i is always in the list */
  list = _GD_Malloc(D, sizeof(*list));
  if (list == NULL) {
    dreturn("%i", 0);
    return 0;
  }
  list[0] = i;

  /* search for children */
  for (j = 0; j < D->n_fragment; ++j)
    if (D->fragment[j].parent == i) {
      int nsub, *sublist, *ptr;

      nsub = _GD_SubFragmentList(D, j, &sublist);
      if (nsub == 0) {
        free(list);
        dreturn("%i", 0);
        return 0;
      }

      /* append to current list */
      ptr = _GD_Realloc(D, list, (nsub + n) * sizeof(*list));
      if (ptr == NULL) {
        free(list);
        dreturn("%i", 0);
        return 0;
      }
      list = ptr;
      memcpy(list + n, sublist, nsub * sizeof(*list));
      free(sublist);

      n += nsub;
    }

  /* done */
  *ilist = list;
  dreturn("%i", n);
  return n;
}

/* Check a fragment namespace change.  Returns non-zero if the checks fail. */
static int _GD_CheckChangeNamespace(DIRFILE *D, const char *newns,
    size_t newnsl, size_t oldnsl, int i, char ***codes)
{
  int j, ni;
  unsigned u;
  int *ilist;
  char **c = *codes;
  ssize_t dnsl;

  dtrace("%p, \"%s\", %" PRIuSIZE ", %" PRIuSIZE ", %i, %p", D, newns, newnsl,
      oldnsl, i, codes);

  dnsl = newnsl - oldnsl;
  if (oldnsl == 0)
    dnsl++; /* Because we need to add the missing '.' in this case */
  else if (newnsl == 0)
    dnsl--; /* Because we need to remove the '.' in this case */

  /* find all affected fragments */
  ni = _GD_SubFragmentList(D, i, &ilist);
  if (ni == 0) { /* malloc error */
    free(*codes);
    dreturn("%i", 1);
    return 1;
  }

  /* find all affected entries and generate new field codes */
  for (u = 0; u < D->n_entries; ++u) {
    /* check fragment list */
    int found = 0;
    size_t l;

    for (j = 0; j < ni; ++j)
      if (D->entry[u]->fragment_index == ilist[j]) {
        found = 1;
        break;
      }

    if (!found) {
      c[u] = NULL;
      continue;
    }

    /* compose the new name */
    l = strlen(D->entry[u]->field);
    c[u] = _GD_Malloc(D, l + dnsl + 1);
    if (c[u] == NULL)
      goto CHECKS_FAILED;

    if (oldnsl == 0)
      /* Need to add a dot in this case */
      sprintf(c[u], "%s.%s", newns, D->entry[i]->field); 
    else if (newnsl == 0)
      /* Need to delete a dot in this case */
      strcpy(c[u], D->entry[i]->field + oldnsl + 1);
    else { /* so much easier than dealing with affixes... */
      strncpy(c[u], newns, newnsl);
      strcpy(c[u] + newnsl, D->entry[i]->field + oldnsl);
    }

    /* Check whether it already exists */
    if (_GD_FindField(D, c[u], D->entry, D->n_entries, 1, NULL)) {
      _GD_SetError(D, GD_E_DUPLICATE, 0, NULL, 0, c[u]);
      goto CHECKS_FAILED;
    }
  }

  free(ilist);
  dreturn("%i", 0);
  return 0;

CHECKS_FAILED:
  do { 
    free(c[u]);
  } while (u--);
  free(c);
  free(ilist);
  dreturn("%i", 1);
  return 1;
}

const char *gd_fragment_namespace(DIRFILE *D, int index, const char *ns)
{
  dtrace("%p, %i, \"%s\"", D, index, ns);

  _GD_ClearError(D);

  GD_RETURN_IF_INVALID(D, "%p", NULL);

  if (index < 0 || index >= D->n_fragment) {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, index, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  if (ns) {
    unsigned u;
    char **codes, *newns, *pns;
    size_t newnsl, pnsl;

    const char *oldns = D->fragment[index].ns;
    size_t oldnsl = D->fragment[index].nsl;

    if (index == 0)
      _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, index, NULL);
    else if ((D->flags & GD_ACCMODE) == GD_RDONLY)
      _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    else if (D->fragment[D->fragment[index].parent].protection
        & GD_PROTECT_FORMAT)
    {
      _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
          D->fragment[D->fragment[index].parent].cname);
    } else if (_GD_ValidateField(ns, D->standards, 1, GD_VF_NS, NULL))
      /* invalid namespace */
      _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_INVALID, NULL, 0, ns);

    if (D->error) {
      dreturn("%p", NULL);
      return NULL;
    }

    /* remove spurious leading dot */
    if (ns[0] == '.')
      ns++;

    pns = D->fragment[D->fragment[index].parent].ns;
    pnsl = D->fragment[D->fragment[index].parent].nsl;

    newnsl = strlen(ns);

    if (ns[0] == '\0') {
      /* Duplicate parent space */
      newnsl = pnsl;
      if (pns == NULL) {
        newns = NULL;
        newnsl = 0;
      } else {
        newns = _GD_Strdup(D, pns);
        if (newns == NULL) {
          dreturn("%p", NULL);
          return NULL;
        }
      }
    } else if (pns) {
      /* append */
      newnsl += pnsl + 1;
      newns = _GD_Malloc(D, newnsl + 1);

      if (newns == NULL) {
        dreturn("%p", NULL);
        return NULL;
      }
      strcpy(newns, pns);
      newns[pnsl] = '.';
      strcpy(newns + pnsl + 1, ns);
    } else {
      /* new top-level space */
      newns = _GD_Strdup(D, ns);
      if (newns == NULL) {
        dreturn("%p", NULL);
        return NULL;
      }
    }

    /* strip trailing dot */
    if (newns && newns[newnsl - 1] == '.')
      newns[--newnsl] = '\0';

    /* check if it's the same, if so, nothing to do */
    if ((newns == NULL && oldns == NULL) ||
        (newns && oldns && strcmp(newns, oldns) == 0))
    {
      free(newns);
      goto NS_UNCHANGED;
    }
    
    /* array of new field codes */
    codes = _GD_Malloc(D, sizeof(*codes) * D->n_entries);
    if (codes == NULL) {
      free(newns);
      dreturn("%p", NULL);
      return NULL;
    }

    /* check whether the rename can be accomplished */
    if (_GD_CheckChangeNamespace(D, newns, newnsl, oldnsl, index, &codes)) {
      /* on failure, codes has already been freed */
      free(newns);
      dreturn("%p", NULL);
      return NULL;
    }

    /* update the codes */
    for (u = 0; u < D->n_entries; ++u)
      if (codes[u]) {
        free(D->entry[u]->field);
        D->entry[u]->field = codes[u];
      }
    free(codes);

    /* update the fragment it self, at the end */
    free(D->fragment[index].ns);
    D->fragment[index].ns = newns;
    D->fragment[index].nsl = newnsl;
  }

NS_UNCHANGED:
  dreturn("\"%s\"", D->fragment[index].ns ? D->fragment[index].ns : "");
  return  D->fragment[index].ns ? (const char*)D->fragment[index].ns : "";
}
