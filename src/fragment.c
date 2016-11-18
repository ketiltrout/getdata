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

int gd_fragment_affixes(DIRFILE *D, int index, char **px, char **sx) gd_nothrow
{
  char *p = NULL, *s = NULL;
  dtrace("%p, %i, %p, %p", D, index, px, sx);

  GD_RETURN_ERR_IF_INVALID(D);

  if (index < 0 || index >= D->n_fragment)
    GD_SET_RETURN_ERROR(D, GD_E_BAD_INDEX, 0, NULL, index, NULL);

  /* Copy to caller's heap */
  if (D->fragment[index].px) {
    p = _GD_CStrdup(D->fragment[index].px);
    if (p == NULL)
      _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
  }
  if (D->fragment[index].sx) {
    s = _GD_CStrdup(D->fragment[index].sx);
    if (s == NULL)
      _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
  }

  if (D->error) {
    free(p);
    free(s);
    GD_RETURN_ERROR(D);
  }

  *px = p;
  *sx = s;
  dreturn("%i", 0);
  return 0;
}

/* Replace the fragment rootspace, prefix and suffix of a field code with
 * something else.  Name verification and duplicate checks are performed.
 * Returns NULL on error or else a newly malloc'd string.
 *
 * The input code is this:
 *
 *     AAAA.BBBB.CCCC.DDDDEEEEGGGGHHHHIIII/KKKK.M0
 *
 * (See the definitions in the description of _GD_CodeOffsets in name.c).
 *
 * We replace BBBB. with new_ns (which is guaranteed to have the trailing '.'),
 * EEEE with new_px and HHHH with new_sx, if they exist.  If the new codes are
 * NULL, no change is made, but if they're the empty string, the old parts are
 * deleted without adding new parts.
 *
 * For field name updates, this is the complement of _GD_RenameCode in name.c,
 * which replaces CCCC. and GGGG, or /KKKK but leaves the rest unchanged.
 * (AAAA., DDDD, and IIII are immutable, because they come from the parent's
 * scope.)
 */
char *_GD_UpdateCode(DIRFILE *D, int index, const char *code, int early,
    const char *new_ns, size_t new_nsl, const char *new_px, size_t new_pxl,
    const char *new_sx, size_t new_sxl)
{
  char *new_code, *ptr;
  size_t new_len, offset[GD_N_CODEOFFSETS];

  dtrace("%p, %i, \"%s\", %i, \"%s\", %" PRIuSIZE ", \"%s\", %" PRIuSIZE
      ", \"%s\", %" PRIuSIZE, D, index, code, early, new_ns, new_nsl, new_px,
      new_pxl, new_sx, new_sxl);

  /* Slice and dice */
  _GD_CodeOffsets(D, index, code, early ? GD_CO_EARLY : 0, offset);

  /* calculate the new code's length */
  new_len = offset[9]; /* i.e. strlen(code) */

  /* Only update these parts if we're changing them. */
  if (new_ns)
    new_len += new_nsl - (offset[1] - offset[0]);

  if (new_px)
    new_len += new_pxl - (offset[4] - offset[3]);

  if (new_sx)
    new_len += new_sxl - (offset[6] - offset[5]);

  new_code = _GD_Malloc(D, new_len + 1);
  if (new_code == NULL) {
    dreturn("%p", NULL);
    return NULL;
  }

  /* Copy the stuff before the fragment local rootspace (AAAA.) */
  if (offset[0])
    memcpy(new_code, code, offset[0]);
  ptr = new_code + offset[0];

  if (new_ns && new_ns[0]) {
    /* Add the new rootspace (including trailing '.') */
    memcpy(ptr, new_ns, new_nsl);
    ptr += new_nsl;
  } else if (new_ns == NULL && offset[1] > offset[0]) {
    /* Copy the old rootspace (BBBB.) */
    memcpy(ptr, code + offset[0], offset[1] - offset[0]);
    ptr = new_code + offset[1];
  }

  /* Copy the subnamespace and parent prefix, if any (CCCC.DDDD) */
  if (offset[3] > offset[1]) {
    memcpy(ptr, code + offset[1], offset[3] - offset[1]);
    ptr += offset[3] - offset[1];
  }

  if (new_px && new_px[0]) {
    /* Add the new prefix */
    memcpy(ptr, new_px, new_pxl);
    ptr += new_pxl;
  } else if (new_px == NULL && offset[4] > offset[3]) {
    /* Copy the old prefix (EEEE) */
    memcpy(ptr, code + offset[3], offset[4] - offset[3]);
    ptr += offset[4] - offset[3];
  }

  /* Copy the field name (which may contain subaffixes).  This is guaranteed
   * to be present. (GGGG) */
  memcpy(ptr, code + offset[4], offset[5] - offset[4]);
  ptr += offset[5] - offset[4];

  if (new_sx && new_sx[0]) {
    /* Add the new suffix */
    memcpy(ptr, new_sx, new_sxl);
    ptr += new_sxl;
  } else if (new_sx == NULL && offset[6] > offset[5]) {
    /* Copy the old sx (HHHH) */
    memcpy(ptr, code + offset[5], offset[6] - offset[5]);
    ptr += offset[6] - offset[5];
  }

  /* Copy the trailing garbage (IIII[/KKKK].M) */
  if (offset[9] > offset[6]) {
    memcpy(ptr, code + offset[6], offset[9] - offset[6]);
    ptr += offset[9] - offset[6];
  }
  
  /* And terminate */
  *ptr = 0;

  /* Now validate and check for duplicate -- metafields don't need to
   * validate */
  if (offset[7] == offset[8] && _GD_ValidateField(new_code, 0, D->standards, 1,
        GD_VF_CODE))
  {
    _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_INVALID, NULL, 0, new_code);
  } else if (_GD_FindField(D, new_code, D->entry, D->n_entries, 0, NULL))
    _GD_SetError(D, GD_E_DUPLICATE, 0, NULL, 0, new_code);
  else {
    /* field code okay */
    dreturn("\"%s\"", new_code);
    return new_code;
  }

  /* Checks failed */
  free(new_code);
  dreturn("%p", NULL);
  return NULL;
}

/* create a list of indices to subfragments, including this one */
int _GD_SubFragmentList(DIRFILE *restrict D, int i, int **restrict ilist)
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

/* Update code affixes for a fragment (including subfragments) with the new
 * values.  nsalloc is 1 if ns has been malloc'd by the caller (in which case
 * this function will steal it).  Returns D->error.
 *
 * When nsalloc is 1, the caller must ensure nsin has a trailing '.' 
 */
static int _GD_UpdateAffixes(DIRFILE *D, int index, const char *nsin,
    int nsalloc, const char *pxin, const char *sxin)
{
  int ni, resort = 0;
  int *ilist;
  size_t u, nsl = 0, pxl = 0, sxl = 0;
  char *fullns = NULL, *fullpx = NULL;
  char *ns = NULL, *px = NULL, *sx = NULL;
  char **codes;
  const struct gd_fragment_t *P;
  struct gd_fragment_t *F;

  dtrace("%p, %i, \"%s\", %i, \"%s\", \"%s\"", D, index, nsin, nsalloc, pxin,
      sxin);

  F = D->fragment + index;
  P = D->fragment + D->fragment[index].parent;

  /* Forget about things that aren't changing. */
  if (nsin && F->ns && strcmp(nsin, F->ns + P->nsl) == 0)
    nsin = NULL;

  if (pxin && F->px && strcmp(pxin, F->px + P->pxl) == 0)
    pxin = NULL;

  if (sxin) {
    sxl = strlen(sxin);
    if (F->sx && sxl == F->sxl && strncmp(sxin, F->sx, F->sxl) == 0)
      sxin = NULL;
  }

  /* Nothing to do */
  if (nsin == NULL && pxin == NULL && sxin == NULL) {
    dreturn("%i", 0);
    return 0;
  }

  /* Compose the full namespace (i.e. containing the parent's space).
   * This will eventually be used to replace the existing F->ns. */
  if (nsin) {
    nsl = strlen(nsin);

    if (nsl == 0) {
      /* nsin is ""; fullns will be P->ns if it exists */
      if (P->ns)
        fullns = _GD_Strdup(D, P->ns);

      /* but ns remains the empty string */
      ns = "";
      if (nsalloc) {
        free((char*)nsin); /* Don't need this anymore */
        nsalloc = 0;
      }
    } else if (_GD_ValidateField(nsin, 0, D->standards, 1, GD_VF_NS)) {
      /* invalid namespace */
      _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_INVALID, NULL, 0, nsin);
    } else if (nsalloc && P->nsl == 0) {
      /* In this case we can just use the caller's string verbatim */
      fullns = ns = (char*)nsin;
      nsalloc = 0;
    } else {
      size_t len;
      if (!nsalloc)
        nsl++; /* space to add the trailing '.' */

      len = P->nsl + nsl;
      if (P->nsl)
        len++; /* space for the intervening '.' */

      fullns = _GD_Malloc(D, len + 1);
      if (fullns) {
        if (P->ns) {
          /* We copy from F not P because it already has the
           * intervening '.' */
          memcpy(fullns, F->ns, P->nsl + 1);
          ns = fullns + P->nsl + 1;
        } else
          ns = fullns;

        if (nsalloc) {
          /* We copy the trailing '.' and NUL here */
          memcpy(ns, nsin, nsl + 1);
          free((char*)nsin); /* Don't need this anymore */
          nsalloc = 0;
        } else {
          memcpy(ns, nsin, nsl - 1); /* No trailing '.' here */
          ns[nsl - 1] = '.'; /* So, add it */
          ns[nsl] = 0; /* And terminate */
        }
      }
    }
  }

  if (!D->error && pxin) {
    if (pxin[0] && _GD_ValidateField(pxin, 0, D->standards, 1, GD_VF_AFFIX))
      _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_INVALID, NULL, 0, pxin);
    else {
      pxl = strlen(pxin);
      fullpx = _GD_Malloc(D, P->pxl + pxl + 1);
      if (fullpx) {
        if (P->px)
          memcpy(fullpx, P->px, P->pxl);
        px = fullpx + P->pxl;
        memcpy(px, pxin, pxl + 1); /* includes the terminating NUL */
      }
    }
  }

  if (!D->error && sxin) {
    /* Although we make the buffer big enough to hold the full suffix, we only
     * copy the local part for now so we can use it in the _GD_UpdateCode calls.
     * We'll add P->sx later
     *
     * Also note: no fullsx needed here because the local part is at the start
     */
    if (sxin[0] && _GD_ValidateField(sxin, 0, D->standards, 1, GD_VF_AFFIX))
      _GD_SetError(D, GD_E_BAD_CODE, GD_E_CODE_INVALID, NULL, 0, sxin);
    else {
      sx = _GD_Malloc(D, P->sxl + sxl + 1);
      if (sx)
        memcpy(sx, sxin, sxl + 1); /* includes the terminating NUL */
    }
  }

  /* vaildation failed or alloc errors */
  if (D->error) {
    if (nsalloc) /* delete nsin on error, if necessary */
      free((char*)nsin);
    free(fullns);
    free(fullpx);
    free(sx);
    GD_RETURN_ERROR(D);
  }

  /* find all affected fragments */
  ni = _GD_SubFragmentList(D, index, &ilist);
  if (ni == 0) { /* malloc error */
    free(fullns);
    free(fullpx);
    free(sx);
    GD_RETURN_ERROR(D);
  }

  /* array of new field codes */
  codes = _GD_Malloc(D, sizeof(*codes) * D->n_entries);
  if (codes == NULL) {
    free(ilist);
    free(fullns);
    free(fullpx);
    free(sx);
    GD_RETURN_ERROR(D);
  }

  /* find all affected entries and generate new field codes */
  for (u = 0; u < D->n_entries; ++u) {
    /* check fragment list */
    if (!_GD_ContainsFragment(ilist, ni, D->entry[u]->fragment_index)) {
      codes[u] = NULL; /* skip this one */
      continue; /* and try the next one */
    }

    /* Generate the new code and check it */
    codes[u] = _GD_UpdateCode(D, index, D->entry[u]->field,
        D->entry[u]->flags & GD_EN_EARLY, ns, nsl, px, pxl, sx, sxl);
    if (codes[u] == NULL)
      break;
  }
  free(ilist);

  /* Change attempt failed */
  if (D->error) {
    do {
      free(codes[u]);
    } while (u--);
    free(codes);
    free(fullns);
    free(fullpx);
    free(sx);
    GD_RETURN_ERROR(D);
  }

  /* update the codes */
  for (u = 0; u < D->n_entries; ++u)
    if (codes[u]) {
      free(D->entry[u]->field);
      D->entry[u]->field = codes[u];
      resort = 1;
    }
  free(codes);

  /* Resort */
  if (resort)
    qsort(D->entry, D->n_entries, sizeof(gd_entry_t*), _GD_EntryCmp);

  /* Kill the trailing '.', if it's present */
  if (nsl)
    ns[--nsl] = 0;

  /* Finish up the suffix */
  if (P->sx)
    memcpy(sx + sxl, P->sx, P->sxl + 1); /* including the trailing NUL */

  /* update the fragment itself, at the end */
  if (nsin) {
    free(F->ns);
    F->ns = fullns;
    F->nsl = nsl;
  }

  if (pxin) {
    free(F->px);
    F->px = fullpx;
    F->pxl = pxl;
  }

  if (sxin) {
    free(F->sx);
    F->sx = sx;
    F->sxl = sxl;
  }
  F->modified = 1;

  GD_RETURN_ERROR(D);
}

int gd_alter_affixes(DIRFILE *D, int index, const char *prefix,
    const char *suffix) gd_nothrow
{
  const char *px;
  char *ns = NULL;
  int ret, nsalloc = 1;
  dtrace("%p, %i, \"%s\", \"%s\"", D, index, prefix, suffix);

  GD_RETURN_ERR_IF_INVALID(D);

  if (index <= 0 || index >= D->n_fragment) 
    GD_SET_RETURN_ERROR(D, GD_E_BAD_INDEX, 0, NULL, index, NULL);

  /* split off the namespace, if present */
  if (D->standards >= 10 && prefix) {
    px = strrchr(prefix, '.');
    if (px) {
      px++; /* Advance the prefix pointer past the '.' */

      /* the ns we create includes the trailing '.', except when it's empty */
      if (px == prefix + 1) {
        ns = "";
        nsalloc = 0;
      } else {
        ns = _GD_Malloc(D, px - prefix + 1);
        if (ns == NULL)
          GD_RETURN_ERROR(D);
        memcpy(ns, prefix, px - prefix);
        ns[px - prefix] = 0;
      }
    } else
      px = prefix;
  } else
    px = prefix;

  /* UpdateAffixes will free ns */
  ret = _GD_UpdateAffixes(D, index, ns, nsalloc, px, suffix);

  dreturn("%i", ret);
  return ret;
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

const char *gd_fragment_namespace(DIRFILE *D, int index, const char *ns)
{
  dtrace("%p, %i, \"%s\"", D, index, ns);

  _GD_ClearError(D);

  GD_RETURN_IF_INVALID(D, "%p", NULL);

  /* Modification of the root format file's root namespace is not permitted */
  if (index < 0 || index >= D->n_fragment || (ns && index == 0)) {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, index, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  if (ns && _GD_UpdateAffixes(D, index, ns, 0, NULL, NULL)) {
    dreturn("%p", NULL);
    return NULL;
  }

  dreturn("\"%s\"", D->fragment[index].ns ? D->fragment[index].ns : "");
  return  D->fragment[index].ns ? (const char*)D->fragment[index].ns : "";
}
