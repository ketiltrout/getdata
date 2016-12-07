/* Copyright (C) 2008-2016 D. V. Wiebe
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

/* Create new affixes given the current affixes and the new parts indicated
 * on a /INCLUDE line.  If there's a new namespace, clear the parser's current
 * namespace.  The caller is responsible for zeroing all the output parameters
 * before the call.
 *
 * Returns 1 on error.  In this case, the caller is responsible for free'ing
 * all the returned buffers.
 *
 * If a new rootspace is created, *newns is set to 1 (to let the caller know it
 * needs to zero p->ns). */
static int _GD_SetFieldAffixes(DIRFILE *D, const struct parser_state *p, int me,
    const char *pxin, const char *sxin, char **ns, size_t *nsl, char **px,
    size_t *pxl, char **sx, size_t *sxl, int *newns)
{
  dtrace("%p, %p, %i, \"%s\", \"%s\", %p, %p, %p, %p, %p, %p, %p", D, p, me,
      pxin, sxin, ns, nsl, px, pxl, sx, sxl, newns);

  /* suffix first, for some reason */
  if (sxin && sxin[0] != '\0') {
    if (_GD_ValidateField(sxin, 0, p->standards, p->pedantic, GD_VF_AFFIX)) {
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, p->file, p->line,
          sxin);
      dreturn("%i", 1);
      return 1;
    }

    if (D->fragment[me].sx == NULL) {
      *sxl = strlen(sxin);
      *sx = _GD_Malloc(D, *sxl + 1);
      if (*sx)
        memcpy(*sx, sxin, *sxl + 1); /* includes terminating NUL */
    } else {
      size_t sxinl = strlen(sxin);
      *sxl = D->fragment[me].sxl + sxinl + 1;
      *sx = _GD_Malloc(D, *sxl);
      if (*sx) {
        memcpy(*sx, sxin, sxinl);
        memcpy(*sx + sxinl, D->fragment[me].sx, D->fragment[me].sxl + 1);
      }
    }
  } else if (D->fragment[me].sx) {
    *sxl = D->fragment[me].sxl;
    *sx = _GD_Malloc(D, *sxl + 1);
    if (*sx)
      memcpy(*sx, D->fragment[me].sx, *sxl + 1); /* includes terminating NUL */
  }

  if (D->error) {
    dreturn("%i", 1);
    return 1;
  }

  /* now the namespace */
  if (GD_PVERS_GE(*p, 10)) {
    const char *nsin = NULL;
    size_t nsinl;

    if (pxin && pxin[0] != '\0') {
      /* try to find a namespace */
      for (nsinl = strlen(pxin); nsinl > 0; --nsinl)
        if (pxin[nsinl - 1] == '.')
          break;

      if (nsinl > 0) { /* found a namespace */
        /* ignore a leading dot: ns is always relative to root */
        if (pxin[0] == '.') {
          pxin++;
          nsinl--;
        }
        nsin = pxin;
        pxin += nsinl;
        if (nsinl)
          nsinl--; /* drop the '.' we found */
      }
    }

    if (nsin == NULL && p->ns) {
      /* If there's a current namespace but no namespace specified in the
       * /INCLUDE directive, then the current namespace becomes the new
       * rootspace */
      nsin = p->ns;
      nsinl = p->nsl;
    }

    if (nsin) {
      /* Signal caller to push and reset the parer's current namespace */
      *newns = 1;

      if (_GD_ValidateField(nsin, 0, p->standards, p->pedantic, GD_VF_NS)) {
        _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, p->file, p->line,
            *ns);
        dreturn("%i", 1);
        return 1;
      }

      if (D->fragment[me].ns == NULL) {
        *nsl = nsinl;
        *ns = _GD_Malloc(D, nsinl + 1);
        if (*ns) {
          memcpy(*ns, nsin, nsinl);
          (*ns)[nsinl] = 0; /* nsin isn't necessarily terminated */
        }
      } else {
        *nsl = D->fragment[me].nsl + nsinl;
        if (nsinl)
          (*nsl)++; /* for the intervening '.' */

        *ns = _GD_Malloc(D, *nsl + 1);
        if (*ns) {
          memcpy(*ns, D->fragment[me].ns, D->fragment[me].nsl);
          if (nsinl) {
            (*ns)[D->fragment[me].nsl] = '.';
            memcpy(*ns + D->fragment[me].nsl + 1, nsin, nsinl);
          }
          (*ns)[*nsl] = 0;
        }
      }
    } else if (D->fragment[me].ns) {
      *nsl = D->fragment[me].nsl;
      *ns = _GD_Malloc(D, *nsl + 1);
      if (*ns)
        memcpy(*ns, D->fragment[me].ns, *nsl + 1);
    }
  }

  /* Now the prefix */
  if (pxin && pxin[0] != '\0') {
    if (_GD_ValidateField(pxin, 0, p->standards, p->pedantic, GD_VF_AFFIX)) {
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, p->file, p->line,
          pxin);
      dreturn("%i", 1);
      return 1;
    }

    if (D->fragment[me].px == NULL) {
      *pxl = strlen(pxin);
      *px = _GD_Malloc(D, *pxl + 1);
      if (*px)
        memcpy(*px, pxin, *pxl + 1); /* includes terminating NUL */
    } else {
      size_t pxinl = strlen(pxin);
      *pxl = D->fragment[me].pxl + pxinl + 1;
      *px = _GD_Malloc(D, *pxl);
      if (*px) {
        memcpy(*px, D->fragment[me].px, D->fragment[me].pxl);
        memcpy(*px + D->fragment[me].pxl, pxin, pxinl + 1); /* incl. NUL */
      }
    }
  } else if (D->fragment[me].px) {
    *pxl = D->fragment[me].pxl;
    *px = _GD_Malloc(D, *pxl + 1);
    if (*px)
      memcpy(*px, D->fragment[me].px, *pxl + 1); /* includes terminating NUL */
  }

  if (D->error) {
    dreturn("%i", 1);
    return 1;
  }

  dreturn("%i (\"%s\"/%" PRIuSIZE ", \"%s\"/%" PRIuSIZE ", \"%s\"/%" PRIuSIZE 
      ", %i)", 0, *ns, *nsl, *px, *pxl, *sx, *sxl, *newns);
  return 0;
}

/* Include a format file fragment -- returns the new fragment index, or
 * -1 on error */
int _GD_Include(DIRFILE *D, struct parser_state *p, const char *ename,
    char **ref_name, int parent, const char *pxin, const char *sxin,
    int immediate)
{
  int i, newns = 0;
  int me = D->n_fragment;
  struct parser_state oldp = *p;
  int dirfd = -1, pop_ns = 0;
  char *temp_buf1 = NULL, *temp_buf2, *sname = NULL;
  char *base = NULL, *px = NULL, *sx = NULL, *ns = NULL;
  size_t pxl = 0, sxl = 0, nsl = 0;
  void *ptr = NULL;
  FILE* new_fp = NULL;
  time_t mtime = 0;
  struct stat statbuf;

  dtrace("%p, %p, \"%s\", %p, %i, \"%s\", \"%s\", %i", D, p, ename, ref_name,
      parent, pxin, sxin, immediate);

  if (++D->recurse_level >= GD_MAX_RECURSE_LEVEL) {
    _GD_SetError(D, GD_E_RECURSE_LEVEL, GD_E_RECURSE_INCLUDE, p->file, p->line,
        ename);
    goto include_error;
  }

  if (_GD_SetFieldAffixes(D, p, parent, pxin, sxin, &ns, &nsl, &px, &pxl, &sx,
        &sxl, &newns))
  {
    goto include_error;
  }

  /* when the rootspace changes, the current namespace gets reset to "", since
   * it's relative to rootspace.  But, remember old current namespace so we can
   * pop at the end
   */
  if (newns) {
    pop_ns = 1;
    p->ns = NULL; /* Don't free: we'll need it back when we pop */
    p->nsl = 0;
  }

  /* isolate filename */
  temp_buf2 = _GD_Strdup(D, ename);
  if (temp_buf2 == NULL)
    goto include_error;
  base = _GD_Strdup(D, basename(temp_buf2));
  free(temp_buf2);
  if (base == NULL)
    goto include_error;

  /* isolate relative path */
  temp_buf2 = _GD_Strdup(D, ename);
  if (temp_buf2 == NULL)
    goto include_error;
  sname = _GD_Strdup(D, dirname(temp_buf2));
  free(temp_buf2);
  if (sname == NULL)
    goto include_error;

  /* canonicalise */
  temp_buf1 = _GD_MakeFullPath(D, D->fragment[parent].dirfd, ename, 1);
  if (temp_buf1 == NULL)
    goto include_error;

  /* Open the containing directory */
  dirfd = _GD_GrabDir(D, D->fragment[parent].dirfd, temp_buf1, 1);
  if (dirfd == -1 && D->error == GD_E_OK)
    _GD_SetError(D, GD_E_IO, GD_E_IO_INCL, p->file, p->line, ename);
  if (D->error)
    goto include_error;

  /* Reject weird stuff */
  if (gd_StatAt(D, dirfd, base, &statbuf, 0)) {
    if (!(p->flags & GD_CREAT)) {
      _GD_SetError(D, GD_E_IO, GD_E_IO_INCL, p->file, p->line, temp_buf1);
      _GD_ReleaseDir(D, dirfd);
      goto include_error;
    }
  } else {
    if (S_ISDIR(statbuf.st_mode)) {
      _GD_SetError2(D, GD_E_IO, GD_E_IO_INCL, p->file, p->line, temp_buf1,
          EISDIR);
      _GD_ReleaseDir(D, dirfd);
      goto include_error;
    } else if (!S_ISREG(statbuf.st_mode)) {
      _GD_SetError2(D, GD_E_IO, GD_E_IO_INCL, p->file, p->line, temp_buf1,
          EINVAL);
      _GD_ReleaseDir(D, dirfd);
      goto include_error;
    }
  }

  /* Try to open the file */
  i = gd_OpenAt(D, dirfd, base,
      ((p->flags & (GD_CREAT | GD_TRUNC)) ? O_RDWR : O_RDONLY) |
      ((p->flags & GD_CREAT) ? O_CREAT : 0) |
      ((p->flags & GD_TRUNC) ? O_TRUNC : 0) |
      ((p->flags & GD_EXCL) ? O_EXCL : 0) | O_BINARY, 0666);

  if (i < 0) {
    _GD_SetError(D, GD_E_IO, GD_E_IO_INCL, p->file, p->line, temp_buf1);
    _GD_ReleaseDir(D, dirfd);
    goto include_error;
  }

  new_fp = fdopen(i, (p->flags & (GD_CREAT | GD_TRUNC)) ? "rb+" : "rb");

  /* If opening the file failed, set the error code and abort parsing. */
  if (new_fp == NULL) {
    _GD_SetError(D, GD_E_IO, GD_E_IO_INCL, p->file, p->line, temp_buf1);
    _GD_ReleaseDir(D, dirfd);
    goto include_error;
  }

  /* fstat the file and record the mtime */
  if (fstat(i, &statbuf) == 0)
    mtime = statbuf.st_mtime;

  /* If we got here, we managed to open the included file; parse it */
  ptr = _GD_Realloc(D, D->fragment, (++D->n_fragment) * sizeof(D->fragment[0]));
  if (ptr == NULL) {
    fclose(new_fp);
    _GD_ReleaseDir(D, dirfd);
    D->n_fragment--;
    goto include_error;
  }
  D->fragment = ptr;

  D->fragment[me].bname = base;
  D->fragment[me].cname = temp_buf1;
  D->fragment[me].ename = _GD_Strdup(D, ename);
  D->fragment[me].enc_data = NULL;
  D->fragment[me].modified = 0;
  D->fragment[me].parent = parent;
  D->fragment[me].dirfd = dirfd;
  D->fragment[me].encoding = p->flags & GD_ENCODING;
  D->fragment[me].byte_sex =
#ifdef WORDS_BIGENDIAN
    (p->flags & GD_LITTLE_ENDIAN) ? GD_LITTLE_ENDIAN : GD_BIG_ENDIAN
#else
    (p->flags & GD_BIG_ENDIAN) ? GD_BIG_ENDIAN : GD_LITTLE_ENDIAN
#endif
    ;
  D->fragment[me].ref_name = NULL;
  D->fragment[me].frame_offset = D->fragment[parent].frame_offset;
  D->fragment[me].protection = GD_PROTECT_NONE;
  D->fragment[me].px = px;
  D->fragment[me].pxl = px ? strlen(px) : 0;
  D->fragment[me].sx = sx;
  D->fragment[me].sxl = sx ? strlen(sx) : 0;
  D->fragment[me].ns = ns;
  D->fragment[me].nsl = nsl;
  D->fragment[me].mtime = mtime;
  D->fragment[me].vers = (p->pedantic) ? 1ULL << p->standards : 0;

  /* compute the (relative) subdirectory name */
  if (sname[0] == '.' && sname[1] == '\0') {
    /* dirname is the same as the parent fragment's */
    D->fragment[me].sname = (D->fragment[parent].sname) ?
      _GD_Strdup(D, D->fragment[parent].sname) : NULL;
    free(sname);
  } else if (D->fragment[parent].sname && _GD_AbsPath(sname)) {
    /* have both a relative dirname and the parent's sname; squish them
     * together */
    D->fragment[me].sname = _GD_Malloc(D, strlen(sname) +
        strlen(D->fragment[parent].sname) + 2);
    if (D->fragment[me].sname)
      sprintf(D->fragment[me].sname, "%s%c%s", D->fragment[parent].sname,
          GD_DIRSEP, sname);
    free(sname);
  } else
    /* just save the sname */
    D->fragment[me].sname = sname;

  /* catch alloc errors */
  if (D->error) {
    D->n_fragment--;
    temp_buf1 = px = sx = base = NULL;
    fclose(new_fp);
    _GD_ReleaseDir(D, dirfd);
    goto include_error;
  }

  *ref_name = _GD_ParseFragment(new_fp, D, p, me, immediate);

  fclose(new_fp);

  /* prevent /VERSION leak in DSV >= 9 */
  if ((oldp.standards >= 9 && oldp.pedantic) || p->standards >= 9) {
    if (p->standards != oldp.standards) {
      p->standards = oldp.standards;
      D->flags |= GD_MULTISTANDARD;
    }
    if (!oldp.pedantic)
      p->pedantic = 0;
  }
  
  /* pop file location */
  p->line = oldp.line;
  p->file = oldp.file;

  /* pop namespace, if necessary */
  if (pop_ns) {
    free(p->ns);
    p->ns = oldp.ns;
    p->nsl = oldp.nsl;
  }

  D->recurse_level--;
  dreturn("%i", me);
  return me;

include_error:
  free(ns);
  free(px);
  free(sx);
  free(base);
  free(sname);
  free(temp_buf1);
  D->recurse_level--;
  dreturn("%i", -1);
  return -1;
}

static int _GD_IncludeAffix(DIRFILE* D, const char *funcname, const char* file,
    int fragment_index, const char *px, const char *sx, unsigned long flags)
{
  char* ref_name = NULL;
  int i, new_fragment;
  /* Remember the last fragment, in case we have to undo everything */
  int old_nfrag = D->n_fragment;

  struct parser_state p = {
    funcname, 0, GD_DIRFILE_STANDARDS_VERSION, flags & GD_PEDANTIC,
    NULL, 0, flags
  };

  dtrace("%p, \"%s\", \"%s\", %i, \"%s\", \"%s\", 0x%lX", D, funcname, file,
      fragment_index, px, sx, flags); 

  /* Early checks */
  GD_RETURN_ERR_IF_INVALID(D);

  if ((D->flags & GD_ACCMODE) == GD_RDONLY) 
    GD_SET_RETURN_ERROR(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
  else if (fragment_index < 0 || fragment_index >= D->n_fragment) 
    GD_SET_RETURN_ERROR(D, GD_E_BAD_INDEX, 0, NULL, fragment_index, NULL);
  else if (D->fragment[fragment_index].protection & GD_PROTECT_FORMAT)
    GD_SET_RETURN_ERROR(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[fragment_index].cname);

  if (~D->flags & GD_HAVE_VERSION)
    _GD_FindVersion(D);

  /* only set if the dirfile conforms to some standard */
  if (D->av)
    p.standards = D->standards;

  /* if the caller specified no encoding scheme, but we were asked to create
   * the fragment, inherit it from the parent */
  if ((p.flags & (GD_ENCODING | GD_CREAT)) == GD_CREAT)
    p.flags |= D->fragment[fragment_index].encoding;

  /* Perform the include */
  new_fragment = _GD_Include(D, &p, file, &ref_name, fragment_index, px, sx, 1);

  /* Honour the reference directive, if not prohibited by the caller */
  if (!D->error && ref_name != NULL && ~flags & GD_IGNORE_REFS) {
    gd_entry_t *E = _GD_FindField(D, ref_name, strlen(ref_name), D->entry,
        D->n_entries, 1, NULL);

    /* FIXME: These errors are problematic, since they'll cause the call to
     * fail, even though the new fragment has been integrated into the DIRFILE.
     */

    if (E == NULL)
      _GD_SetError(D, GD_E_BAD_REFERENCE, GD_E_REFERENCE_CODE, NULL, 0,
          ref_name);
    else if (E->field_type != GD_RAW_ENTRY)
      _GD_SetError(D, GD_E_BAD_REFERENCE, GD_E_REFERENCE_TYPE, NULL, 0,
          ref_name);
    else
      D->reference_field = E;
  }
  free(ref_name);

  /* If there was an error, we now have to:
   *
   * - delete all the entries that were added
   * - delete all the fragemnts that were added
   *
   * This is the same thing that gd_uninclude() does, but it's easier here
   * because we know none of the fields we delete are open, have been reference
   * by the old fields, and all the fragment numbers are contiguous. */
  if (D->error) {
    unsigned int i, first = 0;
    unsigned int next = 0;
    int found_good = 0;

    /* First delete all the new fields */
    for (i = 0; i < D->n_entries; ++i) {
      if (D->entry[i]->fragment_index >= old_nfrag) {
        /* found an entry to delete */
        _GD_FreeE(D, D->entry[i], 1);

        if (found_good) { /* The last entry was good */
          /* There is a run of entries spanning [first,i-1] that need to be
           * moved to [next,next+i-first] */
          memmove(D->entry + next, D->entry + first,
              sizeof(D->entry[0]) * (i - first));

          /* advance next to here */
          next += i - first;

          /* and change modes */
          found_good = 0;
        }
      } else {
        /* found a good entry */

        if (!found_good) { /* The last entry was bad */
          first = i; /* This is the start of the next good run */
          found_good = 1; /* Change modes */
        }
      }
    }

    /* Handle the last bunch of good entries, if necessary */
    if (found_good) {
      memmove(D->entry + next, D->entry + first,
          sizeof(D->entry[0]) * (i - first));
      next += i - first;
    }
    
    D->n_entries = next; /* Done.  Reset entry count. */

    /* Now delete all the new fragments.  This is easier because we don't
     * have to search for them.
     */
    _GD_FreeF(D, old_nfrag, D->n_fragment);
    D->n_fragment = old_nfrag;

    GD_RETURN_ERROR(D);
  }

  /* Successful include.  Mark the parent as dirty */
  D->fragment[fragment_index].modified = 1;
  D->flags &= ~GD_HAVE_VERSION;

  /* If ref_name is non-NULL, the included fragment contained a REFERENCE
   * directive.  If ref_name is NULL but D->fragment[new_fragment].ref_name is
   * non-NULL, no REFERENCE directive was present, but the parser found a RAW
   * field to serve as a reference field.
   */
  if (new_fragment >= 0 && D->fragment[new_fragment].ref_name != NULL)
    /* If the parser chose a reference field, propagate it upward if requried */
    for (i = fragment_index; i != -1; i = D->fragment[i].parent) {
      if (D->fragment[i].ref_name == NULL) {
        D->fragment[i].ref_name = strdup(D->fragment[new_fragment].ref_name);
        D->fragment[i].modified = 1;
      } else
        break;
    }

  dreturn("%i", new_fragment);
  return new_fragment;
}

int gd_include_affix(DIRFILE* D, const char* file, int fragment_index,
    const char *prefix, const char *suffix, unsigned long flags)
{
  int new_fragment;

  dtrace("%p, \"%s\", %i, \"%s\", \"%s\", 0x%lX", D, file, fragment_index,
      prefix, suffix, flags);

  new_fragment = _GD_IncludeAffix(D, "gd_include_affix()", file, fragment_index,
      prefix, suffix, flags);

  dreturn("%i", new_fragment);
  return new_fragment;
}

int gd_include(DIRFILE* D, const char* file, int fragment_index,
    unsigned long flags)
{
  int new_fragment;

  dtrace("%p, \"%s\", %i, 0x%lX", D, file, fragment_index, flags);

  new_fragment = _GD_IncludeAffix(D, "gd_include()", file, fragment_index, NULL,
      NULL, flags);

  dreturn("%i", new_fragment);
  return new_fragment;
}

int gd_include_ns(DIRFILE *D, const char *file, int fragment_index,
    const char *nsin, unsigned long flags)
{
  int new_fragment;
  size_t len = 0;
  char *ns = NULL;

  dtrace("%p, \"%s\", %i, \"%s\", 0x%lX", D, file, fragment_index, nsin, flags);

  if (nsin) {
    ns = _GD_NormaliseNamespace(D, nsin, &len);
    if (ns == NULL)
      GD_RETURN_ERROR(D);
  } 

  new_fragment = _GD_IncludeAffix(D, "gd_include_ns()", file, fragment_index,
      ns, NULL, flags);

  if (ns)
    free(ns);

  dreturn("%i", new_fragment);
  return new_fragment;
}

int _GD_ContainsFragment(const int* f, int nf, int fragment)
{
  int i;

  dtrace("%p, %i, %i", f, nf, fragment);

  for (i = 0; i < nf; ++i)
    if (f[i] == fragment) {
      dreturn("%i", 1);
      return 1;
    }

  dreturn("%i", 0);
  return 0;
}

int gd_uninclude(DIRFILE* D, int fragment_index, int del)
{
  int parent, j, nf;
  unsigned int i, o, old_count;
  int *f = NULL;

  dtrace("%p, %i, %i", D, fragment_index, del);

  GD_RETURN_ERR_IF_INVALID(D);

  if ((D->flags & GD_ACCMODE) == GD_RDONLY) 
    GD_SET_RETURN_ERROR(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
  if (fragment_index <= 0 || fragment_index >= D->n_fragment)
    GD_SET_RETURN_ERROR(D, GD_E_BAD_INDEX, 0, NULL, fragment_index, NULL);

  parent = D->fragment[fragment_index].parent;

  if (D->fragment[parent].protection & GD_PROTECT_FORMAT)
    GD_SET_RETURN_ERROR(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[parent].cname);

  /* find all affected fragments */
  nf = _GD_SubFragmentList(D, fragment_index, &f);

  if (D->error) {
    free(f);
    GD_RETURN_ERROR(D);
  }

  /* close affected raw fields */
  for (i = 0; i < D->n_entries; ++i)
    if (D->entry[i]->field_type == GD_RAW_ENTRY &&
        _GD_ContainsFragment(f, nf, D->entry[i]->fragment_index))
    {
      _GD_Flush(D, D->entry[i], 0, 1);
    }

  /* flush the fragment's metadata, if requested */
  if (!del)
    for (j = 0; j < nf; ++j)
      _GD_FlushMeta(D, f[j], 0);

  if (D->error) {
    free(f);
    GD_RETURN_ERROR(D);
  }

  /* Nothing from now on may fail */

  /* delete the fragments, if requested */
  if (del)
    for (j = 0; j < nf; ++j)
      gd_UnlinkAt(D, D->fragment[f[j]].dirfd, D->fragment[f[j]].bname, 0);

  /* delete fields from the fragment -- memory use is not sufficient to warrant
   * resizing D->entry */
  old_count = D->n_entries;
  for (i = o = 0; i < old_count; ++i)
    if (_GD_ContainsFragment(f, nf, D->entry[i]->fragment_index)) {
      if (D->entry[i]->e->n_meta >= 0)
        D->n_entries--;

      _GD_FreeE(D, D->entry[i], 1);
    } else
      D->entry[o++] = D->entry[i];

  /* Flag the parent as modified */
  D->fragment[parent].modified = 1;
  D->flags &= ~GD_HAVE_VERSION;

  /* delete the fragments -- again, don't bother resizing D->fragment */
  for (j = 0; j < nf; ++j) {
    _GD_FreeF(D, f[j], f[j] + 1);

    memcpy(D->fragment + f[j], D->fragment + D->n_fragment - 1,
        sizeof(struct gd_fragment_t));
    D->n_fragment--;

    /* Relocate all fields of the fragment we just moved */
    for (i = 0; i < D->n_entries; ++i)
      if (D->entry[i]->fragment_index == D->n_fragment)
        D->entry[i]->fragment_index = f[j];
  }

  /* Clear the cache of all fields */
  for (i = 0; i < D->n_entries; ++i) {
    D->entry[i]->flags &= ~GD_EN_CALC;
    for (j = 0; j < GD_MAX_LINCOM; ++j)
      D->entry[i]->e->entry[j] = NULL;
    D->entry[i]->e->fl.value_list_validity = 0;
    D->entry[i]->e->fl.entry_list_validity = 0;
  }

  /* Invalidate the field lists */
  D->fl.value_list_validity = 0;
  D->fl.entry_list_validity = 0;

  free(f);

  dreturn("%i", 0);
  return 0;
}
