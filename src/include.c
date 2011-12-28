/* Copyright (C) 2008-2011 D. V. Wiebe
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
 * on a /INCLUDE line */
static int _GD_SetFieldAffixes(DIRFILE *D, int me, const char *prefix_in,
    const char *suffix_in, int standards, int pedantic, const char *format_file,
    int line, char **prefix, char **suffix)
{
  dtrace("%p, %i, \"%s\", \"%s\", %i, %i, \"%s\", %i, %p, %p", D, me, prefix_in,
      suffix_in, standards, pedantic, format_file, line, prefix, suffix);

  /* suffix first, for some reason */
  if (suffix_in && suffix_in[0] != '\0') {
    if (_GD_ValidateField(suffix_in, standards, pedantic, 1, NULL))
    {
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
          suffix_in);
      dreturn("%i", 1);
      return 1;
    }

    if (D->fragment[me].suffix == NULL)
      *suffix = _GD_Strdup(D, suffix_in);
    else {
      *suffix = _GD_Malloc(D, strlen(D->fragment[me].suffix) +
          strlen(suffix_in) + 1);
      if (*suffix)
        strcat(strcpy(*suffix, suffix_in), D->fragment[me].suffix);
    }
  } else if (D->fragment[me].suffix)
    *suffix = _GD_Strdup(D, D->fragment[me].suffix);

  if (D->error) {
    dreturn("%i", 1);
    return 1;
  }

  /* now the prefix */
  if (prefix_in && prefix_in[0] != '\0') {
    if (_GD_ValidateField(prefix_in, standards, pedantic, 1, NULL))
    {
      _GD_SetError(D, GD_E_FORMAT, GD_E_FORMAT_BAD_NAME, format_file, line,
          prefix_in);
      dreturn("%i", 1);
      return 1;
    }

    if (D->fragment[me].prefix == NULL)
      *prefix = _GD_Strdup(D, prefix_in);
    else {
      *prefix = _GD_Malloc(D, strlen(D->fragment[me].prefix) +
          strlen(prefix_in) + 1);
      if (*prefix)
        strcat(strcpy(*prefix, D->fragment[me].prefix), prefix_in);
    }
  } else if (D->fragment[me].prefix)
    *prefix = _GD_Strdup(D, D->fragment[me].prefix);

  dreturn("%i", D->error);
  return D->error;
}

/* Include a format file fragment -- returns the mew fragment index, or
 * -1 on error */
int _GD_Include(DIRFILE *D, const char *ename, const char *format_file,
    int linenum, char **ref_name, int me, const char *prefix_in,
    const char *suffix_in, int *standards, unsigned long *flags, int resolve)
{
  int i;
  int old_standards = *standards;
  int old_pedantic = *flags & GD_PEDANTIC;
  int dirfd = -1;
  char *temp_buf1, *temp_buf2;
  char *base, *prefix = NULL, *suffix = NULL;
  void *ptr = NULL;
  FILE* new_fp = NULL;

  dtrace("%p, \"%s\", \"%s\", %p, %i, %i, \"%s\", \"%s\", %p, %p, %i", D, ename,
      format_file, ref_name, linenum, me, prefix_in, suffix_in, standards,
      flags, resolve);

  if (++D->recurse_level >= GD_MAX_RECURSE_LEVEL) {
    _GD_SetError(D, GD_E_RECURSE_LEVEL, GD_E_RECURSE_INCLUDE, format_file,
        linenum, ename);
    D->recurse_level--;
    dreturn("%i", 0);
    return 0;
  }

  if (_GD_SetFieldAffixes(D, me, prefix_in, suffix_in, old_standards,
        old_pedantic, format_file, linenum, &prefix, &suffix))
  {
    free(suffix);
    D->recurse_level--;
    dreturn("%i", -1);
    return -1;
  }

  temp_buf2 = _GD_Strdup(D, ename);
  if (temp_buf2 == NULL) {
    free(prefix);
    free(suffix);
    D->recurse_level--;
    dreturn("%i", -1);
    return -1;
  }
  base = _GD_Strdup(D, basename(temp_buf2));
  if (base == NULL) {
    free(temp_buf2);
    free(prefix);
    free(suffix);
    D->recurse_level--;
    dreturn("%i", -1);
    return -1;
  }

  /* Open the containing directory */
  dirfd = _GD_GrabDir(D, D->fragment[me].dirfd, ename);
  if (dirfd == -1 && D->error == GD_E_OK)
    _GD_SetError(D, GD_E_OPEN_FRAGMENT, errno, format_file, linenum, ename);
  if (D->error) {
    free(temp_buf2);
    free(prefix);
    free(suffix);
    D->recurse_level--;
    dreturn("%i", -1);
    return -1;
  }

  temp_buf1 = _GD_MakeFullPath(D, dirfd, base, 1);
  free(temp_buf2);
  if (temp_buf1 == NULL) {
    _GD_ReleaseDir(D, dirfd);
    free(prefix);
    free(suffix);
    D->recurse_level--;
    dreturn("%i", -1);
    return -1;
  }

  /* Try to open the file */
  i = gd_OpenAt(D, dirfd, base, (((D->flags & GD_ACCMODE) == GD_RDWR) ? O_RDWR :
        O_RDONLY) | ((*flags & GD_CREAT) ? O_CREAT : 0) |
      ((*flags & GD_TRUNC) ? O_TRUNC : 0) | ((*flags & GD_EXCL) ? O_EXCL : 0)
      | O_BINARY, 0666);

  if (i < 0) {
    _GD_SetError(D, GD_E_OPEN_FRAGMENT, errno, format_file, linenum,
        temp_buf1);
    free(prefix);
    free(suffix);
    free(base);
    free(temp_buf1);
    D->recurse_level--;
    dreturn("%i", -1);
    return -1;
  }

  new_fp = fdopen(i, ((D->flags & GD_ACCMODE) == GD_RDWR) ? "r+" : "r");

  /* If opening the file failed, set the error code and abort parsing. */
  if (new_fp == NULL) {
    _GD_SetError(D, GD_E_OPEN_FRAGMENT, errno, format_file, linenum, temp_buf1);
    free(prefix);
    free(suffix);
    free(base);
    free(temp_buf1);
    D->recurse_level--;
    dreturn("%i", -1);
    return -1;
  }

  /* If we got here, we managed to open the included file; parse it */
  ptr = _GD_Realloc(D, D->fragment,
      (++D->n_fragment) * sizeof(struct gd_fragment_t));
  if (ptr == NULL) {
    D->n_fragment--;
    free(prefix);
    free(suffix);
    free(base);
    free(temp_buf1);
    D->recurse_level--;
    dreturn("%i", -1);
    return -1;
  }
  D->fragment = (struct gd_fragment_t *)ptr;

  D->fragment[D->n_fragment - 1].bname = base;
  D->fragment[D->n_fragment - 1].cname = temp_buf1;
  D->fragment[D->n_fragment - 1].ename = _GD_Strdup(D, ename);
  D->fragment[D->n_fragment - 1].enc_data = NULL;
  D->fragment[D->n_fragment - 1].modified = 0;
  D->fragment[D->n_fragment - 1].parent = me;
  D->fragment[D->n_fragment - 1].dirfd = dirfd;
  D->fragment[D->n_fragment - 1].encoding = *flags & GD_ENCODING;
  D->fragment[D->n_fragment - 1].byte_sex =
#ifdef WORDS_BIGENDIAN
    (*flags & GD_LITTLE_ENDIAN) ? GD_LITTLE_ENDIAN : GD_BIG_ENDIAN
#else
    (*flags & GD_BIG_ENDIAN) ? GD_BIG_ENDIAN : GD_LITTLE_ENDIAN
#endif
    ;
  D->fragment[D->n_fragment - 1].ref_name = NULL;
  D->fragment[D->n_fragment - 1].frame_offset = D->fragment[me].frame_offset;
  D->fragment[D->n_fragment - 1].protection = GD_PROTECT_NONE;
  D->fragment[D->n_fragment - 1].prefix = prefix;
  D->fragment[D->n_fragment - 1].suffix = suffix;
  D->fragment[D->n_fragment - 1].vers =
    (*flags & GD_PEDANTIC) ? 1ULL << *standards : 0;

  if (D->fragment[D->n_fragment - 1].ename == NULL) {
    D->n_fragment--;
    free(base);
    D->recurse_level--;
    dreturn("%i", -1);
    return -1;
  }

  /* extract the subdirectory name */
  D->fragment[D->n_fragment - 1].sname = _GD_DirName(D, dirfd);

  *ref_name = _GD_ParseFragment(new_fp, D, D->n_fragment - 1, standards, flags,
      resolve);

  fclose(new_fp);

  /* prevent /VERSION leak in DSV >= 9 */
  if ((old_standards >= 9 && old_pedantic) || *standards >= 9) {
    if (*standards != old_standards) {
      *standards = old_standards;
      D->flags |= GD_MULTISTANDARD;
    }
    if (!old_pedantic)
      *flags &= ~GD_PEDANTIC;
  }

  D->recurse_level--;
  dreturn("%i", D->n_fragment - 1);
  return D->n_fragment - 1;
}

int gd_include_affix(DIRFILE* D, const char* file, int fragment_index,
    const char *prefix, const char *suffix, unsigned long flags)
{
  int standards = GD_DIRFILE_STANDARDS_VERSION;
  char* ref_name = NULL;
  int i, new_fragment;

  dtrace("%p, \"%s\", %i, \"%s\", \"%s\", 0x%lX", D, file, fragment_index,
      prefix, suffix, flags);

  if (~D->flags & GD_HAVE_VERSION)
    _GD_FindVersion(D);

  /* only set if the dirfile conforms to some standard */
  if (D->av)
    standards = D->standards;

  _GD_ClearError(D);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }
  /* check access mode */
  if ((D->flags & GD_ACCMODE) == GD_RDONLY) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* check for include index out of range */
  if (fragment_index < 0 || fragment_index >= D->n_fragment) {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, fragment_index, NULL);
    dreturn("%i", -1);
    return -1;
  }

  /* check protection */
  if (D->fragment[fragment_index].protection & GD_PROTECT_FORMAT) {
    _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[fragment_index].cname);
    dreturn("%i", -1);
    return -1;
  }

  /* if the caller specified no encoding scheme, but we were asked to create
   * the fragment, inherit it from the parent */
  if ((flags & (GD_ENCODING | GD_CREAT)) == GD_CREAT)
    flags |= D->flags & GD_ENCODING;

  new_fragment = _GD_Include(D, file, "dirfile_include()", 0, &ref_name,
      fragment_index, prefix, suffix, &standards, &flags, 1);

  if (!D->error) {
    D->fragment[fragment_index].modified = 1;
    D->flags &= ~GD_HAVE_VERSION;
  }

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

  /* Honour the reference directive, if not prohibited by the caller */
  if (ref_name != NULL && ~flags & GD_IGNORE_REFS) {
    gd_entry_t *E = _GD_FindField(D, ref_name, D->entry, D->n_entries, 1, NULL);

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

  dreturn("%i", new_fragment);
  return new_fragment;
}

int gd_include(DIRFILE* D, const char* file, int fragment_index,
    unsigned long flags)
{
  int new_fragment;

  dtrace("%p, \"%s\", %i, 0x%lX", D, file, fragment_index, flags);

  new_fragment = gd_include_affix(D, file, fragment_index, NULL, NULL, flags);
  
  dreturn("%i", new_fragment);
  return new_fragment;
}

static int _GD_CollectFragments(DIRFILE* D, int** f, int fragment, int nf)
{
  int i;
  int *new_f;

  dtrace("%p, %p, %i, %i", D, f, fragment, nf);

  new_f = (int *)_GD_Realloc(D, *f, sizeof(int) * ++nf);
  if (new_f == NULL) {
    dreturn("%i", -1);
    return -1;
  }
  new_f[nf - 1] = fragment;

  for (i = 0; i < D->n_fragment; ++i)
    if (D->fragment[i].parent == fragment) {
      nf = _GD_CollectFragments(D, &new_f, i, nf);
      if (nf == -1)
        break;
    }

  *f = new_f;

  dreturn("%i", nf);
  return nf;
}

static int _GD_ContainsFragment(int* f, int nf, int fragment)
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

  _GD_ClearError(D);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if ((D->flags & GD_ACCMODE) == GD_RDONLY) {
    _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (fragment_index <= 0 || fragment_index >= D->n_fragment) {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, fragment_index, NULL);
    dreturn("%i", -1);
    return -1;
  }

  parent = D->fragment[fragment_index].parent;

  if (D->fragment[parent].protection & GD_PROTECT_FORMAT) {
    _GD_SetError(D, GD_E_PROTECTED, GD_E_PROTECTED_FORMAT, NULL, 0,
        D->fragment[parent].cname);
    dreturn("%i", -1);
    return -1;
  }

  /* find all affected fragments */
  nf = _GD_CollectFragments(D, &f, fragment_index, 0);

  if (D->error) {
    free(f);
    dreturn("%i", -1);
    return -1;
  }

  /* close affected raw fields */
  for (i = 0; i < D->n_entries; ++i)
    if (D->entry[i]->field_type == GD_RAW_ENTRY &&
        _GD_ContainsFragment(f, nf, D->entry[i]->fragment_index))
    {
      _GD_Flush(D, D->entry[i], 1);
    }

  /* flush the fragment's metadata, if requested */
  if (!del)
    for (j = 0; j < nf; ++j)
      _GD_FlushMeta(D, f[j], 0);

  if (D->error) {
    free(f);
    dreturn("%i", -1);
    return -1;
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
      if (D->entry[i]->e->n_meta >= 0) {
        D->n_entries--;
        D->n[_GD_EntryIndex(D->entry[i]->field_type)]--;
      } else
        D->n_meta--;

      _GD_FreeE(D, D->entry[i], 1);
    } else
      D->entry[o++] = D->entry[i];

  /* Flag the parent as modified */
  D->fragment[parent].modified = 1;
  D->flags &= ~GD_HAVE_VERSION;

  /* delete the fragments -- again, don't bother resizing D->fragment */
  for (j = 0; j < nf; ++j) {
    _GD_ReleaseDir(D, D->fragment[f[j]].dirfd);
    free(D->fragment[f[j]].cname);
    free(D->fragment[f[j]].ename);
    free(D->fragment[f[j]].bname);
    free(D->fragment[f[j]].ref_name);

    memcpy(D->fragment + f[j], D->fragment + D->n_fragment - 1,
        sizeof(struct gd_fragment_t));
    D->n_fragment--;

    /* Relocate all fields of the fragment we just moved */
    for (i = 0; i < D->n_entries; ++i)
      if (D->entry[i]->fragment_index == D->n_fragment)
        D->entry[i]->fragment_index = f[j];
  }

  /* Clear the cache of all fields */
  /* FIXME: Should probably just clear affected fields */
  for (i = 0; i < D->n_entries; ++i) {
    D->entry[i]->e->calculated = 0;
    for (j = 0; j < GD_MAX_LINCOM; ++j)
      D->entry[i]->e->entry[j] = NULL;
  }

  /* Invalidate the field lists */
  D->list_validity = 0;
  D->type_list_validity = 0;

  free(f);

  dreturn("%i", 0);
  return 0;
}
