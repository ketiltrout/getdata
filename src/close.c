/* Copyright (C) 2008-2012, 2014-2017 D. V. Wiebe
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

/* Delete the run of fragments [start,stop) from the fragment list */
void _GD_FreeF(DIRFILE *D, int start, int stop)
{
  int j;

  dtrace("%p, %i", D, start);

  for (j = start; j < stop; ++j) {
    _GD_ReleaseDir(D, D->fragment[j].dirfd);
    free(D->fragment[j].enc_data);
    free(D->fragment[j].ns);
    free(D->fragment[j].px);
    free(D->fragment[j].sx);
    free(D->fragment[j].bname);
    free(D->fragment[j].cname);
    free(D->fragment[j].ename);
    free(D->fragment[j].sname);
    free(D->fragment[j].ref_name);
  }

  dreturnvoid();
}

/* _GD_FreeD: free the DIRFILE and its subordinates
*/
static void _GD_FreeD(DIRFILE *D, int keep_dirfile)
{
  unsigned int i;

  dtrace("%p, %i", D, keep_dirfile);

  for (i = 0; i < D->n_entries; ++i)
    _GD_FreeE(D, D->entry[i], 1);

  free(D->entry);
  free(D->tok_base);
  free(D->error_prefix);
  free(D->error_string);
  free(D->error_file);

  free(D->regex_list);
  _GD_FreeFL(&D->fl);
  
  _GD_FreeF(D, 0, D->n_fragment);
  free(D->fragment);

  free(D->name);
  for (i = 0; i < D->ndir; ++i) {
    free(D->dir[i].path);
#ifndef GD_NO_DIR_OPEN
    close(D->dir[i].fd);
#endif
  }
  free(D->dir);

  if (!keep_dirfile)
    free(D);

  dreturnvoid();
}

int _GD_ShutdownDirfile(DIRFILE* D, int flush_meta, int keep_dirfile)
{
  unsigned int i;

  dtrace("%p, %i, %i", D, flush_meta, keep_dirfile);

  if (D == NULL) {
    dreturn("%i", 0);
    return 0;
  }

  _GD_ClearError(D);

  /* Flush */
  if (flush_meta)
    _GD_FlushMeta(D, GD_ALL_FRAGMENTS, 0);

  for(i = 0; i < D->n_entries; ++i)
    if (D->entry[i]->field_type == GD_RAW_ENTRY)
      _GD_Flush(D, D->entry[i], 0, 1);

  if (D->error)
    GD_RETURN_ERROR(D);

  _GD_FreeD(D, keep_dirfile);

  dreturn("%i", GD_E_OK);
  return GD_E_OK;
}

int gd_close(DIRFILE *D)
{
  int ret;

  dtrace("%p", D);

  ret = _GD_ShutdownDirfile(D, 1, 0);

  /* D->error may no longer be valid here */
  dreturn("%i", ret);
  return ret;
}

int gd_discard(DIRFILE* D)
{
  int ret;

  dtrace("%p", D);

  ret = _GD_ShutdownDirfile(D, 0, 0);

  /* D->error may no longer be valid here */
  dreturn("%i", ret);
  return ret;
}
/* vim: ts=2 sw=2 et tw=80
*/
