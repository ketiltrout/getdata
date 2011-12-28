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

/* _GD_FreeD: free the DIRFILE and its subordinates
*/
static void _GD_FreeD(DIRFILE* D)
{
  unsigned int i;
  int j;

  dtrace("%p", D);

  for (i = 0; i < D->n_entries; ++i)
    _GD_FreeE(D, D->entry[i], 1);

  for (j = 0; j < D->n_fragment; ++j) {
    free(D->fragment[j].enc_data);
    free(D->fragment[j].prefix);
    free(D->fragment[j].suffix);
    free(D->fragment[j].bname);
    free(D->fragment[j].cname);
    free(D->fragment[j].ename);
    free(D->fragment[j].ref_name);
  }

  free(D->entry);
  free(D->dot_list);
  free(D->error_string);
  free(D->error_file);
  free(D->field_list);
  free(D->vector_list);
  for (j = 0; j < GD_N_ENTYPES; ++j)
    free(D->type_list[j]);
  free(D->string_value_list);
  free(D->const_value_list);
  if (D->carray_value_list)
    for (i = 0; D->carray_value_list[i].n != 0; ++i)
      free(D->carray_value_list[i].d);
  free(D->carray_value_list);
  free(D->fragment);
  free(D->name);
  for (i = 0; i < D->ndir; ++i)
    free(D->dir[i].path);
  free(D->dir);
  free(D);

  dreturnvoid();
}

static int _GD_ShutdownDirfile(DIRFILE* D, int flush_meta)
{
  unsigned int i;

  dtrace("%p, %i", D, flush_meta);

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
      _GD_Flush(D, D->entry[i], 1);

  if (D->error) {
    dreturn("%i", 1);
    return -1;
  }

#ifndef GD_NO_DIR_OPEN
  /* close the directory */
  for (i = 0; i < (unsigned int)D->ndir; ++i)
    close(D->dir[i].fd);
#endif

  _GD_FreeD(D);

  dreturn("%i", 0);
  return 0;
}

int gd_close(DIRFILE *D)
{
  int ret;

  dtrace("%p", D);

  ret = _GD_ShutdownDirfile(D, 1);

  dreturn("%i", ret);
  return ret;
}

int gd_discard(DIRFILE* D)
{
  int ret;

  dtrace("%p", D);

  ret = _GD_ShutdownDirfile(D, 0);

  dreturn("%i", ret);
  return ret;
}
/* vim: ts=2 sw=2 et tw=80
*/
