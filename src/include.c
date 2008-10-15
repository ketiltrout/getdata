/* (C) 2008 D. V. Wiebe
 *
 ***************************************************************************
 *
 * This file is part of the GetData project.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * GetData is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with GetData; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
#include "internal.h"

#ifdef STDC_HEADERS
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#endif

#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#endif

/* Include a format file fragment -- returns the include index, or 
 * -1 on error */
int _GD_Include(DIRFILE* D, const char* ename, const char* format_file,
    int linenum, int me, int encoding, int* standards, int flags)
{
  int i;
  int found = 0;
  char temp_buf1[FILENAME_MAX];
  char temp_buf2[FILENAME_MAX];
  FILE* new_fp = NULL;

  dtrace("%p, \"%s\", \"%s\", %i, %i, %x, %p, %x\n", D, ename, format_file,
      linenum, me, encoding, standards, flags);

  /* create the format filename */
  snprintf(temp_buf1, FILENAME_MAX, "%s/%s/%s", D->name,
      D->include_list[me].sname, ename);

  /* Run through the include list to see if we've already included this
   * file */
  for (i = 0; i < D->n_include; ++i)
    if (strcmp(temp_buf1, D->include_list[i].cname) == 0) {
      found = 1;
      break;
    }

  /* If we found the file, we won't reopen it.  Continue parsing. */
  if (found) {
    dreturn("%i", i);
    return i;
  }

  /* Otherwise, try to open the file */
  if ((D->flags & GD_ACCMODE) == GD_RDWR) {
    i = open(temp_buf1, O_RDWR | ((flags & GD_CREAT) ? O_CREAT : 0) |
        ((flags & GD_TRUNC) ? O_TRUNC : 0) | ((flags & GD_EXCL) ? O_EXCL : 0),
        0666);
    if (i < 0) {
      _GD_SetError(D, GD_E_OPEN_INCLUDE, errno, format_file, linenum,
          temp_buf1);
      dreturn("%i", -1);
      return -1;
    }
    new_fp = fdopen(i, "r+");
  } else {
    if (flags & (GD_CREAT | GD_TRUNC)) {
      _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
      dreturn("%i", -1);
      return -1;
    } else
      new_fp = fopen(temp_buf1, "r");
  }

  /* If opening the file failed, set the error code and abort parsing. */
  if (new_fp == NULL) {
    _GD_SetError(D, GD_E_OPEN_INCLUDE, errno, format_file, linenum, temp_buf1);
    dreturn("%i", -1);
    return -1;
  }

  /* If we got here, we managed to open the included file; parse it */
  D->include_list = realloc(D->include_list, (++D->n_include) *
      sizeof(struct gd_include_t));
  if (D->include_list == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  D->include_list[D->n_include - 1].cname = strdup(temp_buf1);
  D->include_list[D->n_include - 1].ename = strdup(ename);
  D->include_list[D->n_include - 1].modified = 0;
  D->include_list[D->n_include - 1].parent = me;
  D->include_list[D->n_include - 1].first = 0;
  D->include_list[D->n_include - 1].encoding = encoding;

  /* extract the subdirectory name - dirname both returns a volatile string
   * and modifies its argument, ergo strcpy */
  strncpy(temp_buf1, ename, FILENAME_MAX);
  if (strcmp(D->include_list[me].sname, ".") == 0)
    strcpy(temp_buf2, dirname(temp_buf1));
  else
    snprintf(temp_buf2, FILENAME_MAX, "%s/%s", D->include_list[me].sname,
        dirname(temp_buf1));

  D->include_list[D->n_include - 1].sname = strdup(temp_buf2);

  if (D->include_list[D->n_include - 1].cname == NULL ||
      D->include_list[D->n_include - 1].sname == NULL ||
      D->include_list[D->n_include - 1].ename == NULL)
  {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  if (_GD_ParseFormatFile(new_fp, D, D->n_include - 1, standards))
    D->include_list[me].first = 1;

  fclose(new_fp);

  dreturn("%i", D->n_include - 1);
  return D->n_include - 1;
}

int dirfile_include(DIRFILE* D, const char* file, int format_file,
    unsigned int flags)
{
  int standards = DIRFILE_STANDARDS_VERSION;

  dtrace("%p, \"%s\"", D, file);

  if (D->flags & GD_INVALID) {/* don't crash */
    _GD_SetError(D, GD_E_BAD_DIRFILE, 0, NULL, 0, NULL);
    dreturn("%zi", -1);
    return -1;
  }

 if (format_file < 0 || format_file >= D->n_include) {
    _GD_SetError(D, GD_E_BAD_INDEX, 0, NULL, 0, NULL);
    dreturn("%i", -1);
    return -1;
 }

  int i = _GD_Include(D, file, "dirfile_include()", 0, format_file,
      flags & GD_ENCODING, &standards, flags);

  dreturn("%i", i);
  return i;
}
