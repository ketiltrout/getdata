/* (C) 2002-2005 C. Barth Netterfield
 * (C) 2005-2008 D. V. Wiebe
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
#include <ctype.h>
#include <math.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#endif

#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif

#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#endif

/* attempt to open or create a new dirfile - set error appropriately */
static FILE* _GD_CreateDirfile(DIRFILE* D, const char* format_file,
    const char* filedir)
{
  struct stat statbuf;
  char fullname[FILENAME_MAX];
  DIR* dir;
  char* dirfile_end;
  struct dirent* lamb;
  int dir_error = 0;
  int format_error = 0;
  FILE* fp = NULL;

  dtrace("%p, \"%s\", \"%s\"", D, format_file, filedir);

  /* naively try to open the format file */
  if ((fp = fopen(format_file, "r")) == NULL) {
    format_error = errno;

    /* open failed, try to stat the directory itself */
    if (stat(filedir, &statbuf))
      dir_error = errno;
    else if (!S_ISDIR(statbuf.st_mode))
      dir_error = ENOTDIR;
  }

  /* First, cast out our four failure modes */

  /* unable to read the format file */
  if (format_error == EACCES || dir_error == EACCES) {
    _GD_SetError(D, GD_E_OPEN, GD_E_OPEN_NO_ACCESS, format_file, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  /* the directory exists, but it's not a dirfile, do nothing else -- even if we
   * were asked to truncate it */
  if (!dir_error && format_error) {
    _GD_SetError(D, GD_E_OPEN, GD_E_OPEN_NOT_DIRFILE, filedir, 0, NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  /* Couldn't open the file, and we weren't asked to create it */
  if (format_error && !(D->flags & GD_CREAT)) {
    _GD_SetError(D, GD_E_OPEN, GD_E_OPEN_NOT_EXIST, filedir, format_error,
        NULL);
    dreturn("%p", NULL);
    return NULL;
  }

  /* It does exist, but we were asked to exclusively create it */
  if (!format_error && (D->flags & GD_CREAT) && (D->flags & GD_EXCL)) {
    _GD_SetError(D, GD_E_CREAT, GD_E_CREAT_EXCL, filedir, 0, NULL);
    fclose(fp);
    dreturn("%p", NULL);
    return NULL;
  }

  /* If we made it here we either:
   * 1) have no such directory, but plan to create it, or
   * 2) have a dirfile, which means the directory supplied contains a readable
   *   file called format */

  /* Truncate, if needed -- dangerous!  Truncating a dirfile deletes every
   * regular file in the specified directory.  It does not touch subdirectories.
   * Note that the rather lame definition of a dirfile at this point
   * (specifically, we haven't bothered to see if the format file is parsable)
   * could be problematic if users use GD_TRUNC cavalierly. */
  if (D->flags & GD_TRUNC && !format_error) {
    /* This file isn't going to be around much longer */
    fclose(fp);

    /* can't truncate a read-only dirfile */
    if ((D->flags & GD_ACCMODE) == GD_RDONLY) {
      _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
      dreturn("%p", NULL);
      return NULL;
    }

    /* This code is from defile */
    if ((dir = opendir(filedir)) == NULL) {
      _GD_SetError(D, GD_E_TRUNC, GD_E_TRUNC_DIR, filedir, errno, NULL);
      dreturn("%p", NULL);
      return NULL;
    }

    strcpy(fullname, filedir);
    dirfile_end = fullname + strlen(fullname);
    if (*(dirfile_end - 1) != '/') {
      strcat(fullname, "/");
      dirfile_end++;
    }

    while ((lamb = readdir(dir)) != NULL) {
      strcpy(dirfile_end, lamb->d_name);

      if (stat(fullname, &statbuf)) {
        _GD_SetError(D, GD_E_TRUNC, GD_E_TRUNC_STAT, fullname, errno, NULL);
        dreturn("%p", NULL);
        return NULL;
      }

      /* only delete regular files */
      if (S_ISREG(statbuf.st_mode)) {
        if (unlink(fullname)) {
          _GD_SetError(D, GD_E_TRUNC, GD_E_TRUNC_UNLINK, fullname, errno, NULL);
          dreturn("%p", NULL);
          return NULL;
        }
      }
    }
  }

  /* Create, if needed */
  if ((D->flags & GD_CREAT && format_error) || (D->flags & GD_TRUNC))
  {
    /* can't create a read-only dirfile */
    if ((D->flags & GD_ACCMODE) == GD_RDONLY) {
      _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
      dreturn("%p", NULL);
      return NULL;
    }

    /* attempt to create the dirfile directory, if not present */
    if (dir_error) 
      if (mkdir(filedir, 00777) < 0) {
        _GD_SetError(D, GD_E_CREAT, GD_E_CREAT_DIR, filedir, errno, NULL);
        dreturn("%p", NULL);
        return NULL;
      }

    /* create a new, empty format file */
    if ((fp = fopen(format_file, "wt")) == NULL) {
      _GD_SetError(D, GD_E_CREAT, GD_E_CREAT_FORMAT, format_file, errno, NULL);
      dreturn("%p", NULL);
      return NULL;
    }

    /* set GD_UNENCODED if GD_AUTO_ENCODED was specified */
    if ((D->flags & GD_ENCODING) == GD_AUTO_ENCODED)
      D->flags = (D->flags & ~GD_ENCODING) | GD_UNENCODED;
  }

  /* open succeeds */
  dreturn("%p", fp);
  return fp;
}

void dirfile_parser_callback(DIRFILE* D, int (*sehandler)(const DIRFILE*, int,
      char*))
{
  dtrace("%p, %p", D, sehandler);

  D->sehandler = sehandler;

  dreturnvoid();
}

/* dirfile_cbopen: open (or, perhaps, create) and parse the specified dirfile
*/
DIRFILE* dirfile_cbopen(const char* filedir, unsigned long flags,
    int (*sehandler)(const DIRFILE*, int, char*))
{
  FILE *fp;
  char* ref_name;
  DIRFILE* D;
  gd_entry_t* E;
  char format_file[FILENAME_MAX];
  int standards = DIRFILE_STANDARDS_VERSION;

  dtrace("\"%s\", 0x%lx, %p", filedir, flags, sehandler);

  _GD_InitialiseFramework();

  D = malloc(sizeof(DIRFILE));
  memset(D, 0, sizeof(DIRFILE));

  D->error_string = malloc(FILENAME_MAX);
  D->error_file = malloc(FILENAME_MAX);
  D->name = strdup(filedir);
  D->flags = flags | GD_INVALID;
  D->sehandler = sehandler;

  if (D->error_string == NULL || D->error_file == NULL || D->name == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", D);
    return D;
  }

  /* Add the INDEX entry */
  D->n_entries = 1;

  D->entry = malloc(sizeof(gd_entry_t*));
  if (D->entry == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", D);
    return D;
  }

  D->entry[0] = malloc(sizeof(gd_entry_t));
  if (D->entry == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", D);
    return D;
  }
  memset(D->entry[0], 0, sizeof(gd_entry_t));

  D->entry[0]->field_type = GD_INDEX_ENTRY;
  D->entry[0]->e = malloc(sizeof(struct _gd_private_entry));
  D->entry[0]->field = strdup("INDEX");
  if (D->entry[0]->field == NULL || D->entry[0]->e == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", D);
    return D;
  }
  memset(D->entry[0]->e, 0, sizeof(struct _gd_private_entry));
  D->entry[0]->e->calculated = 1;

  snprintf(format_file, FILENAME_MAX, "%s%sformat", filedir,
      (filedir[strlen(filedir) - 1] == '/') ? "" : "/");

  /* open the format file (or create it) */
  if ((fp = _GD_CreateDirfile(D, format_file, filedir)) == NULL) {
    dreturn("%p", D);
    return D; /* errors have already been set */
  }

  /* Parse the file.  This will take care of any necessary inclusions */
  D->n_fragment = 1;

  D->fragment = malloc(sizeof(struct gd_fragment_t));
  if (D->fragment == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", D);
    return D;
  }
  D->fragment[0].cname = strdup(format_file);
  D->fragment[0].sname = NULL;
  /* The root format file needs no external name */
  D->fragment[0].ename = NULL;
  D->fragment[0].modified = 0;
  D->fragment[0].parent = -1;
  D->fragment[0].encoding = D->flags & GD_ENCODING;
  D->fragment[0].byte_sex =
#ifdef WORDS_BIGENDIAN
    (D->flags & GD_LITTLE_ENDIAN) ? GD_LITTLE_ENDIAN : GD_BIG_ENDIAN
#else
    (D->flags & GD_BIG_ENDIAN) ? GD_BIG_ENDIAN : GD_LITTLE_ENDIAN
#endif
    ;
  D->fragment[0].ref_name = NULL;
  D->fragment[0].frame_offset = 0;
  D->fragment[0].protection = GD_PROTECT_NONE;

  ref_name = _GD_ParseFragment(fp, D, 0, &standards, D->flags);
  fclose(fp);

  if (D->error != GD_E_OK) {
    dreturn("%p", D);
    return D;
  }

  /* Find the reference field */
  if (ref_name != NULL) {
    E = _GD_FindField(D, ref_name, NULL);
    if (E == NULL)
      _GD_SetError(D, GD_E_BAD_REFERENCE, GD_E_REFERENCE_CODE, NULL, 0,
          ref_name);
    else if (E->field_type != GD_RAW_ENTRY)
      _GD_SetError(D, GD_E_BAD_REFERENCE, GD_E_REFERENCE_TYPE, NULL, 0,
          ref_name);
    else
      D->reference_field = E; 
    free(ref_name);
  }

  /* Success! Clear invalid bit */
  if (D->error == GD_E_OK)
    D->flags &= ~GD_INVALID;

  dreturn("%p", D);
  return D;
}

DIRFILE* dirfile_open(const char* filedir, unsigned long flags)
{
  dtrace("\"%s\", 0x%lx", filedir, flags);

  DIRFILE* D = dirfile_cbopen(filedir, flags, NULL);

  dreturn("%p", D);
  return D;
}
/* vim: ts=2 sw=2 et tw=80
*/
