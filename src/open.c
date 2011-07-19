/* Copyright (C) 2002-2005 C. Barth Netterfield
 * Copyright (C) 2005-2011 D. V. Wiebe
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
static FILE* _GD_CreateDirfile(DIRFILE* D, int dirfd, int dir_error,
    const char* filedir)
{
  struct stat statbuf;
  DIR* dir;
  struct dirent* lamb;
  int fd = -1;
  int format_error = 0;
  FILE* fp = NULL;

  dtrace("%p, %i, %i, \"%s\"", D, dirfd, dir_error, filedir);

  /* naively try to open the format file */
  if (dirfd < 0)
    format_error = ENOENT;
  else if ((fd = gd_OpenAt(D, dirfd, "format", O_RDONLY | O_BINARY, 0666)) < 0)
  {
    format_error = errno;

    /* open failed, try to stat the directory itself */
    if (fstat(dirfd, &statbuf))
      dir_error = errno;
    else if (!S_ISDIR(statbuf.st_mode))
      dir_error = ENOTDIR;
  } else
    dir_error = 0;

  /* First, cast out our four failure modes */

  /* unable to read the format file */
  if (format_error == EACCES || dir_error == EACCES) {
    char *format_file = (char *)malloc(strlen(filedir) + 8);
    strcat(strcpy(format_file, filedir), "/format");
    _GD_SetError(D, GD_E_OPEN, GD_E_OPEN_NO_ACCESS, format_file, 0, NULL);
    free(format_file);
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
    close(fd);
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
    close(fd);

    /* can't truncate a read-only dirfile */
    if ((D->flags & GD_ACCMODE) == GD_RDONLY) {
      _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
      dreturn("%p", NULL);
      return NULL;
    }

#ifdef HAVE_FDOPENDIR
    /* crawl the directory, and delete everything */
    if ((fd = dup(dirfd)) == -1) {
      _GD_SetError(D, GD_E_TRUNC, GD_E_TRUNC_DIR, filedir, errno, NULL);
      dreturn("%p", NULL);
      return NULL;
    }
    dir = fdopendir(fd);
#else
    dir = opendir(filedir);
#endif

    if (dir == NULL) {
      _GD_SetError(D, GD_E_TRUNC, GD_E_TRUNC_DIR, filedir, errno, NULL);
      dreturn("%p", NULL);
      return NULL;
    }

    while ((lamb = readdir(dir)) != NULL) {
      if (gd_StatAt(D, dirfd, lamb->d_name, &statbuf, AT_SYMLINK_NOFOLLOW)) {
        char *name = (char *)malloc(strlen(filedir) + strlen(lamb->d_name) + 2);
        strcat(strcat(strcpy(name, filedir), "/"), lamb->d_name);

        _GD_SetError(D, GD_E_TRUNC, GD_E_TRUNC_STAT, name, errno, NULL);
        free(name);
        closedir(dir);
        dreturn("%p", NULL);
        return NULL;
      }

      /* only delete regular files */
      if (S_ISREG(statbuf.st_mode)
#ifdef S_ISLNK
          || S_ISLNK(statbuf.st_mode)
#endif
          )
      {
        if (gd_UnlinkAt(D, dirfd, lamb->d_name, 0)) {
          char *name = (char *)malloc(strlen(filedir) + strlen(lamb->d_name)
              + 2);
          strcat(strcat(strcpy(name, filedir), "/"), lamb->d_name);
          _GD_SetError(D, GD_E_TRUNC, GD_E_TRUNC_UNLINK, name, errno, NULL);
          free(name);
          closedir(dir);
          dreturn("%p", NULL);
          return NULL;
        }
      }
    }

    closedir(dir);
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
    if (dir_error) {
      if (mkdir(filedir, 00777) < 0) {
        _GD_SetError(D, GD_E_CREAT, GD_E_CREAT_DIR, filedir, errno, NULL);
        dreturn("%p", NULL);
        return NULL;
      }

      if ((dirfd = open(filedir, O_RDONLY)) < 0) {
        _GD_SetError(D, GD_E_CREAT, GD_E_CREAT_DIR, filedir, errno, NULL);
        dreturn("%p", NULL);
        return NULL;
      }
    }

    /* create a new, empty format file */
    if ((fd = gd_OpenAt(D, dirfd, "format", O_CREAT | O_EXCL | O_BINARY, 0666))
        < 0)
    {
      char *format_file = (char *)malloc(strlen(filedir) + 8);
      strcat(strcpy(format_file, filedir), "/format");
      _GD_SetError(D, GD_E_CREAT, GD_E_CREAT_FORMAT, format_file, errno, NULL);
      free(format_file);
      dreturn("%p", NULL);
      return NULL;
    }

    /* set GD_UNENCODED if GD_AUTO_ENCODED was specified */
    if ((D->flags & GD_ENCODING) == GD_AUTO_ENCODED)
      D->flags = (D->flags & ~GD_ENCODING) | GD_UNENCODED;
  }

  /* associate a stream with the format file */
  if ((fp = fdopen(fd, "r")) == NULL) {
    char *format_file = (char *)malloc(strlen(filedir) + 8);
    strcat(strcpy(format_file, filedir), "/format");
    _GD_SetError(D, GD_E_CREAT, GD_E_CREAT_FORMAT, format_file, errno, NULL);
    free(format_file);
    close(fd);
    dreturn("%p", NULL);
    return NULL;
  }

  /* open succeeds */
  D->dir = (struct gd_dir_t *)malloc(sizeof(struct gd_dir_t));
  D->dir[0].fd = dirfd;
  D->dir[0].rc = 1;
  D->dir[0].path = strdup(filedir);
  if (D->dir[0].path == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    fclose(fp);
    dreturn("%p", NULL);
    return NULL;
  }
  D->ndir = 1;

  dreturn("%p", fp);
  return fp;
}

void gd_parser_callback(DIRFILE* D, gd_parser_callback_t sehandler, void* extra)
  gd_nothrow
{
  dtrace("%p, %p, %p", D, sehandler, extra);

  D->sehandler = sehandler;
  D->sehandler_extra = extra;

  dreturnvoid();
}

DIRFILE* gd_invalid_dirfile(void) gd_nothrow
{
  DIRFILE *D;

  dtracevoid();

  D = (DIRFILE *)malloc(sizeof(DIRFILE));
  memset(D, 0, sizeof(DIRFILE));
  D->flags = GD_INVALID;

  dreturn("%p", D);
  return D;
}

/* dirfile_cbopen: open (or, perhaps, create) and parse the specified dirfile
*/
DIRFILE* gd_cbopen(const char* filedir, unsigned long flags,
    gd_parser_callback_t sehandler, void* extra)
{
  FILE *fp;
  char* ref_name;
  DIRFILE* D;
  gd_entry_t* E;
  int dirfd, dirfd_error;

  dtrace("\"%s\", 0x%lx, %p, %p", filedir, (unsigned long)flags, sehandler,
      extra);

  /* quickly, before it goes away, grab the directory (if it exists) */
  dirfd = open(filedir, O_RDONLY);
  dirfd_error = errno;

  _GD_InitialiseFramework();

  D = (DIRFILE *)malloc(sizeof(DIRFILE));
  memset(D, 0, sizeof(DIRFILE));

  /* clear GD_PERMISSIVE if it was specified along with GD_PEDANTIC */
  if (flags & GD_PERMISSIVE && flags & GD_PEDANTIC)
    flags &= ~GD_PERMISSIVE;

  D->name = strdup(filedir);
  D->flags = (flags | GD_INVALID) & ~GD_IGNORE_REFS;
  D->sehandler = sehandler;
  D->sehandler_extra = extra;
  D->standards = GD_DIRFILE_STANDARDS_VERSION;

  if (D->name == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", D);
    return D;
  }

  /* Add the INDEX entry */
  D->n_entries = 1;

  D->entry = (gd_entry_t **)malloc(sizeof(gd_entry_t*));
  if (D->entry == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", D);
    return D;
  }

  D->entry[0] = (gd_entry_t *)malloc(sizeof(gd_entry_t));
  if (D->entry == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", D);
    return D;
  }
  memset(D->entry[0], 0, sizeof(gd_entry_t));

  D->entry[0]->field_type = GD_INDEX_ENTRY;
  D->entry[0]->e =
    (struct _gd_private_entry *)malloc(sizeof(struct _gd_private_entry));
  D->entry[0]->field = strdup("INDEX");
  if (D->entry[0]->field == NULL || D->entry[0]->e == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", D);
    return D;
  }
  memset(D->entry[0]->e, 0, sizeof(struct _gd_private_entry));
  D->entry[0]->e->calculated = 1;

  /* open the format file (or create it) */
  if ((fp = _GD_CreateDirfile(D, dirfd, dirfd_error, filedir)) == NULL) {
    dreturn("%p", D);
    return D; /* errors have already been set */
  }

  /* Parse the file.  This will take care of any necessary inclusions */
  D->n_fragment = 1;

  D->fragment = (struct gd_fragment_t *)malloc(sizeof(struct gd_fragment_t));
  if (D->fragment == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", D);
    return D;
  }
  D->fragment[0].cname = malloc(strlen(filedir) + 8);
  strcat(strcpy(D->fragment[0].cname, filedir), "/format");
  D->fragment[0].sname = NULL;
  /* The root format file needs no external name */
  D->fragment[0].ename = NULL;
  D->fragment[0].modified = 0;
  D->fragment[0].parent = -1;
  D->fragment[0].dirfd = D->dir[0].fd;
  D->fragment[0].encoding = D->flags & GD_ENCODING;
  D->fragment[0].byte_sex = (
#ifdef WORDS_BIGENDIAN
      (D->flags & GD_LITTLE_ENDIAN) ? GD_LITTLE_ENDIAN : GD_BIG_ENDIAN
#else
      (D->flags & GD_BIG_ENDIAN) ? GD_BIG_ENDIAN : GD_LITTLE_ENDIAN
#endif
      ) | (D->flags & GD_ARM_FLAG);
  D->fragment[0].ref_name = NULL;
  D->fragment[0].frame_offset = 0;
  D->fragment[0].protection = GD_PROTECT_NONE;
  D->fragment[0].vers = (flags & GD_PEDANTIC) ? GD_DIRFILE_STANDARDS_VERSION :
    0;

  ref_name = _GD_ParseFragment(fp, D, 0, &D->standards, &D->flags);
  fclose(fp);

  if (D->error != GD_E_OK) {
    dreturn("%p", D);
    return D;
  }

  /* Find the reference field */
  if (ref_name != NULL) {
    E = _GD_FindField(D, ref_name, D->entry, D->n_entries, NULL);
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

  /* if GD_PEDANTIC is not set, we don't know which version this conforms to;
   * try to figure it out. */
  if (!D->error && !(D->flags & GD_PEDANTIC)) {
    if (_GD_FindVersion(D)) {
      /* conforms to some standard, use the latest */
      gd_dirfile_standards(D, GD_VERSION_LATEST); /* can't fail */
      D->flags &= ~GD_PERMISSIVE;
    } else
      /* non-conformant dirfile, flag it */
      D->flags |= GD_PERMISSIVE;
  } else
    D->flags &= ~GD_PERMISSIVE;

  dreturn("%p", D);
  return D;
}

DIRFILE* gd_open(const char* filedir, unsigned long flags)
{
  DIRFILE *D;

  dtrace("\"%s\", 0x%lx", filedir, (unsigned long)flags);

  D = gd_cbopen(filedir, flags, NULL, NULL);

  dreturn("%p", D);
  return D;
}
/* vim: ts=2 sw=2 et tw=80
*/
