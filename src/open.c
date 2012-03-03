/* Copyright (C) 2002-2005 C. Barth Netterfield
 * Copyright (C) 2005-2012 D. V. Wiebe
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

/* crawl the directory, and delete everything */
static int _GD_TruncDir(DIRFILE *D, int dirfd, const char *dirfile, int root)
{
  int format_trunc = 0;
  DIR* dir;
  struct dirent* lamb;
  struct stat statbuf;

  dtrace("%p, %i, \"%s\", %i", D, dirfd, dirfile, root);

#if defined(HAVE_FDOPENDIR) && !defined(GD_NO_DIR_OPEN)
  /* only need to duplicate the fd of the root directory; otherwise this
   * function will close the fd */
  if (root && (fd = dup(dirfd)) == -1) {
    _GD_SetError(D, GD_E_TRUNC, GD_E_TRUNC_DIR, dirfile, errno, NULL);
    dreturn("%i", -1);
    return -1;
  }
  dir = fdopendir(fd);
#else
  dir = opendir(dirfile);
#endif

  if (dir == NULL) {
    _GD_SetError(D, GD_E_TRUNC, GD_E_TRUNC_DIR, dirfile, errno, NULL);
    dreturn("%i", -1);
    return -1;
  }

  while ((lamb = readdir(dir)) != NULL) {
    if (lamb->d_name[0] == '.' && lamb->d_name[1] == '\0') {
      continue; /* skip current dir */
    } else if (lamb->d_name[0] == '.' && lamb->d_name[1] == '.' &&
        lamb->d_name[2] == '\0')
    {
      continue; /* skip parent dir */
    } else {
      char *name = (char *)malloc(strlen(dirfile) + strlen(lamb->d_name) + 2);
      sprintf(name, "%s%c%s", dirfile, GD_DIRSEP, lamb->d_name);
      if (
#if defined(HAVE_FSTATAT) && !defined(GD_NO_DIR_OPEN)
        fstatat(dirfd, lamb->d_name, &statbuf, AT_SYMLINK_NOFOLLOW)
#elif HAVE_LSTAT
        lstat(name, &statbuf)
#else
        stat(name, &statbuf)
#endif
        )
      {
        _GD_SetError(D, GD_E_TRUNC, GD_E_TRUNC_STAT, name, errno, NULL);
        free(name);
        closedir(dir);
        dreturn("%i", -1);
        return -1;
      }
      free(name);
    }

    /* check file type */
    switch (statbuf.st_mode & S_IFMT) {
      case S_IFREG:
      case S_IFBLK:
      case S_IFIFO:
      case S_IFCHR:
      case S_IFLNK:
        if (root && strcmp(lamb->d_name, "format") == 0) {
          /* don't delete the format file; we'll truncate it later */
          format_trunc = 1;
        } else if (gd_UnlinkAt(D, dirfd, lamb->d_name, 0)) {
          char *name = (char *)malloc(strlen(dirfile) + strlen(lamb->d_name)
              + 2);
          sprintf(name, "%s%c%s", dirfile, GD_DIRSEP, lamb->d_name);
          _GD_SetError(D, GD_E_TRUNC, GD_E_TRUNC_UNLINK, name, errno, NULL);
          free(name);
          closedir(dir);
          dreturn("%i", -1);
          return -1;
        }
        break;
      case S_IFDIR:
        /* descend into subdir if requested */
        if (D->flags & GD_TRUNCSUB) {
          int subdirfd;
          char *subdir = (char *)malloc(strlen(dirfile) + strlen(lamb->d_name)
              + 2);
          sprintf(subdir, "%s%c%s", dirfile, GD_DIRSEP, lamb->d_name);
#ifdef GD_NO_DIR_OPEN
          subdirfd = 0; /* unused */
#else
          if ((
#ifdef HAVE_OPENAT
                subdirfd = openat(dirfd, lamb->d_name, O_RDONLY)
#else
                subdirfd = open(subdir, O_RDONLY)
#endif
              ) < 0)
          {
            _GD_SetError(D, GD_E_TRUNC, GD_E_TRUNC_STAT, subdir, errno, NULL);
            free(subdir);
            dreturn("%i", -1);
            return -1;
          }
#endif
          /* descend -- this will close subdirfd */
          _GD_TruncDir(D, subdirfd, subdir, 0);
          free(subdir);

          /* delete */
#ifdef HAVE_UNLINKAT___
          unlinkat(dirfd, lamb->d_name, /* AT_...? */);
#else
          rmdir(subdir);
#endif
        }
    }
  }

  closedir(dir);
  dreturn("%i", format_trunc);
  return format_trunc;
}

/* attempt to open or create a new dirfile - set error appropriately */
static FILE *_GD_CreateDirfile(DIRFILE *restrict D, int dirfd, int dir_error,
    char *restrict dirfile)
{
#ifndef GD_NO_DIR_OPEN
  struct stat statbuf;
#endif
  int fd = -1;
  int format_error = 0, format_trunc = 0;
  FILE* fp = NULL;

  dtrace("%p, %i, %i, \"%s\"", D, dirfd, dir_error, dirfile);

  /* naively try to open the format file */
  if (dirfd < 0)
    format_error = ENOENT;
  else if ((fd = gd_OpenAt(D, dirfd, "format", O_RDONLY | O_BINARY, 0666)) < 0)
  {
    format_error = errno;

    /* in the non-POSIX case, this has already been done. */
#ifndef GD_NO_DIR_OPEN
    /* open failed, try to stat the directory itself */
    if (fstat(dirfd, &statbuf))
      dir_error = errno;
    else if (!S_ISDIR(statbuf.st_mode))
      dir_error = ENOTDIR;
#endif
  } else
    dir_error = 0;

  /* First, cast out our four failure modes */

  /* unable to read the format file */
  if (format_error == EACCES || dir_error == EACCES) {
    char *format_file = (char *)malloc(strlen(dirfile) + 8);
    sprintf(format_file, "%s%cformat", dirfile, GD_DIRSEP);
    _GD_SetError(D, GD_E_OPEN, GD_E_OPEN_NO_ACCESS, format_file, 0, NULL);
    free(dirfile);
    free(format_file);
    dreturn("%p", NULL);
    return NULL;
  }

  /* the directory exists, but it's not a dirfile, do nothing else -- even if we
   * were asked to truncate it */
  if (!dir_error && format_error) {
    _GD_SetError(D, GD_E_OPEN, GD_E_OPEN_NOT_DIRFILE, dirfile, 0, NULL);
    free(dirfile);
    dreturn("%p", NULL);
    return NULL;
  }

  /* Couldn't open the file, and we weren't asked to create it */
  if (format_error && !(D->flags & GD_CREAT)) {
    _GD_SetError(D, GD_E_OPEN, GD_E_OPEN_NOT_EXIST, dirfile, format_error,
        NULL);
    free(dirfile);
    dreturn("%p", NULL);
    return NULL;
  }

  /* It does exist, but we were asked to exclusively create it */
  if (!format_error && (D->flags & GD_CREAT) && (D->flags & GD_EXCL)) {
    _GD_SetError(D, GD_E_CREAT, GD_E_CREAT_EXCL, dirfile, 0, NULL);
    free(dirfile);
    close(fd);
    dreturn("%p", NULL);
    return NULL;
  }

  /* If we made it here we either:
   * 1) have no such directory, but plan to create it, or
   * 2) have a "dirfile", i.e. the directory supplied contains a readable
   *   file called format */

  /* Truncate, if needed -- dangerous!  Truncating a dirfile deletes every
   * regular file in the specified directory.  It does not touch subdirectories.
   * Note that the rather lame definition of a dirfile at this point
   * (specifically, we haven't bothered to see if the format file is parsable)
   * could be problematic if users use GD_TRUNC cavalierly. */
  if (D->flags & GD_TRUNC && !format_error) {
    close(fd);

    /* can't truncate a read-only dirfile */
    if ((D->flags & GD_ACCMODE) == GD_RDONLY) {
      _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
      free(dirfile);
      dreturn("%p", NULL);
      return NULL;
    }

    format_trunc = _GD_TruncDir(D, dirfd, dirfile, 1);
    if (format_trunc < 0) {
      free(dirfile);
      dreturn("%p", NULL);
      return NULL;
    }
  }

  /* Create, if needed */
  if ((D->flags & GD_CREAT && format_error) || (D->flags & GD_TRUNC))
  {
    /* can't create a read-only dirfile */
    if ((D->flags & GD_ACCMODE) == GD_RDONLY) {
      _GD_SetError(D, GD_E_ACCMODE, 0, NULL, 0, NULL);
      free(dirfile);
      dreturn("%p", NULL);
      return NULL;
    }

    /* attempt to create the dirfile directory, if not present */
    if (dir_error) {
      if (mkdir(dirfile, 0777) < 0) {
        _GD_SetError(D, GD_E_CREAT, GD_E_CREAT_DIR, dirfile, errno, NULL);
        free(dirfile);
        dreturn("%p", NULL);
        return NULL;
      }

#ifdef GD_NO_DIR_OPEN
      /* in the non-POSIX situation, dirfd just holds the index in the
       * directory entry */
      dirfd = 0;
#else
      if ((dirfd = open(dirfile, O_RDONLY)) < 0) {
        _GD_SetError(D, GD_E_CREAT, GD_E_CREAT_OPEN, dirfile, errno, NULL);
        free(dirfile);
        dreturn("%p", NULL);
        return NULL;
      }
#endif
    }

    /* create a new, empty format file, or else truncate it */
    if ((fd = gd_OpenAt(D, dirfd, "format", O_RDWR | O_CREAT | O_BINARY |
            (format_trunc ? O_TRUNC : O_EXCL), 0666)) < 0)
    {
      char *format_file = (char *)malloc(strlen(dirfile) + 8);
      strcat(strcpy(format_file, dirfile), "/format");
      _GD_SetError(D, GD_E_CREAT, GD_E_CREAT_FORMAT, format_file, errno, NULL);
      free(dirfile);
      free(format_file);
      dreturn("%p", NULL);
      return NULL;
    }

    /* set GD_UNENCODED if GD_AUTO_ENCODED was specified */
    if ((D->flags & GD_ENCODING) == GD_AUTO_ENCODED)
      D->flags = (D->flags & ~GD_ENCODING) | GD_UNENCODED;
  }

  /* associate a stream with the format file */
  if ((fp = fdopen(fd, "rb")) == NULL) {
    char *format_file = (char *)malloc(strlen(dirfile) + 8);
    strcat(strcpy(format_file, dirfile), "/format");
    _GD_SetError(D, GD_E_CREAT, GD_E_CREAT_FORMAT, format_file, errno, NULL);
    free(dirfile);
    free(format_file);
    close(fd);
    dreturn("%p", NULL);
    return NULL;
  }

  /* open succeeds */
  D->dir = (struct gd_dir_t *)malloc(sizeof(struct gd_dir_t));
  D->dir[0].fd = dirfd;
  D->dir[0].rc = 1;
  D->dir[0].path = dirfile;
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
  if (D) {
    memset(D, 0, sizeof(DIRFILE));
    D->flags = GD_INVALID;
  }

  dreturn("%p", D);
  return D;
}

/* dirfile_cbopen: open (or, perhaps, create) and parse the specified dirfile
*/
DIRFILE* gd_cbopen(const char* filedir, unsigned long flags,
    gd_parser_callback_t sehandler, void* extra)
{
  FILE *fp;
  char *ref_name;
  char *dirfile;
  DIRFILE* D;
  gd_entry_t* E;
  int dirfd = -1, dirfd_error = 0;

#ifdef GD_NO_DIR_OPEN
  gd_stat64_t statbuf;
#endif

  dtrace("\"%s\", 0x%lX, %p, %p", filedir, (unsigned long)flags, sehandler,
      extra);

#ifdef GD_NO_DIR_OPEN
  /* if we can't cache directory descriptors, we just have to remember paths,
   * so get the current directory, so that we're protected against the caller
   * using chdir.
   */

  /* canonicalise the path */
  dirfile = _GD_CanonicalPath(NULL, filedir);

  /* that done, now stat the path to see if it exists (and is a directory) */
  if (dirfile) {
    if (gd_stat64(dirfile, &statbuf))
      dirfd_error = errno;
    else if (!S_ISDIR(statbuf.st_mode))
      dirfd_error = ENOTDIR;
    else
      dirfd = 0;
  }
#else
  /* quickly, before it goes away, grab the directory (if it exists) */
  dirfd = open(filedir, O_RDONLY);
  dirfd_error = errno;
  /* There's a race condition here: someone might change a path element in
   * filedir after we get dirfd but before we canonicalise the path.  For that
   * reason, anything requiring a full path must be treated with suspicion.
   */
  dirfile = _GD_CanonicalPath(NULL, filedir);
#endif
  _GD_InitialiseFramework();

  D = (DIRFILE *)malloc(sizeof(DIRFILE));
  if (dirfile == NULL || D == NULL) {
    free(dirfile);
#ifndef GD_NO_DIR_OPEN
    close(dirfd);
#endif
    dreturn("%p", NULL);
    return NULL;
  }
  memset(D, 0, sizeof(DIRFILE));

  /* clear GD_PERMISSIVE if it was specified along with GD_PEDANTIC */
  if (flags & GD_PERMISSIVE && flags & GD_PEDANTIC)
    flags &= ~GD_PERMISSIVE;

  D->name = dirfile; /* temporarily store canonicalised path here */
  D->flags = (flags | GD_INVALID) & ~GD_IGNORE_REFS;
  D->sehandler = sehandler;
  D->sehandler_extra = extra;
  D->standards = GD_DIRFILE_STANDARDS_VERSION;

  /* Add the INDEX entry */
  D->n_entries = 1;
  D->n[_GD_EntryIndex(GD_INDEX_ENTRY)] = 1;

  D->entry = (gd_entry_t **)_GD_Malloc(D, sizeof(gd_entry_t*));
  if (D->entry)
    D->entry[0] = (gd_entry_t *)_GD_Malloc(D, sizeof(gd_entry_t));

  if (D->error) {
    free(dirfile);
#ifndef GD_NO_DIR_OPEN
    close(dirfd);
#endif
    dreturn("%p", D);
    return D;
  }

  memset(D->entry[0], 0, sizeof(gd_entry_t));
  D->entry[0]->field_type = GD_INDEX_ENTRY;
  D->entry[0]->e =
    (struct _gd_private_entry *)_GD_Malloc(D, sizeof(struct _gd_private_entry));
  D->entry[0]->field = _GD_Strdup(D, "INDEX");
  if (D->error) {
    free(dirfile);
#ifndef GD_NO_DIR_OPEN
    close(dirfd);
#endif
    dreturn("%p", D);
    return D;
  }
  memset(D->entry[0]->e, 0, sizeof(struct _gd_private_entry));
  D->entry[0]->e->calculated = 1;

  /* open the format file (or create it) */
  if ((fp = _GD_CreateDirfile(D, dirfd, dirfd_error, dirfile)) == NULL) {
#ifndef GD_NO_DIR_OPEN
    close(dirfd);
#endif
    D->name = NULL; /* so a subsequent gd_discard() doesn't go awry. */
    dreturn("%p", D);
    return D; /* errors have already been set */
  }

  /* remember back when we temporarily stored the canonicalised path here?
   * We're over that.  Remember the dirfile's name.  */
  D->name = strdup(filedir);

  if (D->name == NULL) {
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", D);
    return D;
  }

  /* Parse the file.  This will take care of any necessary inclusions */
  D->n_fragment = 1;

  D->fragment = (struct gd_fragment_t *)_GD_Malloc(D,
      sizeof(struct gd_fragment_t));
  if (D->error) {
    dreturn("%p", D);
    return D;
  }

  D->fragment[0].cname = _GD_CanonicalPath(dirfile, "format");
  if (D->fragment[0].cname == NULL) {
    if (errno == ENOMEM)
      _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    else
      _GD_SetError(D, GD_E_OPEN, GD_E_OPEN_PATH, dirfile, 0, NULL);
  }

  D->fragment[0].bname = _GD_Strdup(D, "format");

  if (D->error) {
    dreturn("%p", D);
    return D;
  }
  D->fragment[0].sname = NULL;
  /* The root format file needs no external name */
  D->fragment[0].ename = NULL;
  D->fragment[0].enc_data = NULL;
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
  D->fragment[0].suffix = D->fragment[0].prefix = NULL;

  ref_name = _GD_ParseFragment(fp, D, 0, &D->standards, &D->flags, 1);
  fclose(fp);

  if (D->error != GD_E_OK) {
    dreturn("%p", D);
    return D;
  }

  /* Find the reference field */
  if (ref_name != NULL) {
    E = _GD_FindField(D, ref_name, D->entry, D->n_entries, 1, NULL);
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

  /* if GD_PEDANTIC is not set or GD_MULTISTANDARD is set, we don't know which
   * version this conforms to; try to figure it out. */
  if (!D->error && (!(D->flags & GD_PEDANTIC) || D->flags & GD_MULTISTANDARD)) {
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

  dtrace("\"%s\", 0x%lX", filedir, (unsigned long)flags);

  D = gd_cbopen(filedir, flags, NULL, NULL);

  dreturn("%p", D);
  return D;
}
/* vim: ts=2 sw=2 et tw=80
*/
