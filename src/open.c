/* Copyright (C) 2002-2005 C. Barth Netterfield
 * Copyright (C) 2005-2016 D. V. Wiebe
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
  int ret, format_trunc = 0;
  DIR* dir;
  struct dirent *result;
  struct dirent unused;
  struct stat statbuf;
  size_t dirent_len = offsetof(struct dirent, d_name);
  size_t dirfile_len = strlen(dirfile);

  dtrace("%p, %i, \"%s\", %i", D, dirfd, dirfile, root);

#if defined(HAVE_FDOPENDIR) && !defined(GD_NO_DIR_OPEN)
  {
    int fd = dirfd;
    /* only need to duplicate the fd of the root directory; otherwise this
     * function will close the fd */
    if (root && (fd = dup(dirfd)) == -1) {
      _GD_SetError(D, GD_E_IO, GD_E_IO_OPEN, dirfile, 0, NULL);
      dreturn("%i", -1);
      return -1;
    }
    dir = fdopendir(fd);

#ifdef HAVE_FPATHCONF
    dirent_len += fpathconf(fd, _PC_NAME_MAX) + 1;
#elif defined HAVE_PATHCONF
    dirent_len += pathconf(dirfile, _PC_NAME_MAX) + 1;
#else
    dirent_len += FILENAME_MAX;
#endif
  }

#else
  dir = opendir(dirfile);

#ifdef HAVE_PATHCONF
  dirent_len += pathconf(dirfile, _PC_NAME_MAX) + 1;
#else
  dirent_len += FILENAME_MAX;
#endif

#endif

  if (dir == NULL) {
    _GD_SetError(D, GD_E_IO, GD_E_IO_OPEN, dirfile, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  ret = _GD_ReadDir(dir, &unused, &result);
  for (; result; ret = _GD_ReadDir(dir, &unused, &result)) {
    char *name;
    if (result->d_name[0] == '.' && result->d_name[1] == '\0')
      continue; /* skip current dir */
    else if (result->d_name[0] == '.' && result->d_name[1] == '.' &&
        result->d_name[2] == '\0')
    {
      continue; /* skip parent dir */
    }

    name = _GD_Malloc(D, dirfile_len + strlen(result->d_name) + 2);
    if (name == NULL) {
      closedir(dir);
      dreturn("%i", -1);
      return -1;
    }
    sprintf(name, "%s%c%s", dirfile, GD_DIRSEP, result->d_name);
    if (
#if defined(HAVE_FSTATAT) && !defined(GD_NO_DIR_OPEN)
        fstatat(dirfd, result->d_name, &statbuf, AT_SYMLINK_NOFOLLOW)
#elif HAVE_LSTAT
        lstat(name, &statbuf)
#else
        stat(name, &statbuf)
#endif
       )
    {
      _GD_SetError(D, GD_E_IO, 0, name, 0, NULL);
      free(name);
      closedir(dir);
      dreturn("%i", -1);
      return -1;
    }

    /* check file type */
    switch (statbuf.st_mode & S_IFMT) {
      case S_IFREG:
#ifdef S_IFBLK
      case S_IFBLK:
#endif
#ifdef S_IFIFO
      case S_IFIFO:
#endif
#ifdef S_IFCHR
      case S_IFCHR:
#endif
#ifdef S_IFLNK
      case S_IFLNK:
#endif
        if (root && strcmp(result->d_name, "format") == 0) {
          /* don't delete the format file; we'll truncate it later */
          format_trunc = 1;
        } else if (
#ifdef HAVE_UNLINKAT
            unlinkat(dirfd, result->d_name, 0)
#else
            unlink(name)
#endif
            )
        {
          _GD_SetError(D, GD_E_IO, GD_E_IO_UNLINK, name, 0, NULL);
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
#ifdef GD_NO_DIR_OPEN
          subdirfd = 0; /* unused */
#else
          if ((
#ifdef HAVE_OPENAT
                subdirfd = openat(dirfd, result->d_name, O_RDONLY)
#else
                subdirfd = open(name, O_RDONLY)
#endif
              ) < 0)
          {
            _GD_SetError(D, GD_E_IO, 0, name, 0, NULL);
            closedir(dir);
            free(name);
            dreturn("%i", -1);
            return -1;
          }
#endif
          /* descend -- this will close subdirfd */
          _GD_TruncDir(D, subdirfd, name, 0);

          /* delete */
          if (
#ifdef HAVE_UNLINKAT
              unlinkat(dirfd, result->d_name, AT_REMOVEDIR)
#else
              rmdir(name)
#endif
             ) {
            _GD_SetError(D, GD_E_IO, GD_E_IO_UNLINK, name, 0, NULL);
            free(name);
            closedir(dir);
            dreturn("%i", -1);
            return -1;
          }
        }
    }
    free(name);
  }

  closedir(dir);

  if (ret) {
    _GD_SetError(D, GD_E_IO, GD_E_IO_READ, dirfile, 0, NULL);
    dreturn("%i", -1);
    return -1;
  }

  dreturn("%i", format_trunc);
  return format_trunc;
}

/* attempt to open or create a new dirfile - set error appropriately */
static FILE *_GD_CreateDirfile(DIRFILE *restrict D, int dirfd, int dir_error,
    char *restrict dirfile, time_t *mtime)
{
  struct stat statbuf;
  int fd = -1;
  int format_error = 0, format_trunc = 0;
  FILE* fp = NULL;

  dtrace("%p, %i, %i, \"%s\", %p", D, dirfd, dir_error, dirfile, mtime);

  /* naively try to open the format file */
  if (dirfd < 0)
    ; /* Directory error */
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

  /* First, cast out our failure modes */

  /* Error reading the directory, and we weren't asked to create it */
  if (dir_error == EACCES || (dirfd < 0 && !(D->flags & GD_CREAT))) {
    _GD_SetError2(D, GD_E_IO, GD_E_IO_OPEN, dirfile, 0, NULL, dir_error);
    free(dirfile);
    dreturn("%p", NULL);
    return NULL;
  }

  /* Error reading the format file, and we weren't asked to create it; do
   * nothing else. */
  if (format_error == EACCES || (format_error && !(D->flags & GD_CREAT))) {
    char *format_file = malloc(strlen(dirfile) + 8);
    sprintf(format_file, "%s%cformat", dirfile, GD_DIRSEP);
    _GD_SetError2(D, GD_E_IO, GD_E_IO_OPEN, format_file, 0, NULL, format_error);
    free(format_file);
    free(dirfile);
    dreturn("%p", NULL);
    return NULL;
  }

  /* It does exist, but we were asked to exclusively create it */
  if (!format_error && dirfd >= 0 && (D->flags & GD_CREAT)
      && (D->flags & GD_EXCL))
  {
    _GD_SetError(D, GD_E_EXISTS, 0, NULL, 0, NULL);
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
  if (D->flags & GD_TRUNC && !format_error && dirfd >= 0) {
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
  if ((D->flags & GD_CREAT && (dirfd < 0 || format_error))
      || (D->flags & GD_TRUNC))
  {
    /* a newly created dirfile ignores the specified access mode */
    if ((D->flags & GD_ACCMODE) == GD_RDONLY)
      D->flags |= GD_RDWR;

    /* attempt to create the dirfile directory, if not present */
    if (dirfd < 0) {
      if (mkdir(dirfile, 0777) < 0) {
        _GD_SetError(D, GD_E_CREAT, GD_E_CREAT_DIR, dirfile, 0, NULL);
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
        _GD_SetError(D, GD_E_CREAT, GD_E_CREAT_OPEN, dirfile, 0, NULL);
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
      char *format_file = malloc(strlen(dirfile) + 8);
      sprintf(format_file, "%s/format", dirfile);
      _GD_SetError(D, GD_E_CREAT, GD_E_CREAT_FORMAT, format_file, 0, NULL);
      free(dirfile);
      free(format_file);
#ifndef GD_NO_DIR_OPEN
      if (dirfd >= 0)
        close(dirfd);
#endif
      dreturn("%p", NULL);
      return NULL;
    }

    /* set GD_UNENCODED if GD_AUTO_ENCODED was specified */
    if ((D->flags & GD_ENCODING) == GD_AUTO_ENCODED)
      D->flags = (D->flags & ~GD_ENCODING) | GD_UNENCODED;
  }

  /* associate a stream with the format file */
  if ((fp = fdopen(fd, "rb")) == NULL) {
    char *format_file = malloc(strlen(dirfile) + 8);
    sprintf(format_file, "%s/format", dirfile);
    _GD_SetError(D, GD_E_CREAT, GD_E_CREAT_FORMAT, format_file, 0, NULL);
    free(dirfile);
    free(format_file);
    close(fd);
#ifndef GD_NO_DIR_OPEN
    if (dirfd >= 0)
      close(dirfd);
#endif
    dreturn("%p", NULL);
    return NULL;
  }

  /* open succeeds */
  D->dir = malloc(sizeof(struct gd_dir_t));
  D->dir[0].fd = dirfd;
  D->dir[0].rc = 1;
  D->dir[0].path = dirfile;
  D->ndir = 1;

  /* get the mtime */
  if (fstat(fd, &statbuf) == 0)
    *mtime = statbuf.st_mtime;

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

  D = malloc(sizeof(DIRFILE));
  if (D) {
    memset(D, 0, sizeof(DIRFILE));
    D->flags = GD_INVALID;
  }

  dreturn("%p", D);
  return D;
}

/* _GD_Open: open (or, perhaps, create) and parse the specified dirfile
*/
static DIRFILE *_GD_Open(DIRFILE *D, int dirfd, const char *filedir,
    unsigned long flags, gd_parser_callback_t sehandler, void *extra)
{
  FILE *fp;
  char *ref_name;
  char *dirfile;
  gd_entry_t* E;
  int dirfd_error = 0;
  time_t mtime = 0;
  struct parser_state p;

#ifdef GD_NO_DIR_OPEN
  gd_stat64_t statbuf;
#endif

  dtrace("%p, %i, \"%s\", 0x%lX, %p, %p", D, dirfd, filedir,
      (unsigned long)flags, sehandler, extra);

  errno = 0;

  /* canonicalise the path to protect us against the caller chdir'ing away */
  dirfile = _GD_CanonicalPath(NULL, filedir);

  if (dirfile) {
#ifdef GD_NO_DIR_OPEN
  /* if we can't cache directory descriptors, we just have to remember paths.
   * so stat the path to see if it exists (and is a directory) */
    if (gd_stat64(dirfile, &statbuf))
      dirfd_error = errno;
    else if (!S_ISDIR(statbuf.st_mode))
      dirfd_error = ENOTDIR;
    else
      dirfd = 0;
#else
    /* quickly, before it goes away, grab the directory (if it exists) */
    if (dirfd == -1)
      dirfd = open(dirfile, O_RDONLY);
    dirfd_error = errno;
#endif
  } else
    dirfd_error = errno;
  _GD_InitialiseFramework();

  if (D == NULL)
    D = malloc(sizeof(DIRFILE));
  if (D == NULL) {
    free(dirfile);
#ifndef GD_NO_DIR_OPEN
    if (dirfd >= 0)
      close(dirfd);
#endif
    dreturn("%p", NULL);
    return NULL;
  }
  memset(D, 0, sizeof(DIRFILE));

  /* user specified flags (used if we're forced to re-open) */
  D->open_flags = flags;

  /* clear GD_PERMISSIVE if it was specified along with GD_PEDANTIC */
  if (flags & GD_PERMISSIVE && flags & GD_PEDANTIC)
    flags &= ~GD_PERMISSIVE;

  D->name = dirfile; /* temporarily store canonicalised path here */
  D->flags = (flags | GD_INVALID) & ~GD_IGNORE_REFS;
  D->sehandler = sehandler;
  D->sehandler_extra = extra;
  D->standards = GD_DIRFILE_STANDARDS_VERSION;
  D->lookback = GD_DEFAULT_LOOKBACK;

  if (dirfile == NULL) {
    _GD_SetError2(D, GD_E_IO, 0, filedir, 0, NULL, dirfd_error);
#ifndef GD_NO_DIR_OPEN
    if (dirfd >= 0)
      close(dirfd);
#endif
    dreturn("%p", D);
    return D;
  }

  /* Add the INDEX entry */
  D->n_entries = 1;

  D->entry = _GD_Malloc(D, sizeof(*D->entry));
  if (D->entry)
    D->entry[0] = _GD_Malloc(D, sizeof(**D->entry));

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
  D->entry[0]->e = _GD_Malloc(D, sizeof(*D->entry[0]->e));
  D->entry[0]->field = _GD_Strdup(D, "INDEX");
  if (D->error) {
    free(dirfile);
#ifndef GD_NO_DIR_OPEN
    close(dirfd);
#endif
    dreturn("%p", D);
    return D;
  }
  memset(D->entry[0]->e, 0, sizeof(struct gd_private_entry_));
  D->entry[0]->flags = GD_EN_CALC;
  D->entry[0]->e->len = 5;

  /* open the format file (or create it) */
  if ((fp = _GD_CreateDirfile(D, dirfd, dirfd_error, dirfile, &mtime)) == NULL)
  {
#ifndef GD_NO_DIR_OPEN
    if (dirfd >= 0)
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
    fclose(fp);
    _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    dreturn("%p", D);
    return D;
  }

  /* Build the root fragment metadata */
  D->n_fragment = 1;

  D->fragment = _GD_Malloc(D, sizeof(*D->fragment));
  if (D->error) {
    dreturn("%p", D);
    return D;
  }

  D->fragment[0].cname = _GD_CanonicalPath(dirfile, "format");
  if (D->fragment[0].cname == NULL) {
    if (errno == ENOMEM)
      _GD_SetError(D, GD_E_ALLOC, 0, NULL, 0, NULL);
    else
      _GD_SetError(D, GD_E_IO, 0, dirfile, 0, NULL);
  }

  D->fragment[0].bname = _GD_Strdup(D, "format");

  if (D->error) {
    dreturn("%p", D);
    return D;
  }
  D->fragment[0].sname = NULL;
  D->fragment[0].ename = NULL; /* The root format file needs no external name */
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
  D->fragment[0].mtime = mtime;
  D->fragment[0].protection = GD_PROTECT_NONE;
  D->fragment[0].vers = (flags & GD_PEDANTIC) ? GD_DIRFILE_STANDARDS_VERSION :
    0;
  D->fragment[0].px = D->fragment[0].sx = D->fragment[0].ns = NULL;
  D->fragment[0].pxl = D->fragment[0].sxl = D->fragment[0].nsl = 0;

  /* parser proto-state */
  p.line = 0;
  p.file = NULL;
  p.standards = GD_DIRFILE_STANDARDS_VERSION;
  p.pedantic = flags & GD_PEDANTIC;
  p.flags = D->flags;
  p.ns = NULL;
  p.nsl = 0;

  /* Parser invocation */
  ref_name = _GD_ParseFragment(fp, D, &p, 0, 1);
  fclose(fp);
  free(p.ns);

  if (D->error != GD_E_OK) {
    dreturn("%p", D);
    return D;
  }

  /* export the parser data */
  D->standards = p.standards;
  if (p.pedantic)
    D->flags |= GD_PEDANTIC;
  else
    D->flags &= ~GD_PEDANTIC;

  /* Find the reference field */
  if (ref_name != NULL) {
    E = _GD_FindField(D, ref_name, strlen(ref_name), D->entry, D->n_entries, 1,
        NULL);
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
      D->flags &= ~GD_NOSTANDARD;
    } else
      /* non-conformant dirfile, flag it */
      D->flags |= GD_NOSTANDARD;
  } else
    D->flags &= ~GD_NOSTANDARD;

  dreturn("%p", D);
  return D;
}

DIRFILE *gd_cbopen(const char* filedir, unsigned long flags,
    gd_parser_callback_t sehandler, void* extra)
{
  DIRFILE *D;

  dtrace("\"%s\", 0x%lX, %p, %p", filedir, (unsigned long)flags, sehandler,
      extra);

  D = _GD_Open(NULL, -1, filedir, flags, sehandler, extra);

  dreturn("%p", D);
  return D;
}

DIRFILE* gd_open(const char* filedir, unsigned long flags)
{
  DIRFILE *D;

  dtrace("\"%s\", 0x%lX", filedir, (unsigned long)flags);

  D = _GD_Open(NULL, -1, filedir, flags, NULL, NULL);

  dreturn("%p", D);
  return D;
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
/* vim: ts=2 sw=2 et tw=80
*/
