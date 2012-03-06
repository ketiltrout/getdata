/* Copyright (C) 2010, 2011 D. V. Wiebe
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

/* The MSVCRT gmtime() is threadsafe */
#ifndef HAVE_GMTIME_R
struct tm *gmtime_r(const time_t *restrict timep, struct tm *restrict result)
{
  struct tm *ptr;

  dtrace("%p, %p", timep, result);

  ptr = gmtime(timep);
  if (!ptr)
    return NULL;

  memcpy(result, ptr, sizeof(struct tm));

  dreturn("%p", result);
  return result;
}
#endif

/* ...at() functions for platforms lacking them.  These are originally Solaris
 * extensions which have subsequently been included in POSIX.1-2008.
 */
#ifndef HAVE_OPENAT
int gd_OpenAt(const DIRFILE *D, int dirfd, const char *name, int flags,
    mode_t mode)
{
  int ret;
  char *path;

  dtrace("%p, %i, \"%s\", %x, 0%o", D, dirfd, name, flags, mode);

  path = _GD_MakeFullPathOnly(D, dirfd, name);
  ret = open(path, flags | O_BINARY, mode);
  free(path);

  dreturn("%i", ret);
  return ret;
}
#endif

#ifndef HAVE_RENAMEAT
int gd_RenameAt(const DIRFILE *D, int olddirfd, const char *oldname,
    int newdirfd, const char *newname)
{
  int ret;
  char *oldpath, *newpath;

  dtrace("%p, %i, \"%s\", %i, \"%s\"", D, olddirfd, oldname, newdirfd, newname);

  newpath = _GD_MakeFullPathOnly(D, newdirfd, newname);
#ifdef __MSVCRT__
  if (unlink(newpath)) {
    if (errno != ENOENT) {
      free(newpath);
      dreturn("%i", -1);
      return -1;
    }
  }
#endif

  oldpath = _GD_MakeFullPathOnly(D, olddirfd, oldname);
  ret = rename(oldpath, newpath);
  free(newpath);
  free(oldpath);

  dreturn("%i", ret);
  return ret;
}
#endif

#ifndef HAVE_FSTATAT
int gd_StatAt(const DIRFILE* D, int dirfd, const char* name, struct stat* buf,
    int flags)
{
  int ret;
  char *path;

  dtrace("%p, %i, \"%s\", %p, %x", D, dirfd, name, buf, flags);

  path = _GD_MakeFullPathOnly(D, dirfd, name);
#if defined(HAVE_LSTAT) && defined(AT_SYMLINK_NOFOLLOW)
  if (flags & AT_SYMLINK_NOFOLLOW)
    ret = lstat(path, buf);
  else
#endif
    ret = stat(path, buf);
  free(path);

  dreturn("%i", ret);
  return ret;
}
#endif

#ifndef HAVE_FSTATAT64
int gd_StatAt64(const DIRFILE* D, int dirfd, const char* name, gd_stat64_t* buf,
    int flags __gd_unused)
{
  int ret;
  char *path;

  dtrace("%p, %i, \"%s\", %p, <unused>", D, dirfd, name, buf);

  path = _GD_MakeFullPathOnly(D, dirfd, name);
  ret = gd_stat64(path, buf);
  free(path);

  dreturn("%i", ret);
  return ret;
}
#endif

#ifndef HAVE_UNLINKAT
int gd_UnlinkAt(const DIRFILE *D, int dirfd, const char *name,
    int flags __gd_unused)
{
  int ret;
  char *path;

  dtrace("%p, %i, \"%s\", <unused>", D, dirfd, name);

  path = _GD_MakeFullPathOnly(D, dirfd, name);
  ret = unlink(path);
  free(path);

  dreturn("%i", ret);
  return ret;
}
#endif

/* Non-threadsafe version of strerror_r */
#ifndef HAVE_STRERROR_R
int strerror_r(int errnum, char *buf, size_t buflen)
{
  char *ptr;

  dtrace("%i, %p, %zu", errnum, buf, buflen);

  ptr = strerror(errnum);
  strncpy(buf, ptr, buflen);

  dreturn("%i", 0);
  return 0;
}
#endif

/* A getdelim() for platforms lacking it.  getdelim was originally a GNU
 * extension and has subsequently been POSIXised in POSIX.1-2008.
 */
#ifndef HAVE_GETDELIM
#define GD_SSIZE_T_MAX ((ssize_t)((size_t)-1>>1))
ssize_t getdelim(char **lineptr, size_t *n, int delim, FILE *stream)
{
  size_t nread;
  ssize_t count = 0;
  char *p, *q;
  size_t len, new_len;
  off64_t pos;

  dtrace("%p, %p, '\\x%02x', %p", lineptr, n, (char)delim, stream);

  /* create a new buffer, if necessary */
  if (*lineptr == NULL || *n == 0) {
    *lineptr = (char *)malloc(*n = 100);
    if (*lineptr == NULL) {
      dreturn("%i)", -1);
      return -1;
    }
  }
  p = *lineptr;
  len = *n;

  /* apparently getdelim returns -1 if encountering EOF at the start of
   * a read, so try reading some text before beginning the main loop */
  pos = ftello64(stream);
  nread = fread(p, 1, len, stream);

  if (nread == 0) {
    /* this is an error or EOF with no data read */
    dreturn("%i)", -1);
    return -1;
  }

  for (;;) {
    /* look for delim */
    q = (char *)memchr(p, delim, nread);
    if (q) {
#ifdef __MSVCRT__
      int r;
      off64_t new_pos;
#endif

      /* found delim */
      count += (q - p) + 1;

      /* make sure we have room for a terminating NUL */
      new_len = count;
      /* rewind */
      pos += (q - p);
#ifndef __MSVCRT__
      pos++;
#endif
      fseeko64(stream, pos, SEEK_SET);
#ifdef __MSVCRT__
      /* Even when we open a text file in binary mode, fseek/ftell seem able
       * to screw up.  So, do things the hard way. */
      r = fgetc(stream);
      new_pos = ftello64(stream);
      while (r != EOF && (new_pos <= pos || r != '\n')) {
        r = fgetc(stream);
        new_pos = ftello64(stream);
      }
#endif
    } else {
      /* no delim, increase the buffer size */
      count += nread;
      p += nread;

      if (count == GD_SSIZE_T_MAX) {
        /* out of ssize_t room */
        errno = EOVERFLOW;
        dreturn("%i", -1);
        return -1;
      } else if (count >= GD_SSIZE_T_MAX / 2)
        new_len = GD_SSIZE_T_MAX;
      else
        new_len = count * 2;
      len = new_len - count;
    }

    /* realloc, if necessary */
    if (*n < new_len) {
      char *ptr = (char *)realloc(*lineptr, new_len);
      if (!ptr) {
        dreturn("%i", -1);
        return -1;
      }
      *n = new_len;
      p = ptr + (p - *lineptr);
      *lineptr = ptr;
    }

    /* quit if there's no need to read more */
    if (q)
      break;

    if (feof(stream)) {
      q = p - 1;
      break;
    }

    /* read some more */
    pos = ftello64(stream);
    nread = fread(p, 1, len, stream);
  }
  *(q + 1) = '\0';

  dreturn("%li %li (\"%s\")", (long int)count, (long int)*n, *lineptr);
  return count;
}
#endif
